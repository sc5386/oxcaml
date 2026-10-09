module Clock = Unix
open! Core
open Hw2_tictactoe_logic

let shuffle random cards =
  let cards = Array.of_list cards in
  for i = Array.length cards - 1 downto 1 do
    let j = Random.State.int random (i + 1) in
    let c = cards.(i) in cards.(i) <- cards.(j); cards.(j) <- c
  done;
  Array.to_list cards

let new_game ?seed ?(level = Card.Rank.Two) () =
  let random = match seed with
    | Some seed -> Random.State.make [| seed |]
    | None -> Random.State.make_self_init ()
  in
  let north, cards = List.split_n (shuffle random (Card.deck ())) 27 in
  let east, cards = List.split_n cards 27 in
  let south, west = List.split_n cards 27 in
  Game_state.create ~level ~north ~east ~south ~west ()

let random_move ?random state =
  let moves = Game_state.legal_moves state in
  match moves with
  | [] -> None
  | _ ->
    let index = match random with
      | Some random -> Random.State.int random (List.length moves)
      | None -> Random.int (List.length moves)
    in Some (List.nth_exn moves index)

(* Card counts, table plays and our own hand are the only information consulted.
   The heuristic never examines the ranks in a partner's or opponent's hand. *)
let remaining_cost ~level cards =
  let wilds, naturals = List.partition_tf cards ~f:(Card.is_wild ~level) in
  let groups = List.sort_and_group naturals ~compare:(fun a b -> Card.Rank.compare a.rank b.rank) in
  let singles = List.count groups ~f:(fun cs -> List.length cs = 1) in
  let groups_cost = List.sum (module Int) groups ~f:(fun cs ->
    let n = List.length cs in if n >= 4 then 1 else if n = 3 then 1 else if n = 2 then 1 else 2) in
  groups_cost - Int.min (List.length wilds) singles

let score state move =
  let me = Game_state.current_player state in
  let hand = Game_state.hand_for state me in
  let count player = List.length (Game_state.hand_for state player) in
  let partner_leading = Option.exists (Game_state.last_player state) ~f:(fun p ->
    Player.equal p (Player.partner me)) in
  let opponents = List.filter Player.all ~f:(fun p -> not (Player.same_team me p)) in
  let danger = List.exists opponents ~f:(fun p -> count p > 0 && count p <= 2) in
  match move with
  | Move.Pass -> if partner_leading then 1200 else if danger then -20000 else -10000
  | Play cards | Play_as (cards, _) ->
    let remaining =
      List.fold cards ~init:hand ~f:(fun hand card ->
        let removed = ref false in
        List.filter hand ~f:(fun c -> if not !removed && Card.equal c card then
          (removed := true; false) else true)) in
    if List.is_empty remaining then 100000 else
    let size = List.length cards in
    let ranks = List.map cards ~f:(fun c -> Card.strength ~level:(Game_state.level state) c.rank) in
    let strength = Option.value (List.max_elt ranks ~compare:Int.compare) ~default:0 in
    let same_rank = List.for_all cards ~f:(fun c -> Card.Rank.equal c.rank (List.hd_exn cards).rank) in
    let bomb_penalty = if size >= 4 && same_rank && not danger then 90 else 0 in
    size * 100 - remaining_cost ~level:(Game_state.level state) remaining * 80
    - strength * 3 - bomb_penalty - (if partner_leading && not danger then 2000 else 0)

(* Retained as a separately selectable baseline, not described as minimax. *)
let heuristic_move state =
  Game_state.legal_moves state
  |> List.max_elt ~compare:(fun a b -> Int.compare (score state a) (score state b))

module Search = struct
  type stats = { completed_depth : int; nodes : int; cutoffs : int; interrupted : bool }
  exception Budget_exhausted

  (* A team, rather than a seat, is the maximizing player. Consecutive turns
     belonging to the same team remain maximizing/minimizing as appropriate. *)
  let choose ~children ~maximizing ~evaluate ~terminal ~now ~time_limit
      ~max_nodes ~max_depth root =
    if max_nodes < 1 || max_depth < 1 then invalid_arg "Search limits must be positive";
    let deadline = now () +. time_limit in
    let nodes = ref 0 and cutoffs = ref 0 and completed_depth = ref 0 in
    let interrupted = ref false in
    let check () =
      if !nodes >= max_nodes || Float.(now () >= deadline) then raise Budget_exhausted;
      incr nodes in
    let rec visit state depth alpha beta =
      check ();
      if depth = 0 || terminal state then evaluate state else
      let successors = children state in
      match successors with
      | [] -> evaluate state
      | _ ->
        let maximize = maximizing state in
        let rec loop best alpha beta = function
          | [] -> best
          | (_, child) :: rest ->
            let value = visit child (depth - 1) alpha beta in
            let best = if maximize then Int.max best value else Int.min best value in
            let alpha = if maximize then Int.max alpha best else alpha in
            let beta = if maximize then beta else Int.min beta best in
            if alpha >= beta then (incr cutoffs; best) else loop best alpha beta rest
        in loop (if maximize then Int.min_value else Int.max_value) alpha beta successors
    in
    let best = ref None in
    (try
       for depth = 1 to max_depth do
         check ();
         let successors = children root in
         let maximize = maximizing root in
         let alpha = ref Int.min_value and beta = ref Int.max_value in
         let iteration = ref None and value = ref (if maximize then Int.min_value else Int.max_value) in
         List.iter successors ~f:(fun (move, child) ->
           let candidate = visit child (depth - 1) !alpha !beta in
           if Option.is_none !iteration || (if maximize then candidate > !value else candidate < !value)
           then (iteration := Some move; value := candidate);
           if maximize then alpha := Int.max !alpha !value else beta := Int.min !beta !value);
         (* Never publish a partial iteration: an interrupted deeper search
            cannot overwrite the last completely searched root decision. *)
         best := !iteration;
         completed_depth := depth
       done
     with Budget_exhausted -> interrupted := true);
    !best, { completed_depth = !completed_depth; nodes = !nodes;
             cutoffs = !cutoffs; interrupted = !interrupted }
end

let sample_unknown_hands state =
  let observer = Game_state.current_player state in
  let own = Game_state.hand_for state observer in
  let public = Game_state.played_cards state in
  let remove cards card =
    let rec loop prefix = function
      | [] -> invalid_arg "Known cards exceed the double deck"
      | head :: rest -> if Card.equal head card then List.rev_append prefix rest
        else loop (head :: prefix) rest in
    loop [] cards in
  let unseen = List.fold (own @ public) ~init:(Card.deck ()) ~f:remove in
  (* Seed only from observable information. Never inspect unknown card ranks,
     even to initialize randomness. Small fixtures sample from the full unseen
     deck too, rather than exposing their concealed initial_cards. *)
  let seed = Sexp.to_string [%sexp (observer : Player.t), (own : Card.t list),
    (public : Card.t list), (Game_state.level state : Card.Rank.t)]
    |> String.fold ~init:17 ~f:(fun hash c -> ((hash * 31) lxor Char.to_int c) land 0x3fffffff) in
  let remaining = ref (shuffle (Random.State.make [|seed|]) unseen) in
  let hands = List.map Player.all ~f:(fun player ->
    if Player.equal player observer then player, own else
    let hand, rest = List.split_n !remaining (List.length (Game_state.hand_for state player)) in
    remaining := rest; player, hand) in
  Game_state.with_sampled_hands state ~observer ~hands

let evaluate ~root_player state =
  match Game_state.decision state with
  | Game_over (winner :: order) ->
    let partner_position = List.findi order ~f:(fun _ p -> Player.equal p (Player.partner winner))
      |> Option.value_map ~default:3 ~f:(fun (i, _) -> i + 2) in
    let value = 1000000 + (4 - partner_position) * 10000 in
    if Player.same_team root_player winner then value else -value
  | Game_over [] -> 0
  | In_progress ->
    let cost player =
      let cards = Game_state.hand_for state player in
      List.length cards * 100 + remaining_cost ~level:(Game_state.level state) cards * 40 in
    let team_cost team = List.sum (module Int) Player.all ~f:(fun player ->
      if Player.same_team player team then cost player else 0) in
    let lead = Option.value_map (Game_state.last_player state) ~default:0 ~f:(fun p ->
      if Player.same_team p root_player then 25 else -25) in
    team_cost (Player.next root_player) - team_cost root_player + lead

let better_move_with_stats ?(time_limit = 1.9) ?(max_nodes = 320) ?(max_depth = 6) state =
  if Float.is_nan time_limit || Float.(time_limit < 0. || time_limit > 2.) then
    invalid_arg "time_limit must be between 0 and 2 seconds";
  let start = Clock.gettimeofday () in
  let moves = Game_state.legal_moves state in
  match moves with
  | [] -> None, {Search.completed_depth = 0; nodes = 0; cutoffs = 0; interrupted = false}
  | _ ->
    let ordered state moves = List.map moves ~f:(fun move -> score state move, move)
      |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Int.compare b a)
      |> List.map ~f:snd in
    let fallback = List.hd_exn (ordered state moves) in
    let root_player = Game_state.current_player state in
    let sampled = sample_unknown_hands state in
    (* Iterative depths revisit the same prefixes. Cache their successors by
       move path so legal generation and ordering run once per prefix. This
       changes work, not the searched tree or minimax values. *)
    let cutoff = start +. Float.max 0. (time_limit -. 0.1) in
    let check_time () = if Float.(Clock.gettimeofday () >= cutoff) then raise Search.Budget_exhausted in
    let successors = Hashtbl.Poly.create () in
    let children (state, path) = Hashtbl.find_or_add successors path ~default:(fun () ->
      check_time ();
      let moves = Game_state.legal_successors state
        |> List.map ~f:(fun (move, next) -> score state move, move, next)
        |> List.stable_sort ~compare:(fun (a, _, _) (b, _, _) -> Int.compare b a) in
      check_time ();
      List.map moves ~f:(fun (_, move, next) -> move, (next, move :: path))) in
    let terminal (state, _) = match Game_state.decision state with Game_over _ -> true | In_progress -> false in
    let remaining = Float.max 0. (time_limit -. (Clock.gettimeofday () -. start) -. 0.1) in
    let move, stats = Search.choose ~children
      ~maximizing:(fun (state, _) -> Player.same_team root_player (Game_state.current_player state))
      ~evaluate:(fun (state, _) -> evaluate ~root_player state) ~terminal ~now:Clock.gettimeofday
      ~time_limit:remaining ~max_nodes ~max_depth (sampled, []) in
    Some (Option.value move ~default:fallback), stats

let better_move ?time_limit state = fst (better_move_with_stats ?time_limit state)

let play_computers_until ~human_player state =
  (* Deterministic replay for Bonsai's model: no global RNG dependency. *)
  let rec loop state = match Game_state.decision state with
    | Decision.Game_over _ -> state
    | In_progress when Player.equal (Game_state.current_player state) human_player -> state
    | In_progress -> match better_move state with
      | None -> failwith "No legal computer move in an ongoing game"
      | Some move -> match Game_state.make_move state move with
        | Error _ -> failwith "The computer selected an illegal move"
        | Ok next -> loop next
  in loop state
