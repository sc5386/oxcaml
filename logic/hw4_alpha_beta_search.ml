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

let better_move ?(time_limit = 1.9) state =
  if Float.is_nan time_limit || Float.(time_limit < 0. || time_limit > 2.) then
    invalid_arg "time_limit must be between 0 and 2 seconds";
  let deadline = Clock.gettimeofday () +. time_limit in
  let moves = Game_state.legal_moves state in
  match moves with
  | [] -> None
  | fallback :: rest ->
    let best = ref fallback in
    let best_score = ref (score state fallback) in
    List.iter rest ~f:(fun move ->
      if Float.(Clock.gettimeofday () < deadline) then
        let value = score state move in
        if value > !best_score then (best := move; best_score := value));
    Some !best

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
