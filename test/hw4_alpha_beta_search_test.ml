module Clock = Unix
open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search

let%test "seeded deal is reproducible and contains the entire double deck" =
  let a = new_game ~seed:17 () and b = new_game ~seed:17 () in
  List.for_all Player.all ~f:(fun p -> List.length (Game_state.hand_for a p) = 27
    && List.equal Card.equal (Game_state.hand_for a p) (Game_state.hand_for b p))
  && List.equal Card.equal (List.sort (Game_state.initial_cards a) ~compare:Card.compare)
       (List.sort (Card.deck ()) ~compare:Card.compare)

let%test_unit "both opponents choose legal moves within two seconds for 100 deals" =
  for seed = 0 to 99 do
    let state = new_game ~seed () in
    let start = Clock.gettimeofday () in
    let move = better_move state |> Option.value_exn in
    assert (Float.(Clock.gettimeofday () -. start < 2.));
    assert (Result.is_ok (Game_state.make_move state move));
    assert (Result.is_ok (Game_state.make_move state (random_move state |> Option.value_exn)))
  done

let%test "stronger opponent immediately finishes an available hand" =
  let c rank suit : Card.t = {rank; suit} in
  let cards = [c Three Clubs; c Three Diamonds; c Three Hearts] in
  let state = Game_state.create ~north:cards ~east:[c Four Clubs] ~south:[c Five Clubs] ~west:[c Six Clubs] () in
  match better_move state with Some (Play cs) -> List.length cs = 3 | _ -> false

let%test "terminal state returns no move; zero budget still returns legal fallback" =
  Option.is_none (better_move Hw1.terminal_state) && Option.is_none (random_move Hw1.terminal_state)
  && Result.is_ok (Game_state.make_move (new_game ~seed:3 ())
    (better_move ~time_limit:0. (new_game ~seed:3 ()) |> Option.value_exn))

let%test "invalid time limits are rejected" =
  List.for_all [-1.; 2.1; Float.nan] ~f:(fun time_limit ->
    try ignore (better_move ~time_limit (new_game ~seed:1 ())); false with Invalid_argument _ -> true)

let%test "computer replay is deterministic and returns control to human" =
  let initial = new_game ~seed:7 () in
  let first = List.hd_exn (Game_state.hand_for initial North) in
  let state = Game_state.make_move initial (Play [first]) |> Result.ok |> Option.value_exn in
  let a = play_computers_until ~human_player:North state in
  Random.init 12345;
  let b = play_computers_until ~human_player:North state in
  List.for_all Player.all ~f:(fun p -> List.equal Card.equal (Game_state.hand_for a p) (Game_state.hand_for b p))
  && (match Game_state.decision a with Game_over _ -> true | In_progress -> Player.equal (Game_state.current_player a) North)

let%test "stronger opponent does not read opponents' hidden ranks" =
  let c rank suit : Card.t = {rank; suit} in
  let make east west = Game_state.create ~north:[c Three Clubs; c Three Diamonds; c Four Clubs]
    ~east ~south:[c Five Clubs] ~west () in
  Option.equal Move.equal
    (better_move (make [c Six Clubs] [c Seven Clubs]))
    (better_move (make [c King Clubs] [c Ace Clubs]))

let%test "original heuristic preserves partner's lead when it cannot finish" =
  let c rank suit : Card.t = {rank; suit} in
  let initial = Game_state.create ~north:[c Three Clubs; c King Clubs; c Queen Clubs]
    ~east:[c Six Clubs] ~south:[c Five Clubs; c Seven Clubs] ~west:[c Eight Clubs] () in
  let state = List.fold [Move.Play [c Three Clubs]; Pass; Play [c Five Clubs]; Pass]
    ~init:initial ~f:(fun state move -> Game_state.make_move state move |> Result.ok |> Option.value_exn) in
  Option.equal Move.equal (heuristic_move state) (Some Pass)

let%test "random opponent can choose a grouped play, not just singles" =
  let random = Random.State.make [|42|] in
  let state = Hw1.initial_state in
  List.exists (List.init 50 ~f:(fun _ -> random_move ~random state)) ~f:(function
    | Some (Play cards) -> List.length cards > 1 | _ -> false)

(* This tree deliberately makes the attractive one-ply choice lose after the
   minimizing opponent replies. It also has a provably prunable subtree. *)
let tree_children = function
  | 0 -> ["A", 1; "B", 2]
  | 1 -> ["a", 3; "b", 4]
  | 2 -> ["c", 5; "d", 6]
  | _ -> []
let tree_evaluate = function
  | 1 -> 10 | 2 -> 20 | 3 -> 3 | 4 -> 5 | 5 -> 2 | 6 -> 100 | _ -> 0
let tree_maximizing n = n = 0
let tree_terminal n = n >= 3
let run_tree ?(max_nodes = 100) ?(now = fun () -> 0.) ?(time_limit = 1.) depth =
  Search.choose ~children:tree_children ~maximizing:tree_maximizing
    ~evaluate:tree_evaluate ~terminal:tree_terminal ~now ~time_limit
    ~max_nodes ~max_depth:depth 0

let%test "depth-two minimax reverses the greedy choice and alpha-beta prunes" =
  let shallow, _ = run_tree 1 in
  let deep, stats = run_tree 2 in
  Option.equal String.equal shallow (Some "B")
  && Option.equal String.equal deep (Some "A")
  && stats.completed_depth = 2 && stats.cutoffs > 0

let%test "interrupted deepening retains the last completed decision" =
  let move, stats = run_tree ~max_nodes:5 4 in
  Option.equal String.equal move (Some "B")
  && stats.completed_depth = 1 && stats.interrupted

let%test "deadline interrupts with a legal fallback available to the caller" =
  let clock = ref 0. in
  let now () = clock := !clock +. 0.1; !clock in
  let move, stats = run_tree ~now ~time_limit:0.25 5 in
  Option.is_none move && stats.completed_depth = 0 && stats.interrupted

let%test "alpha-beta matches exhaustive minimax on 100 seeded trees" =
  List.for_all (List.init 100 ~f:Fn.id) ~f:(fun seed ->
    let random = Random.State.make [|seed|] in
    let leaves = Array.init 81 ~f:(fun _ -> Random.State.int random 200 - 100) in
    let children (depth, index) = if depth = 4 then [] else
      List.init 3 ~f:(fun branch -> branch, (depth + 1, index * 3 + branch)) in
    let maximizing (depth, _) = depth % 2 = 0 in
    let terminal (depth, _) = depth = 4 in
    let evaluate (depth, index) = if depth = 4 then leaves.(index) else 0 in
    let rec exhaustive state = if terminal state then evaluate state else
      let values = List.map (children state) ~f:(fun (_, child) -> exhaustive child) in
      List.reduce_exn values ~f:(if maximizing state then Int.max else Int.min) in
    let expected = List.max_elt (children (0, 0)) ~compare:(fun (_, a) (_, b) ->
      Int.compare (exhaustive a) (exhaustive b)) |> Option.value_exn |> fst in
    let actual, stats = Search.choose ~children ~maximizing ~terminal ~evaluate
      ~now:(fun () -> 0.) ~time_limit:1. ~max_nodes:10000 ~max_depth:4 (0, 0) in
    Option.equal Int.equal actual (Some expected) && stats.completed_depth = 4)

let%test "real Guandan search completes multiple future-turn depths" =
  let move, stats = better_move_with_stats ~max_nodes:2000 (new_game ~seed:17 ()) in
  stats.completed_depth >= 2 && stats.nodes > 0
  && Result.is_ok (Game_state.make_move (new_game ~seed:17 ()) (Option.value_exn move))

let%test "team search permits consecutive maximizing turns when a seat is skipped" =
  let move, stats = Search.choose ~children:tree_children ~maximizing:(fun _ -> true)
    ~evaluate:tree_evaluate ~terminal:tree_terminal ~now:(fun () -> 0.)
    ~time_limit:1. ~max_nodes:100 ~max_depth:2 0 in
  Option.equal String.equal move (Some "B") && stats.completed_depth = 2

let%test "minimizing root chooses the adversarial lowest value" =
  let move, _ = Search.choose ~children:tree_children ~maximizing:(fun _ -> false)
    ~evaluate:tree_evaluate ~terminal:tree_terminal ~now:(fun () -> 0.)
    ~time_limit:1. ~max_nodes:100 ~max_depth:2 0 in
  Option.equal String.equal move (Some "B")
