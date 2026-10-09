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

let%test "stronger opponent preserves partner's lead when it cannot finish" =
  let c rank suit : Card.t = {rank; suit} in
  let initial = Game_state.create ~north:[c Three Clubs; c King Clubs; c Queen Clubs]
    ~east:[c Six Clubs] ~south:[c Five Clubs; c Seven Clubs] ~west:[c Eight Clubs] () in
  let state = List.fold [Move.Play [c Three Clubs]; Pass; Play [c Five Clubs]; Pass]
    ~init:initial ~f:(fun state move -> Game_state.make_move state move |> Result.ok |> Option.value_exn) in
  Option.equal Move.equal (better_move state) (Some Pass)

let%test "random opponent can choose a grouped play, not just singles" =
  let random = Random.State.make [|42|] in
  let state = Hw1.initial_state in
  List.exists (List.init 50 ~f:(fun _ -> random_move ~random state)) ~f:(function
    | Some (Play cards) -> List.length cards > 1 | _ -> false)
