open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic

let card rank suit : Card.t = { rank; suit }

let card3c = card Card.Rank.Three Card.Clubs
let card3d = card Card.Rank.Three Card.Diamonds
let card4c = card Card.Rank.Four Card.Clubs
let card4d = card Card.Rank.Four Card.Diamonds
let card5c = card Card.Rank.Five Card.Clubs
let card5d = card Card.Rank.Five Card.Diamonds
let card6c = card Card.Rank.Six Card.Clubs

let player_name = function
  | Player.North -> "North"
  | East -> "East"
  | South -> "South"
  | West -> "West"
;;

let hand_string cards = List.map cards ~f:Card.to_string |> String.concat ~sep:" "

let ok_exn = function
  | Ok value -> value
  | Error _ -> failwith "Expected a legal move"
;;

let initial_state () =
  Game_state.create
    ~north:[ card3c; card3d; card4c ]
    ~east:[ card4d; card5d ]
    ~south:[ card5c; card6c ]
    ~west:[]
;;

let%test "a legal pair is removed from the hand and advances the turn" =
  let state = initial_state () in
  match Game_state.make_move state (Move.Play [ card3c; card3d ]) with
  | Error _ -> false
  | Ok state ->
    Player.equal (Game_state.current_player state) East
    && List.equal Card.equal (Game_state.hand_for state North) [ card4c ]
    && Option.equal (List.equal Card.equal) (Game_state.last_play state) (Some [ card3c; card3d ])
;;

let%test "a play must use cards from the current hand" =
  match Game_state.make_move (initial_state ()) (Move.Play [ card5d ]) with
  | Error Game_state.Move_error.Cards_not_in_hand -> true
  | _ -> false
;;

let%test "a play must be a recognized combination" =
  match Game_state.make_move (initial_state ()) (Move.Play [ card3c; card4c ]) with
  | Error Game_state.Move_error.Invalid_combination -> true
  | _ -> false
;;

let%test "a response must beat the current combination" =
  let state =
    Game_state.create
      ~north:[ card5c; card5d ]
      ~east:[ card3c; card3d ]
      ~south:[]
      ~west:[]
  in
  let state = Game_state.make_move state (Move.Play [ card5c; card5d ]) |> ok_exn in
  match Game_state.make_move state (Move.Play [ card3c; card3d ]) with
  | Error Game_state.Move_error.Must_beat_current_play -> true
  | _ -> false
;;

let%test "a stronger pair legally beats the current pair" =
  let state =
    Game_state.create
      ~north:[ card3c; card3d ]
      ~east:[ card4c; card4d ]
      ~south:[ card5c ]
      ~west:[ card5d ]
  in
  let state = Game_state.make_move state (Move.Play [ card3c; card3d ]) |> ok_exn in
  match Game_state.make_move state (Move.Play [ card4c; card4d ]) with
  | Error _ -> false
  | Ok state -> Player.equal (Game_state.current_player state) South
;;

let%test "a passing round returns the lead to its owner" =
  let state =
    Game_state.create
      ~north:[ card3c; card4c ]
      ~east:[ card5c ]
      ~south:[ card6c ]
      ~west:[ card5d ]
  in
  let state = Game_state.make_move state (Move.Play [ card3c ]) |> ok_exn in
  let state = Game_state.make_move state Move.Pass |> ok_exn in
  let state = Game_state.make_move state Move.Pass |> ok_exn in
  let state = Game_state.make_move state Move.Pass |> ok_exn in
  Player.equal (Game_state.current_player state) North
  && Option.is_none (Game_state.last_play state)
;;

let%test "passing is not allowed when leading a trick" =
  match Game_state.make_move (initial_state ()) Move.Pass with
  | Error Game_state.Move_error.Cannot_pass_on_lead -> true
  | _ -> false
;;

let%expect_test "a turn can be applied as an immutable UI-friendly transition" =
  let initial =
    Game_state.create
      ~north:[ card3c; card3d ]
      ~east:[ card4c; card4d ]
      ~south:[]
      ~west:[]
  in
  let after_north = Game_state.make_move initial (Move.Play [ card3c; card3d ]) |> ok_exn in
  let after_east = Game_state.make_move after_north (Move.Play [ card4c; card4d ]) |> ok_exn in
  print_endline ("After North: " ^ player_name (Game_state.current_player after_north));
  print_endline ("After East: " ^ player_name (Game_state.current_player after_east));
  print_endline ("Original North hand: " ^ hand_string (Game_state.hand_for initial North));
  print_endline ("New North hand: " ^ hand_string (Game_state.hand_for after_north North));
  print_endline
    ("Last play: "
     ^ (Game_state.last_play after_east |> Option.value_exn |> hand_string));
  [%expect
    {|
    After North: East
    After East: South
    Original North hand: 3C 3D
    New North hand:
    Last play: 4C 4D
    |}]
;;
