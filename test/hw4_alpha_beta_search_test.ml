open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search

let ok_exn = function
  | Ok state -> state
  | Error _ -> failwith "Expected a legal move"
;;

let%test "a new game deals 27 cards to each seat" =
  let state = new_game ~seed:17 () in
  List.for_all Player.all ~f:(fun player -> List.length (Game_state.hand_for state player) = 27)
;;

let%test "the random opponent returns a legal move" =
  let state = new_game ~seed:42 () in
  match random_move state with
  | None -> false
  | Some move -> Game_state.make_move state move |> Result.is_ok
;;

let%test "computer turns return control to the human seat" =
  let state = new_game ~seed:7 () in
  let first_card = List.hd_exn (Game_state.hand_for state Player.North) in
  let state = Game_state.make_move state (Move.Play [ first_card ]) |> ok_exn in
  let state = play_computers_until ~human_player:Player.North state in
  match Game_state.decision state with
  | Decision.Game_over _ -> true
  | In_progress -> Player.equal (Game_state.current_player state) Player.North
;;