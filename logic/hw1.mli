open! Core
open! Hw2_tictactoe_logic

type player_kind = Player.t
type game_state = Game_state.t
type move = Move.t

val initial_state : game_state
val first_move : move
val state_after_first_move : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state
