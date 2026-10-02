open! Core
open! Hw2_tictactoe_logic

val new_game : ?seed:int -> unit -> Game_state.t
val random_move : Game_state.t -> Move.t option
val play_computers_until : human_player:Player.t -> Game_state.t -> Game_state.t
