open! Core
open! Hw2_tictactoe_logic

val new_game : ?seed:int -> ?level:Card.Rank.t -> unit -> Game_state.t
val random_move : ?random:Random.State.t -> Game_state.t -> Move.t option
(** Partnership-aware heuristic. Uses our cards and public hand counts only.
    Returns promptly rather than consuming the entire two-second allowance. *)
val better_move : ?time_limit:float -> Game_state.t -> Move.t option
val play_computers_until : human_player:Player.t -> Game_state.t -> Game_state.t
