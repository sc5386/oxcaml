open! Core
open Hw2_tictactoe_logic
module Team : sig
  type t = North_south | East_west [@@deriving sexp, compare, equal]
  val of_player : Player.t -> t
  val other : t -> t
  val name : t -> string
end
type rules = { double_finish_promotion : int; return_max_rank : int option;
               ace_requires_double_finish : bool; ace_loss_to_aces_demotes : bool }
[@@deriving sexp, compare, equal]
val default_rules : rules
type phase = Playing | Between_rounds | Returning_tribute | Match_over of Team.t
[@@deriving sexp, compare, equal]
type exchange = { giver : Player.t; receiver : Player.t; tribute : Card.t; returned : Card.t option }
[@@deriving sexp, compare, equal]
type round_result = { number : int; winner : Team.t; finish_order : Player.t list; promotion : int }
[@@deriving sexp, compare, equal]
type t
val new_match : ?rules:rules -> ?seed:int -> unit -> t
val from_state : ?rules:rules -> ?seed:int -> ?north_south_level:Card.Rank.t -> ?east_west_level:Card.Rank.t -> ?declarer:Team.t -> ?north_south_ace_failures:int -> ?east_west_ace_failures:int -> Game_state.t -> t
val phase : t -> phase
val round_state : t -> Game_state.t
val rules : t -> rules
val round_number : t -> int
val history : t -> round_result list
val exchanges : t -> exchange list
val tribute_resisted : t -> bool
val declarer : t -> Team.t option
val level_for : t -> Team.t -> Card.Rank.t
val failures_for : t -> Team.t -> int
val hand_for : t -> Player.t -> Card.t list
val current_player : t -> Player.t
val returning_player : t -> Player.t option
val make_move : t -> Move.t -> (t, Game_state.Move_error.t) Result.t
val start_next_round : t -> t
(** Start the next round using an explicit deal, for deterministic fixtures. *)
val setup_next_round : t -> Game_state.t -> t
val return_options : t -> Card.t list
val recommended_return : t -> Card.t option
val return_card : t -> Card.t -> (t, string) Result.t
val play_computers_until : human_player:Player.t -> t -> t
