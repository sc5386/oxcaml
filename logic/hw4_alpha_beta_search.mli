open! Core
open! Hw2_tictactoe_logic

val new_game : ?seed:int -> ?level:Card.Rank.t -> unit -> Game_state.t
val random_move : ?random:Random.State.t -> Game_state.t -> Move.t option
val heuristic_move : Game_state.t -> Move.t option

module Search : sig
  type stats = { completed_depth : int; nodes : int; cutoffs : int; interrupted : bool }
  (** Generic iterative-deepening minimax with alpha-beta pruning. Only complete
      depth iterations are committed. Injected clock permits deterministic tests. *)
  val choose : children:('state -> ('move * 'state) list)
    -> maximizing:('state -> bool) -> evaluate:('state -> int)
    -> terminal:('state -> bool) -> now:(unit -> float) -> time_limit:float
    -> max_nodes:int -> max_depth:int -> 'state -> 'move option * stats
end
(** Team minimax with alpha-beta and iterative deepening on a reproducibly
    sampled hidden deal. The evaluator is used at leaf cutoffs and for ordering,
    not as a substitute for search. Default bounds: 320 nodes, depth 6, 1.9s.
    Searches the engine's representative legal move catalogue. *)
val better_move_with_stats : ?time_limit:float -> ?max_nodes:int -> ?max_depth:int
  -> Game_state.t -> Move.t option * Search.stats
val better_move : ?time_limit:float -> Game_state.t -> Move.t option
val play_computers_until : human_player:Player.t -> Game_state.t -> Game_state.t
