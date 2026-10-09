open! Core

module Player : sig
  type t =
    | North
    | East
    | South
    | West
  [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val all : t list
  val next : t -> t
  val partner : t -> t
  val same_team : t -> t -> bool
end

module Card : sig
  module Rank : sig
    type t =
      | Three
      | Four
      | Five
      | Six
      | Seven
      | Eight
      | Nine
      | Ten
      | Jack
      | Queen
      | King
      | Ace
      | Two
      | Small_joker
      | Big_joker
    [@@deriving sexp, compare, equal]

    include Comparable.S with type t := t

    val value : t -> int
    val natural_value : t -> int
    val ordinary : t list
    val all : t list
    val of_natural_value : int -> t
  end

  type suit =
    | Clubs
    | Diamonds
    | Hearts
    | Spades
    | Black_joker
    | Red_joker
  [@@deriving sexp, compare, equal]

  type t =
    { rank : Rank.t
    ; suit : suit
    }
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val ordinary_suits : suit list
  val valid : t -> bool
  val is_wild : level:Rank.t -> t -> bool
  val strength : level:Rank.t -> Rank.t -> int
  val deck : unit -> t list
end

module Combination : sig
  type t = Single of int | Pair of int | Triple of int | Full_house of int | Straight of int
    | Consecutive_pairs of int | Consecutive_triples of int | Bomb of int * int
    | Straight_flush of int | Joker_bomb
  [@@deriving sexp, compare, equal]
  val to_string : t -> string
end

module Move : sig
  type t =
    | Play of Card.t list
    | Play_as of Card.t list * Combination.t
    | Pass
  [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | In_progress
    | Game_over of Player.t list
  [@@deriving sexp, compare, equal]
end

module Game_state : sig
  type t

  module Move_error : sig
    type t =
      | Game_is_over
      | Cards_not_in_hand
      | Empty_play
      | Invalid_combination
      | Must_beat_current_play
      | Cannot_pass_on_lead
      | Invalid_declaration
    [@@deriving sexp, compare, equal]
  end

  val create
    :  ?level:Card.Rank.t
    -> ?starting_player:Player.t
    -> north:Card.t list
    -> east:Card.t list
    -> south:Card.t list
    -> west:Card.t list
    -> unit
    -> t

  val current_player : t -> Player.t
  val hand_for : t -> Player.t -> Card.t list
  val last_play : t -> Card.t list option
  val decision : t -> Decision.t
  val level : t -> Card.Rank.t
  val last_player : t -> Player.t option
  val played_cards : t -> Card.t list
  val initial_cards : t -> Card.t list
  val finished_players : t -> Player.t list
  (** Search-only information-set sampling. Preserves the observer's hand, public
      hand counts, trick, pass count, finish order and played cards. *)
  val with_sampled_hands : t -> observer:Player.t -> hands:(Player.t * Card.t list) list -> t

  (** One legal representative per available rank pattern; identical deck copies
      and equivalent suit choices are collapsed. Not an exhaustive subset list. *)
  val interpretations : t -> Card.t list -> Combination.t list
  val legal_moves : t -> Move.t list
  val legal_successors : t -> (Move.t * t) list
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
end
