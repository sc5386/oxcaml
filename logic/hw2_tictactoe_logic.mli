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
end

module Move : sig
  type t =
    | Play of Card.t list
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
    [@@deriving sexp, compare, equal]
  end

  val create
    :  north:Card.t list
    -> east:Card.t list
    -> south:Card.t list
    -> west:Card.t list
    -> t

  val current_player : t -> Player.t
  val hand_for : t -> Player.t -> Card.t list
  val last_play : t -> Card.t list option
  val decision : t -> Decision.t
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
end
