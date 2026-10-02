open! Core

module Player = struct
  module T = struct
    type t =
      | North
      | East
      | South
      | West
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let all = [ North; East; South; West ]

  let next = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North
  ;;
end

module Card = struct
  module Rank = struct
    module T = struct
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
    end

    include T
    include Comparable.Make (T)

    let value = function
      | Three -> 3
      | Four -> 4
      | Five -> 5
      | Six -> 6
      | Seven -> 7
      | Eight -> 8
      | Nine -> 9
      | Ten -> 10
      | Jack -> 11
      | Queen -> 12
      | King -> 13
      | Ace -> 14
      | Two -> 15
      | Small_joker -> 16
      | Big_joker -> 17
    ;;
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

  let to_string { rank; suit } =
    let rank =
      match rank with
      | Rank.Three -> "3"
      | Rank.Four -> "4"
      | Rank.Five -> "5"
      | Rank.Six -> "6"
      | Rank.Seven -> "7"
      | Rank.Eight -> "8"
      | Rank.Nine -> "9"
      | Rank.Ten -> "10"
      | Rank.Jack -> "J"
      | Rank.Queen -> "Q"
      | Rank.King -> "K"
      | Rank.Ace -> "A"
      | Rank.Two -> "2"
      | Rank.Small_joker -> "SJ"
      | Rank.Big_joker -> "BJ"
    in
    let suit =
      match suit with
      | Clubs -> "C"
      | Diamonds -> "D"
      | Hearts -> "H"
      | Spades -> "S"
      | Black_joker -> ""
      | Red_joker -> ""
    in
    rank ^ suit
  ;;
end

module Move = struct
  type t =
    | Play of Card.t list
    | Pass
  [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | In_progress
    | Game_over of Player.t list
  [@@deriving sexp, compare, equal]
end

module Game_state = struct
  type combination =
    | Single of int
    | Pair of int
    | Triple of int
    | Full_house of int
    | Straight of int
    | Consecutive_pairs of int
    | Bomb of int * int
    | Straight_flush of int
    | Joker_bomb

  type t =
    { hands : Card.t list Player.Map.t
    ; current_player : Player.t
    ; trick : (Player.t * combination) option
    ; consecutive_passes : int
    ; finish_order : Player.t list
    ; last_play : Card.t list option
    ; decision : Decision.t
    }

  let create ~north ~east ~south ~west =
    { hands = Player.Map.of_alist_exn [ North, north; East, east; South, south; West, west ]
    ; current_player = North
    ; trick = None
    ; consecutive_passes = 0
    ; finish_order = []
    ; last_play = None
    ; decision = Decision.In_progress
    }
  ;;

  let current_player t = t.current_player
  let hand_for t player = Map.find_exn t.hands player
  let last_play t = t.last_play
  let decision t = t.decision

  let rank_counts (cards : Card.t list) =
    List.fold cards ~init:Card.Rank.Map.empty ~f:(fun counts card ->
      Map.update counts card.rank ~f:(function
        | None -> 1
        | Some count -> count + 1))
  ;;

  let is_run ranks =
    let rec loop = function
      | rank_a :: (rank_b :: _ as rest) ->
        Card.Rank.value rank_b = Card.Rank.value rank_a + 1 && loop rest
      | _ -> true
    in
    loop ranks
  ;;

  let all_run_ranks ranks =
    List.for_all ranks ~f:(fun rank -> Card.Rank.value rank <= Card.Rank.value Card.Rank.Ace)
    && is_run ranks
  ;;

  let classify cards =
    let length = List.length cards in
    let counts = rank_counts cards |> Map.to_alist in
    let ranks = List.map counts ~f:fst in
    let count_values = List.map counts ~f:snd |> List.sort ~compare:Int.compare in
    let max_rank = List.map ranks ~f:Card.Rank.value |> List.max_elt ~compare:Int.compare in
    let same_suit =
      match cards with
      | [] -> false
      | first :: rest -> List.for_all rest ~f:(fun card -> Card.equal_suit card.suit first.suit)
    in
    let all_jokers =
      List.for_all cards ~f:(fun card ->
        match card.suit with
        | Card.Black_joker | Red_joker -> true
        | Clubs | Diamonds | Hearts | Spades -> false)
    in
    if length = 4 && all_jokers && List.equal Int.equal count_values [ 2; 2 ]
    then Some Joker_bomb
    else if length = 1
    then Option.map max_rank ~f:(fun rank -> Single rank)
    else if length = 2 && List.equal Int.equal count_values [ 2 ]
    then Option.map max_rank ~f:(fun rank -> Pair rank)
    else if length = 3 && List.equal Int.equal count_values [ 3 ]
    then Option.map max_rank ~f:(fun rank -> Triple rank)
    else if length = 5 && List.equal Int.equal count_values [ 2; 3 ]
    then
      List.find_map counts ~f:(fun (rank, count) ->
        if count = 3 then Some (Full_house (Card.Rank.value rank)) else None)
    else if length = 5 && List.length ranks = 5 && all_run_ranks ranks && same_suit
    then Option.map max_rank ~f:(fun rank -> Straight_flush rank)
    else if length = 5 && List.length ranks = 5 && all_run_ranks ranks
    then Option.map max_rank ~f:(fun rank -> Straight rank)
    else if length >= 6 && length mod 2 = 0 && List.for_all counts ~f:(fun (_, count) -> count = 2) && all_run_ranks ranks
    then Option.map max_rank ~f:(fun rank -> Consecutive_pairs rank)
    else if length >= 4 && List.equal Int.equal count_values [ length ]
    then Option.map max_rank ~f:(fun rank -> Bomb (length, rank))
    else None
  ;;

  let beats candidate incumbent =
    match candidate, incumbent with
    | Joker_bomb, _ -> true
    | _, Joker_bomb -> false
    | Straight_flush _, (Single _ | Pair _ | Triple _ | Full_house _ | Straight _ | Consecutive_pairs _ | Bomb _) -> true
    | Bomb _, (Single _ | Pair _ | Triple _ | Full_house _ | Straight _ | Consecutive_pairs _) -> true
    | Straight_flush left, Straight_flush right -> left > right
    | Bomb (left_count, left_rank), Bomb (right_count, right_rank) ->
      left_count > right_count || (left_count = right_count && left_rank > right_rank)
    | Single left, Single right
    | Pair left, Pair right
    | Triple left, Triple right
    | Full_house left, Full_house right
    | Straight left, Straight right
    | Consecutive_pairs left, Consecutive_pairs right -> left > right
    | _ -> false
  ;;

  let remove_one card cards =
    let rec loop prefix = function
      | [] -> None
      | head :: tail when Card.equal card head -> Some (List.rev_append prefix tail)
      | head :: tail -> loop (head :: prefix) tail
    in
    loop [] cards
  ;;

  let remove_cards hand cards =
    List.fold cards ~init:(Some hand) ~f:(fun remaining card ->
      Option.bind remaining ~f:(remove_one card))
  ;;

  let is_finished t player = List.mem t.finish_order player ~equal:Player.equal

  let next_active t player =
    let rec find candidate =
      let candidate = Player.next candidate in
      if is_finished t candidate then find candidate else candidate
    in
    find player
  ;;

  let finish_if_needed finish_order =
    if List.length finish_order = 3
    then
      let last_player = List.find_exn Player.all ~f:(fun player -> not (List.mem finish_order player ~equal:Player.equal)) in
      Decision.Game_over (finish_order @ [ last_player ])
    else Decision.In_progress
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Cards_not_in_hand
      | Empty_play
      | Invalid_combination
      | Must_beat_current_play
      | Cannot_pass_on_lead
    [@@deriving sexp, compare, equal]
  end

  let make_move t move : (t, Move_error.t) Result.t =
    match t.decision with
    | Decision.Game_over _ -> Error Game_is_over
    | Decision.In_progress ->
      let hand = hand_for t t.current_player in
      (match move with
       | Move.Play [] -> Error Empty_play
       | Move.Play cards ->
         (match remove_cards hand cards with
          | None -> Error Cards_not_in_hand
          | Some remaining ->
            (match classify cards with
             | None -> Error Invalid_combination
             | Some combination ->
               (match t.trick with
                | Some (_, incumbent) when not (beats combination incumbent) ->
                  Error Must_beat_current_play
                | _ ->
                  let hands = Map.set t.hands ~key:t.current_player ~data:remaining in
                  let finish_order =
                    if List.is_empty remaining then t.finish_order @ [ t.current_player ] else t.finish_order
                  in
                  let decision = finish_if_needed finish_order in
                  let next_state =
                    { hands
                    ; current_player = next_active { t with finish_order } t.current_player
                    ; trick = Some (t.current_player, combination)
                    ; consecutive_passes = 0
                    ; finish_order
                    ; last_play = Some cards
                    ; decision
                    }
                  in
                  Ok next_state)))
       | Move.Pass ->
         (match t.trick with
          | None -> Error Cannot_pass_on_lead
          | Some (leader, _) ->
            let consecutive_passes = t.consecutive_passes + 1 in
            let active_count = 4 - List.length t.finish_order in
            let leader_is_active = not (is_finished t leader) in
            let passes_to_reset = active_count - (if leader_is_active then 1 else 0) in
            if consecutive_passes >= passes_to_reset
            then
              let current_player = if leader_is_active then leader else next_active t leader in
              Ok { t with current_player; trick = None; consecutive_passes = 0; last_play = None }
            else
              Ok
                { t with
                  current_player = next_active t t.current_player
                ; consecutive_passes
                }))
  ;;
end
