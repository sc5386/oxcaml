open! Core

module Player = struct
  module T = struct
    type t = North | East | South | West [@@deriving sexp, compare, equal]
  end
  include T
  include Comparable.Make (T)
  let all = [ North; East; South; West ]
  let next = function North -> East | East -> South | South -> West | West -> North
  let partner = function North -> South | South -> North | East -> West | West -> East
  let same_team a b = equal a b || equal (partner a) b
end

module Card = struct
  module Rank = struct
    module T = struct
      type t = Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen
             | King | Ace | Two | Small_joker | Big_joker
      [@@deriving sexp, compare, equal]
    end
    include T
    include Comparable.Make (T)
    (* Legacy display order; sequences use natural_value, level groups use strength. *)
    let value = function
      | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7 | Eight -> 8
      | Nine -> 9 | Ten -> 10 | Jack -> 11 | Queen -> 12 | King -> 13 | Ace -> 14
      | Two -> 15 | Small_joker -> 16 | Big_joker -> 17
    let natural_value = function Two -> 2 | rank -> value rank
    let ordinary = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
    let all = ordinary @ [ Small_joker; Big_joker ]
    let of_natural_value n = List.find_exn ordinary ~f:(fun r -> Int.equal (natural_value r) n)
  end
  type suit = Clubs | Diamonds | Hearts | Spades | Black_joker | Red_joker
  [@@deriving sexp, compare, equal]
  type t = { rank : Rank.t; suit : suit } [@@deriving sexp, compare, equal]
  let ordinary_suits = [ Clubs; Diamonds; Hearts; Spades ]
  let valid { rank; suit } = match rank, suit with
    | Rank.Small_joker, Black_joker | Big_joker, Red_joker -> true
    | (Small_joker | Big_joker), _ | _, (Black_joker | Red_joker) -> false
    | _ -> true
  let is_wild ~level { rank; suit } = Rank.equal rank level && equal_suit suit Hearts
  let strength ~level rank = if Rank.equal rank level then 15 else Rank.natural_value rank
  let to_string { rank; suit } =
    let r = match rank with
      | Rank.Three -> "3" | Four -> "4" | Five -> "5" | Six -> "6" | Seven -> "7"
      | Eight -> "8" | Nine -> "9" | Ten -> "10" | Jack -> "J" | Queen -> "Q"
      | King -> "K" | Ace -> "A" | Two -> "2" | Small_joker -> "SJ" | Big_joker -> "BJ"
    in
    r ^ (match suit with Clubs -> "C" | Diamonds -> "D" | Hearts -> "H" | Spades -> "S"
                       | Black_joker | Red_joker -> "")
  let deck () =
    List.concat_map Rank.ordinary ~f:(fun rank ->
      List.concat_map ordinary_suits ~f:(fun suit -> List.init 2 ~f:(fun _ -> { rank; suit })))
    @ List.init 2 ~f:(fun _ -> { rank = Rank.Small_joker; suit = Black_joker })
    @ List.init 2 ~f:(fun _ -> { rank = Rank.Big_joker; suit = Red_joker })
end

module Combination = struct
  type t = Single of int | Pair of int | Triple of int | Full_house of int | Straight of int
    | Consecutive_pairs of int | Consecutive_triples of int | Bomb of int * int
    | Straight_flush of int | Joker_bomb
  [@@deriving sexp, compare, equal]
  let rank_name = function 15 -> "level" | 16 -> "small joker" | 17 -> "big joker"
    | 14 -> "A" | 13 -> "K" | 12 -> "Q" | 11 -> "J" | n -> Int.to_string n
  let to_string = function
    | Single n -> "Single " ^ rank_name n | Pair n -> "Pair " ^ rank_name n
    | Triple n -> "Triple " ^ rank_name n | Full_house n -> "Full house, triple " ^ rank_name n
    | Straight n -> "Straight to " ^ rank_name n | Consecutive_pairs n -> "Three pairs to " ^ rank_name n
    | Consecutive_triples n -> "Two triples to " ^ rank_name n
    | Bomb (size, n) -> Int.to_string size ^ "-card bomb " ^ rank_name n
    | Straight_flush n -> "Straight flush to " ^ rank_name n | Joker_bomb -> "Four-joker bomb"
end

module Move = struct
  type t = Play of Card.t list | Play_as of Card.t list * Combination.t | Pass [@@deriving sexp, compare, equal]
end
module Decision = struct
  type t = In_progress | Game_over of Player.t list [@@deriving sexp, compare, equal]
end

module Game_state = struct
  type combination = Combination.t =
    | Single of int | Pair of int | Triple of int | Full_house of int | Straight of int
    | Consecutive_pairs of int | Consecutive_triples of int | Bomb of int * int
    | Straight_flush of int | Joker_bomb
  [@@deriving compare]
  type t =
    { hands : Card.t list Player.Map.t
    ; current_player : Player.t
    ; trick : (Player.t * combination) option
    ; consecutive_passes : int
    ; finish_order : Player.t list
    ; last_play : Card.t list option
    ; played_cards : Card.t list
    ; initial_cards : Card.t list
    ; level : Card.Rank.t
    ; decision : Decision.t
    }
  let current_player t = t.current_player
  let hand_for t player = Map.find_exn t.hands player
  let last_play t = t.last_play
  let decision t = t.decision
  let level t = t.level
  let last_player t = Option.map t.trick ~f:fst
  let played_cards t = t.played_cards
  let initial_cards t = t.initial_cards
  let finished_players t = t.finish_order
  let is_finished t player = List.mem t.finish_order player ~equal:Player.equal
  let finish_if_needed finish_order =
    let double_finish = match finish_order with
      | first :: second :: _ -> Player.same_team first second
      | _ -> false
    in
    if List.length finish_order >= 3 || double_finish then
      Decision.Game_over (finish_order @ List.filter Player.all ~f:(fun p ->
        not (List.mem finish_order p ~equal:Player.equal)))
    else Decision.In_progress
  let next_active t player =
    let rec find candidate left =
      if left = 0 then candidate else
      let candidate = Player.next candidate in
      if is_finished t candidate then find candidate (left - 1) else candidate
    in find player 4
  let create ?(level = Card.Rank.Two) ?(starting_player = Player.North) ~north ~east ~south ~west () =
    if not (List.mem Card.Rank.ordinary level ~equal:Card.Rank.equal) then
      invalid_arg "The level must be an ordinary rank (2 through A).";
    let hands = Player.Map.of_alist_exn [ North, north; East, east; South, south; West, west ] in
    let initial_cards = List.concat [ north; east; south; west ] in
    if List.exists initial_cards ~f:(fun c -> not (Card.valid c)) then
      invalid_arg "A card has an invalid rank/suit pairing.";
    if List.exists [ north; east; south; west ] ~f:(fun h -> List.length h > 27) then
      invalid_arg "A hand cannot contain more than 27 cards.";
    let grouped = List.sort_and_group initial_cards ~compare:Card.compare in
    if List.exists grouped ~f:(fun group -> List.length group > 2) then
      invalid_arg "A double deck has at most two copies of any card.";
    let finish_order = List.filter Player.all ~f:(fun p -> List.is_empty (Map.find_exn hands p)) in
    let t = { hands; current_player = starting_player; trick = None; consecutive_passes = 0;
              finish_order; last_play = None; played_cards = []; initial_cards; level;
              decision = finish_if_needed finish_order } in
    if is_finished t starting_player then { t with current_player = next_active t starting_player } else t

  let remove_one card cards =
    let rec loop prefix = function
      | [] -> None
      | head :: tail when Card.equal card head -> Some (List.rev_append prefix tail)
      | head :: tail -> loop (head :: prefix) tail
    in loop [] cards
  let remove_cards hand cards =
    List.fold cards ~init:(Some hand) ~f:(fun rest c -> Option.bind rest ~f:(remove_one c))

  (* A pattern consists of rank counts and, for a straight flush, a fixed suit.
     Wilds fill only ordinary-rank deficits and may never stand for jokers. *)
  let build_patterns level =
    let group n rank = [ rank, n ] in
    let ordinary = Card.Rank.ordinary in
    let ranked n constructor = List.map Card.Rank.all ~f:(fun r ->
      constructor (Card.strength ~level r), group n r, None) in
    let sequences length copies constructor =
      List.init (15 - length) ~f:(fun i ->
        let start = i + 1 in
        let ranks = List.init length ~f:(fun offset ->
          let n = start + offset in
          Card.Rank.of_natural_value (if n = 1 then 14 else n), copies) in
        constructor (start + length - 1), ranks, None)
    in
    ranked 1 (fun r -> Single r) @ ranked 2 (fun r -> Pair r)
    @ List.map ordinary ~f:(fun r -> Triple (Card.strength ~level r), group 3 r, None)
    @ List.concat_map ordinary ~f:(fun triple ->
      List.filter_map Card.Rank.all ~f:(fun pair ->
        if Card.Rank.equal triple pair then None else
        Some (Full_house (Card.strength ~level triple), [ triple, 3; pair, 2 ], None)))
    @ sequences 5 1 (fun r -> Straight r)
    @ sequences 3 2 (fun r -> Consecutive_pairs r)
    @ sequences 2 3 (fun r -> Consecutive_triples r)
    @ List.concat_map ordinary ~f:(fun r -> List.init 7 ~f:(fun i ->
      Bomb (i + 4, Card.strength ~level r), group (i + 4) r, None))
    @ List.concat_map Card.ordinary_suits ~f:(fun suit ->
      List.map (sequences 5 1 (fun r -> Straight_flush r)) ~f:(fun (c, rs, _) -> c, rs, Some suit))
    @ [ Joker_bomb, [ Card.Rank.Small_joker, 2; Big_joker, 2 ], None ]

  let pattern_cache = List.map Card.Rank.ordinary ~f:(fun level -> level, build_patterns level)
  let patterns level = List.Assoc.find_exn pattern_cache level ~equal:Card.Rank.equal
  let patterns_for_count level count = List.filter (patterns level) ~f:(fun (_, requirements, _) ->
    List.sum (module Int) requirements ~f:snd = count)

  let matches ~level cards (_, requirements, suit) =
    let wilds, naturals = List.partition_tf cards ~f:(Card.is_wild ~level) in
    let counts = List.fold naturals ~init:Card.Rank.Map.empty ~f:(fun m c ->
      Map.update m c.rank ~f:(fun n -> Option.value n ~default:0 + 1)) in
    let suit_ok = Option.for_all suit ~f:(fun s ->
      List.for_all naturals ~f:(fun c -> Card.equal_suit s c.suit)) in
    let natural_ok = Map.for_alli counts ~f:(fun ~key:rank ~data:count ->
      match List.Assoc.find requirements rank ~equal:Card.Rank.equal with
      | Some needed -> count <= needed | None -> false) in
    let joker_ok = List.for_all requirements ~f:(fun (rank, count) ->
      Card.Rank.natural_value rank <= 14 ||
      (Map.find counts rank |> Option.value ~default:0) = count) in
    suit_ok && natural_ok && joker_ok &&
    List.sum (module Int) requirements ~f:snd = List.length cards &&
    List.length wilds = List.sum (module Int) requirements ~f:(fun (r, n) ->
      n - Option.value (Map.find counts r) ~default:0)

  let classify ~level (cards : Card.t list) =
    let count = List.length cards in
    if count = 1 then [ Single (Card.strength ~level (List.hd_exn cards).rank) ]
    else if count = 2 && List.for_all cards ~f:(Card.is_wild ~level) then [ Pair 15 ]
    else
      List.filter_map (patterns_for_count level count) ~f:(fun ((combination, _, _) as pattern) ->
        let ordinary_straight_allowed = match combination with
          | Straight _ -> List.exists cards ~f:(Card.is_wild ~level)
            || not (List.for_all cards ~f:(fun c -> Card.equal_suit c.suit (List.hd_exn cards).suit))
          | _ -> true in
        if ordinary_straight_allowed && matches ~level cards pattern then Some combination else None)
      |> List.dedup_and_sort ~compare:compare_combination

  let interpretations t cards = classify ~level:t.level cards

  let bomb_strength = function
    | Bomb (n, r) -> Some ((if n <= 5 then n else n + 1), r)
    | Straight_flush r -> Some (6, r)
    | Joker_bomb -> Some (100, 0)
    | _ -> None
  let beats left right = match bomb_strength left, bomb_strength right with
    | Some a, Some b -> [%compare: int * int] a b > 0
    | Some _, None -> true | None, Some _ -> false
    | None, None -> match left, right with
      | Single a, Single b | Pair a, Pair b | Triple a, Triple b | Full_house a, Full_house b
      | Straight a, Straight b | Consecutive_pairs a, Consecutive_pairs b
      | Consecutive_triples a, Consecutive_triples b -> a > b
      | _ -> false

  module Move_error = struct
    type t = Game_is_over | Cards_not_in_hand | Empty_play | Invalid_combination
           | Must_beat_current_play | Cannot_pass_on_lead | Invalid_declaration
    [@@deriving sexp, compare, equal]
  end
  let make_move t move : (t, Move_error.t) Result.t = match t.decision with
    | Decision.Game_over _ -> Error Game_is_over
    | In_progress -> match move with
      | Move.Play [] | Play_as ([], _) -> Error Empty_play
      | Play cards | Play_as (cards, _) -> begin match remove_cards (hand_for t t.current_player) cards with
        | None -> Error Cards_not_in_hand
        | Some remaining ->
          let candidates = classify ~level:t.level cards in
          let declared = match move with Play_as (_, declaration) -> Some declaration | _ -> None in
          let declaration_valid = Option.for_all declared ~f:(fun d -> List.mem candidates d ~equal:Combination.equal) in
          let candidates = Option.value_map declared ~default:candidates ~f:(fun d -> if declaration_valid then [d] else []) in
          let legal = match t.trick with None -> candidates | Some (_, old) -> List.filter candidates ~f:(fun c -> beats c old) in
          begin match List.last legal with
          | None -> if not declaration_valid then Error Invalid_declaration else if List.is_empty candidates then Error Invalid_combination else Error Must_beat_current_play
          | Some combination ->
            let finish_order = if List.is_empty remaining then t.finish_order @ [ t.current_player ] else t.finish_order in
            let next_state = { t with hands = Map.set t.hands ~key:t.current_player ~data:remaining;
              trick = Some (t.current_player, combination); consecutive_passes = 0; finish_order;
              last_play = Some cards; played_cards = t.played_cards @ cards;
              decision = finish_if_needed finish_order } in
            Ok { next_state with current_player = next_active next_state t.current_player }
          end
        end
      | Pass -> match t.trick with
        | None -> Error Cannot_pass_on_lead
        | Some (leader, _) ->
          let passes = t.consecutive_passes + 1 in
          let active_count = 4 - List.length t.finish_order in
          let leader_active = not (is_finished t leader) in
          if passes >= active_count - (if leader_active then 1 else 0) then
            let leader = if leader_active then leader else Player.partner leader in
            let leader = if is_finished t leader then next_active t leader else leader in
            Ok { t with current_player = leader; trick = None; last_play = None; consecutive_passes = 0 }
          else Ok { t with current_player = next_active t t.current_player; consecutive_passes = passes }

  (* Representative move catalogue: every available rank pattern, plus every
     straight-flush suit. Equivalent choices of double-deck copies are collapsed. *)
  let candidate_plays t =
    let hand = hand_for t t.current_player in
    let wilds, naturals = List.partition_tf hand ~f:(Card.is_wild ~level:t.level) in
    let by_rank = List.fold naturals ~init:Card.Rank.Map.empty ~f:(fun map card ->
      Map.update map card.rank ~f:(fun previous -> card :: Option.value previous ~default:[])) in
    let choose (_, requirements, suit) =
      let chosen = List.map requirements ~f:(fun (rank, needed) ->
        let matching = Map.find by_rank rank |> Option.value ~default:[] |> List.rev
          |> List.filter ~f:(fun c -> Option.for_all suit ~f:(Card.equal_suit c.suit)) in
        rank, needed, List.take matching needed) in
      if List.exists chosen ~f:(fun (r, needed, cs) -> Card.Rank.natural_value r > 14 && List.length cs < needed) then None else
      let deficit = List.sum (module Int) chosen ~f:(fun (_, n, cs) -> n - List.length cs) in
      if deficit > List.length wilds then None else
      let cards = List.concat_map chosen ~f:(fun (_, _, cs) -> cs) @ List.take wilds deficit in
      if List.is_empty cards then None else Some (Move.Play cards)
    in
    List.map hand ~f:(fun c -> Move.Play [ c ]) @ List.filter_map (patterns t.level) ~f:choose
    |> List.dedup_and_sort ~compare:Move.compare
  let legal_moves t = match t.decision with
    | Game_over _ -> []
    | In_progress ->
      let moves = candidate_plays t @ if Option.is_some t.trick then [ Move.Pass ] else [] in
      List.filter moves ~f:(fun move -> Result.is_ok (make_move t move))
end
