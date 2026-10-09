open! Core
open Hw2_tictactoe_logic

module Team = struct
  type t = North_south | East_west [@@deriving sexp, compare, equal]
  let of_player p = if Player.same_team p North then North_south else East_west
  let other = function North_south -> East_west | East_west -> North_south
  let name = function North_south -> "North + South" | East_west -> "East + West"
end

type rules =
  { double_finish_promotion : int
  ; return_max_rank : int option
  ; ace_requires_double_finish : bool
  ; ace_loss_to_aces_demotes : bool
  } [@@deriving sexp, compare, equal]
let default_rules = { double_finish_promotion = 3; return_max_rank = Some 10;
                      ace_requires_double_finish = false; ace_loss_to_aces_demotes = false }
type phase = Playing | Between_rounds | Returning_tribute | Match_over of Team.t
[@@deriving sexp, compare, equal]
type exchange = { giver : Player.t; receiver : Player.t; tribute : Card.t; returned : Card.t option }
[@@deriving sexp, compare, equal]
type round_result = { number : int; winner : Team.t; finish_order : Player.t list; promotion : int }
[@@deriving sexp, compare, equal]
type t =
  { rules : rules; seed : int; number : int; game : Game_state.t; phase : phase
  ; ns_level : int; ew_level : int; ns_ace_failures : int; ew_ace_failures : int
  ; declarer : Team.t option; history : round_result list
  ; setup_hands : Card.t list Player.Map.t option; pending : exchange list
  ; exchanges : exchange list; opener : Player.t; first_finish_cards : Card.t list option
  ; tribute_resisted : bool }
let phase t = t.phase
let round_state t = t.game
let rules t = t.rules
let round_number t = t.number
let history t = t.history
let exchanges t = t.exchanges
let tribute_resisted t = t.tribute_resisted
let declarer t = t.declarer
let level_for t = function Team.North_south -> Card.Rank.of_natural_value t.ns_level | East_west -> Card.Rank.of_natural_value t.ew_level
let failures_for t = function Team.North_south -> t.ns_ace_failures | East_west -> t.ew_ace_failures
let hand_for t p = match t.setup_hands with Some hands -> Map.find_exn hands p | None -> Game_state.hand_for t.game p
let returning_player t = Option.map (List.hd t.pending) ~f:(fun e -> e.receiver)
let current_player t = match returning_player t with Some p -> p | None -> Game_state.current_player t.game
let validate_rules rules =
  if rules.double_finish_promotion < 1 || rules.double_finish_promotion > 4 then invalid_arg "Invalid double-finish promotion";
  Option.iter rules.return_max_rank ~f:(fun max -> if max < 2 || max > 14 then invalid_arg "Invalid return-card rank limit")
let from_state ?(rules = default_rules) ?(seed = 0) ?(north_south_level = Card.Rank.Two)
    ?(east_west_level = Card.Rank.Two) ?declarer ?(north_south_ace_failures = 0)
    ?(east_west_ace_failures = 0) game =
  validate_rules rules;
  List.iter [north_south_level; east_west_level] ~f:(fun r ->
    if not (List.mem Card.Rank.ordinary r ~equal:Card.Rank.equal) then invalid_arg "Invalid team level");
  List.iter [north_south_ace_failures; east_west_ace_failures] ~f:(fun n ->
    if n < 0 || n > 2 then invalid_arg "Ace failure count must be 0, 1 or 2");
  (match Game_state.decision game with Game_over _ -> invalid_arg "from_state requires an ongoing round" | In_progress -> ());
  { rules; seed; number = 1; game; phase = Playing; ns_level = Card.Rank.natural_value north_south_level;
    ew_level = Card.Rank.natural_value east_west_level; ns_ace_failures = north_south_ace_failures;
    ew_ace_failures = east_west_ace_failures; declarer; history = []; setup_hands = None;
    pending = []; exchanges = []; opener = Game_state.current_player game; first_finish_cards = None;
    tribute_resisted = false }
let new_match ?(rules = default_rules) ?(seed = 0) () =
  let dealt = Hw4_alpha_beta_search.new_game ~seed () in
  let random = Random.State.make [|seed; 1741|] in
  let starting_player = List.nth_exn Player.all (Random.State.int random 4) in
  let game = Game_state.create ~starting_player
    ~north:(Game_state.hand_for dealt North) ~east:(Game_state.hand_for dealt East)
    ~south:(Game_state.hand_for dealt South) ~west:(Game_state.hand_for dealt West) () in
  from_state ~rules ~seed game

let finish_round t order =
  let first = List.hd_exn order in
  let winner = Team.of_player first in
  let partner_position = List.findi_exn order ~f:(fun _ p -> Player.equal p (Player.partner first)) |> fst |> ( + ) 1 in
  let promotion = if partner_position = 2 then t.rules.double_finish_promotion else if partner_position = 3 then 2 else 1 in
  let declared_ace = Option.filter t.declarer ~f:(fun team -> Card.Rank.equal (level_for t team) Ace) in
  let won = Option.exists declared_ace ~f:(fun team -> Team.equal winner team)
    && (partner_position = 2 || (partner_position = 3 && not t.rules.ace_requires_double_finish)) in
  let history = t.history @ [{ number = t.number; winner; finish_order = order; promotion }] in
  let t = {t with history} in
  if won then { t with phase = Match_over winner } else
  let winner_level = Int.min 14 (Card.Rank.natural_value (level_for t winner) + promotion) in
  let t = match winner with
    | North_south -> {t with ns_level = winner_level} | East_west -> {t with ew_level = winner_level} in
  let t = match declared_ace with
    | None -> t
    | Some team ->
      let failures = failures_for t team + 1 in
      let aces_loss = t.rules.ace_loss_to_aces_demotes && not (Team.equal team winner)
        && Option.exists t.first_finish_cards ~f:(fun cards -> not (List.is_empty cards)
          && List.for_all cards ~f:(fun c -> Card.Rank.equal c.Card.rank Ace)) in
      let demote = failures >= 3 || aces_loss in
      match team with
      | North_south -> {t with ns_level = (if demote then 2 else t.ns_level); ns_ace_failures = (if demote then 0 else failures)}
      | East_west -> {t with ew_level = (if demote then 2 else t.ew_level); ew_ace_failures = (if demote then 0 else failures)} in
  {t with declarer = Some winner; phase = Between_rounds}

let make_move t move = match t.phase with
  | Playing -> Result.map (Game_state.make_move t.game move) ~f:(fun game ->
      let first_finish_cards = if List.is_empty (Game_state.finished_players t.game)
        && not (List.is_empty (Game_state.finished_players game)) then Game_state.last_play game
        else t.first_finish_cards in
      let t = {t with game; first_finish_cards} in
      match Game_state.decision game with In_progress -> t | Game_over order -> finish_round t order)
  | _ -> Error Game_state.Move_error.Game_is_over

let remove_one cards card =
  let removed = ref false in
  let result = List.filter cards ~f:(fun c -> if not !removed && Card.equal c card then (removed := true; false) else true) in
  if not !removed then invalid_arg "Card is not held" else result
let transfer hands ~giver ~receiver card =
  hands |> Map.set ~key:giver ~data:(remove_one (Map.find_exn hands giver) card)
  |> Map.set ~key:receiver ~data:(card :: Map.find_exn hands receiver)
let finalize_setup t hands =
  let level = level_for t (Option.value_exn t.declarer) in
  let game = Game_state.create ~level ~starting_player:t.opener ~north:(Map.find_exn hands Player.North)
    ~east:(Map.find_exn hands East) ~south:(Map.find_exn hands South) ~west:(Map.find_exn hands West) () in
  {t with game; phase = Playing; setup_hands = None; pending = []; first_finish_cards = None}
let setup_next_round t dealt =
  if not (equal_phase t.phase Between_rounds) then invalid_arg "Finish the current round first";
  let order = (List.last_exn t.history).finish_order in
  let first = List.hd_exn order in
  let double = Player.same_team first (List.nth_exn order 1) in
  let givers = if double then List.drop order 2 else [List.last_exn order] in
  let hands = Player.Map.of_alist_exn (List.map Player.all ~f:(fun p -> p, Game_state.hand_for dealt p)) in
  let level = level_for t (Option.value_exn t.declarer) in
  let resisted = List.sum (module Int) givers ~f:(fun p -> List.count (Map.find_exn hands p)
    ~f:(fun c -> Card.Rank.equal c.Card.rank Big_joker)) = 2 in
  let t = {t with number = t.number + 1; exchanges = []; tribute_resisted = resisted; first_finish_cards = None} in
  if resisted then finalize_setup {t with opener = first} hands else
  let tributes = List.map givers ~f:(fun giver ->
    let card = Map.find_exn hands giver |> List.filter ~f:(fun c -> not (Card.is_wild ~level c))
      |> List.max_elt ~compare:(fun a b -> Int.compare (Card.strength ~level a.Card.rank) (Card.strength ~level b.Card.rank))
      |> Option.value_exn in giver, card)
    |> List.stable_sort ~compare:(fun (_, a) (_, b) -> Int.compare (Card.strength ~level b.Card.rank) (Card.strength ~level a.Card.rank)) in
  let exchanges = List.mapi tributes ~f:(fun i (giver, tribute) ->
    {giver; receiver = List.nth_exn order i; tribute; returned = None}) in
  let hands = List.fold exchanges ~init:hands ~f:(fun hands e -> transfer hands ~giver:e.giver ~receiver:e.receiver e.tribute) in
  {t with setup_hands = Some hands; exchanges; pending = exchanges; opener = fst (List.hd_exn tributes); phase = Returning_tribute}
let start_next_round t =
  if not (equal_phase t.phase Between_rounds) then invalid_arg "Finish the current round first";
  let level = level_for t (Option.value_exn t.declarer) in
  let seed = t.seed + (t.number * 7919) in
  setup_next_round t (Hw4_alpha_beta_search.new_game ~seed ~level ())
let return_options t = match t.pending with
  | [] -> []
  | e :: _ ->
    let cards = hand_for t e.receiver |> List.filter ~f:(fun c -> not (Card.equal c e.tribute)
      && not (Card.is_wild ~level:(level_for t (Option.value_exn t.declarer)) c)) in
    let low = List.filter cards ~f:(fun c -> Option.for_all t.rules.return_max_rank ~f:(fun max ->
      Card.Rank.natural_value c.Card.rank <= max)) in
    (* A forced all-high hand must still be able to return a card. *)
    (if List.is_empty low then
      let lowest = List.min_elt cards ~compare:(fun a b -> Int.compare (Card.Rank.natural_value a.Card.rank) (Card.Rank.natural_value b.Card.rank)) in
      List.filter cards ~f:(fun c -> Option.exists lowest ~f:(fun minimum ->
        Card.Rank.equal c.Card.rank minimum.Card.rank))
     else low) |> List.dedup_and_sort ~compare:Card.compare
let recommended_return t = return_options t |> List.min_elt ~compare:(fun a b ->
  Int.compare (Card.Rank.natural_value a.Card.rank) (Card.Rank.natural_value b.Card.rank))
let return_card t card = match t.phase, t.pending, t.setup_hands with
  | Returning_tribute, e :: remaining, Some hands ->
    if not (List.mem (return_options t) card ~equal:Card.equal) then Error "Choose an eligible return card from your hand." else
    let hands = transfer hands ~giver:e.receiver ~receiver:e.giver card in
    let exchanges = List.map t.exchanges ~f:(fun ex -> if Player.equal ex.receiver e.receiver then {ex with returned = Some card} else ex) in
    let t = {t with pending = remaining; setup_hands = Some hands; exchanges} in
    Ok (if List.is_empty remaining then finalize_setup t hands else t)
  | _ -> Error "There is no tribute card to return."
let play_computers_until ~human_player t =
  let rec loop t = match t.phase with
    | Between_rounds | Match_over _ -> t
    | Playing when Player.equal (Game_state.current_player t.game) human_player -> t
    | Playing ->
      let move = Hw4_alpha_beta_search.better_move t.game |> Option.value_exn in
      loop (make_move t move |> Result.ok |> Option.value_exn)
    | Returning_tribute when Option.exists (returning_player t) ~f:(Player.equal human_player) -> t
    | Returning_tribute -> loop (return_card t (recommended_return t |> Option.value_exn) |> Result.ok_or_failwith)
  in loop t
