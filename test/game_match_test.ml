open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Game_match
let c rank suit : Card.t = {rank; suit}
let three = c Three Clubs and four = c Four Clubs and five = c Five Clubs and six = c Six Clubs
let fixture ?(level = Card.Rank.Two) () = Game_state.create ~level ~north:[three] ~east:[four] ~south:[five] ~west:[six] ()
let apply t move = make_move t move |> Result.ok |> Option.value_exn
let finish t moves = List.fold moves ~init:t ~f:apply
let double = [Move.Play [three]; Pass; Play [five]]
let third = [Move.Play [three]; Play [four]; Play [five]]
let fourth = [Move.Play [three]; Play [four]; Pass; Play [six]]
let completed moves = finish (from_state (fixture ())) moves
let%test "first/second, first/third and first/fourth promote by 3, 2 and 1" =
  List.for_all [double, 5; third, 4; fourth, 3] ~f:(fun (moves, expected) ->
    let t = completed moves in Card.Rank.natural_value (level_for t North_south) = expected
    && Card.Rank.equal (level_for t East_west) Two && equal_phase (phase t) Between_rounds)
let%test "promotion caps at Ace without winning immediately" =
  let t = finish (from_state ~north_south_level:King ~declarer:North_south (fixture ~level:King ())) double in
  Card.Rank.equal (level_for t North_south) Ace && equal_phase (phase t) Between_rounds
let%test "Ace declarer wins with partner second or third, not fourth" =
  List.for_all [double, true; third, true; fourth, false] ~f:(fun (moves, won) ->
    let t = finish (from_state ~north_south_level:Ace ~declarer:North_south (fixture ~level:Ace ())) moves in
    Bool.equal (equal_phase (phase t) (Match_over North_south)) won)
let%test "opponents already on Ace must become declarers before winning" =
  let t = finish (from_state ~north_south_level:Ace ~declarer:East_west (fixture ())) double in
  equal_phase (phase t) Between_rounds && Option.equal Team.equal (declarer t) (Some North_south)
let%test "third unsuccessful Ace attempt demotes to Two" =
  let t = finish (from_state ~north_south_level:Ace ~declarer:North_south ~north_south_ace_failures:2 (fixture ~level:Ace ())) fourth in
  Card.Rank.equal (level_for t North_south) Two && failures_for t North_south = 0
let%test "variant can require a double finish on Ace and four-level promotion" =
  let rules = {default_rules with double_finish_promotion = 4; ace_requires_double_finish = true} in
  let a = finish (from_state ~rules (fixture ())) double in
  let b = finish (from_state ~rules ~north_south_level:Ace ~declarer:North_south (fixture ~level:Ace ())) third in
  Card.Rank.equal (level_for a North_south) Six && equal_phase (phase b) Between_rounds
let dealt ~east ~west = Game_state.create ~north:[c Three Diamonds; c Four Diamonds; c Five Diamonds]
  ~east ~south:[c Six Diamonds; c Seven Diamonds; c Eight Diamonds] ~west ()
let%test "double tribute routes higher card to first finisher and chooses giver's opening lead" =
  let t = setup_next_round (completed double)
    (dealt ~east:[c King Spades; c Four Spades; c Six Spades] ~west:[c Ace Clubs; c Three Spades; c Seven Spades]) in
  match exchanges t with
  | [{giver = West; receiver = North; tribute; _}; {giver = East; receiver = South; _}] ->
    Card.Rank.equal tribute.rank Ace
    && List.length (hand_for t North) = 4 && List.length (hand_for t West) = 2
    && (let t = return_card t (recommended_return t |> Option.value_exn) |> Result.ok_or_failwith in
        let t = return_card t (recommended_return t |> Option.value_exn) |> Result.ok_or_failwith in
        equal_phase (phase t) Playing && Player.equal (current_player t) West
        && List.for_all Player.all ~f:(fun p -> List.length (hand_for t p) = 3))
  | _ -> false
let%test "single tribute comes from last player even when last is winner's partner" =
  let t = setup_next_round (completed fourth)
    (dealt ~east:[c King Spades] ~west:[c Ace Clubs]) in
  match exchanges t with [{giver = South; receiver = North; _}] -> true | _ -> false
let%test "two red jokers resist double tribute, including one per loser" =
  let t = setup_next_round (completed double)
    (dealt ~east:[c Big_joker Red_joker; c Four Spades] ~west:[c Big_joker Red_joker; c Six Spades]) in
  tribute_resisted t && List.is_empty (exchanges t) && equal_phase (phase t) Playing
  && Player.equal (current_player t) North
let%test "single loser needs both red jokers to resist tribute" =
  let make west = setup_next_round (completed third) (dealt ~east:[c King Spades] ~west) in
  tribute_resisted (make [c Big_joker Red_joker; c Big_joker Red_joker; c Six Spades])
  && not (tribute_resisted (make [c Big_joker Red_joker; c Six Spades]))
let%test "tribute excludes wild cards, returns exclude donated card and high cards" =
  let t = setup_next_round (completed double)
    (dealt ~east:[c Five Hearts; c Ace Spades; c Four Spades] ~west:[c Queen Spades; c Three Spades]) in
  let first = List.hd_exn (exchanges t) in
  Card.Rank.equal first.tribute.rank Ace
  && Result.is_error (return_card t first.tribute)
  && List.for_all (return_options t) ~f:(fun c -> Card.Rank.natural_value c.Card.rank <= 10)
let%test "cannot play during tribute, advance an ongoing round or return outside tribute" =
  let initial = new_match ~seed:3 () in
  let reject_next = try ignore (start_next_round initial); false with _ -> true in
  let tribute = start_next_round (completed double) in
  reject_next && Result.is_error (return_card initial three)
  && Result.is_error (make_move tribute (Move.Play [three]))
let%test_unit "40 complete matches reach a winner with legal moves and conserved decks" =
  for seed = 0 to 39 do
    let random = Random.State.make [|seed; 441|] in
    let rec loop t moves =
      assert (round_number t <= 200 && moves <= 40000);
      (match phase t with
      | Match_over _ -> assert (not (List.is_empty (history t)))
      | Between_rounds -> loop (start_next_round t) moves
      | Returning_tribute ->
        let next = return_card t (recommended_return t |> Option.value_exn) |> Result.ok_or_failwith in
        let held = List.concat_map Player.all ~f:(hand_for next) in
        assert (List.equal Card.equal (List.sort held ~compare:Card.compare) (List.sort (Card.deck ()) ~compare:Card.compare));
        loop next moves
      | Playing ->
        let game = round_state t in
        let cards = Game_state.played_cards game @ List.concat_map Player.all ~f:(hand_for t) in
        assert (List.equal Card.equal (List.sort cards ~compare:Card.compare) (List.sort (Card.deck ()) ~compare:Card.compare));
        let move = if Player.same_team (current_player t) North then Hw4_alpha_beta_search.better_move game
          else Hw4_alpha_beta_search.random_move ~random game in
        loop (apply t (Option.value_exn move)) (moves + 1))
    in loop (new_match ~seed ()) 0
  done

let%test "losing Ace declarers accumulate failure while the other team promotes" =
  let game = Game_state.create ~level:Ace ~north:[c Five Clubs] ~east:[c Three Clubs]
    ~south:[c Six Clubs] ~west:[c Four Clubs] ~starting_player:East () in
  let t = finish (from_state ~north_south_level:Ace ~declarer:North_south game)
    [Move.Play [c Three Clubs]; Pass; Play [c Four Clubs]] in
  equal_phase (phase t) Between_rounds && failures_for t North_south = 1
  && Card.Rank.equal (level_for t East_west) Five

let%test "optional Ace-loss-to-aces demotion rule is applied" =
  let game = Game_state.create ~level:Ace ~north:[c Three Clubs] ~east:[c Ace Clubs]
    ~south:[c Four Clubs] ~west:[c Five Clubs] ~starting_player:East () in
  let rules = {default_rules with ace_loss_to_aces_demotes = true} in
  let t = finish (from_state ~rules ~north_south_level:Ace ~declarer:North_south game)
    [Move.Play [c Ace Clubs]; Pass; Pass; Pass; Play [c Five Clubs]] in
  Card.Rank.equal (level_for t North_south) Two && failures_for t North_south = 0

let%test "tribute and returns preserve the full seeded deck and are replayable" =
  let completed = completed double in
  let a = start_next_round completed and b = start_next_round completed in
  List.equal equal_exchange (exchanges a) (exchanges b)
  && List.for_all Player.all ~f:(fun p -> List.equal Card.equal (hand_for a p) (hand_for b p))

let%test "match terminal rejects further moves and redeals" =
  let t = finish (from_state ~north_south_level:Ace ~declarer:North_south (fixture ~level:Ace ())) double in
  Result.is_error (make_move t Move.Pass)
  && (try ignore (start_next_round t); false with Invalid_argument _ -> true)

let%test "an all-high hand can return only its lowest non-wild rank" =
  let deal = Game_state.create ~north:[c Jack Clubs; c Queen Clubs; c King Clubs; c Ace Clubs]
    ~east:[c Big_joker Red_joker; c Small_joker Black_joker]
    ~south:[c Jack Diamonds; c Queen Diamonds; c King Diamonds; c Ace Diamonds]
    ~west:[c Ace Spades; c King Spades] () in
  let t = setup_next_round (completed double) deal in
  List.equal Card.equal (return_options t) [c Jack Clubs]
  && Result.is_error (return_card t (c Queen Clubs))
  && Result.is_ok (return_card t (c Jack Clubs))
