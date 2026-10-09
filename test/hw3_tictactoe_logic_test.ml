open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Move

let c rank suit : Card.t = { rank; suit }
let r n = Card.Rank.of_natural_value n
let group n count = List.init count ~f:(fun i -> c (r n) (List.nth_exn Card.ordinary_suits (i % 4)))
let seq start length copies = List.concat_map (List.range start (start + length)) ~f:(fun n -> group (if n = 1 then 14 else n) copies)
let apply state move = match Game_state.make_move state move with Ok s -> s | Error e -> raise_s [%sexp (e : Game_state.Move_error.t)]
let fixture north east = Game_state.create ~north ~east ~south:[c (r 12) Spades] ~west:[c (r 13) Spades] ()
let reserves cards =
  List.fold cards ~init:(Card.deck ()) ~f:(fun deck card ->
    let removed = ref false in
    List.filter deck ~f:(fun c -> if not !removed && Card.equal c card then (removed := true; false) else true))
  |> fun deck -> List.take deck 4
let lead cards =
  match reserves cards with
  | [a;b;c;d] -> Game_state.create ~north:(cards @ [a]) ~east:[b] ~south:[c] ~west:[d] ()
  | _ -> assert false
let valid cards = Result.is_ok (Game_state.make_move (lead cards) (Play cards))
let error state move expected = match Game_state.make_move state move with
  | Error e -> Game_state.Move_error.equal e expected | Ok _ -> false
let response a b =
  let state = match reserves (a @ b) with
    | [w;x;y;z] -> Game_state.create ~north:(a @ [w]) ~east:(b @ [x]) ~south:[y] ~west:[z] ()
    | _ -> assert false in
  Game_state.make_move (apply state (Play a)) (Play b) |> Result.is_ok

let%test "all ordinary groups and joker pairs" =
  List.for_all [3;4;5;6;7;8;9;10;11;12;13;14;2] ~f:(fun n ->
    List.for_all [1;2;3;4;5;6;7;8] ~f:(fun count -> valid (group n count)))
  && valid [c Small_joker Black_joker; c Small_joker Black_joker]

let%test "full house, tube, plate, ace-low and high straights" =
  List.for_all [group 3 3 @ group 4 2; seq 3 3 2; seq 3 2 3; seq 1 5 1; seq 10 5 1] ~f:valid

let%test "invalid lengths, mismatched groups and joker triples" =
  List.for_all [seq 3 4 1; seq 3 6 1; seq 3 4 2; group 3 2 @ group 4 1;
                [c Small_joker Black_joker; c Small_joker Black_joker; c Big_joker Red_joker]]
    ~f:(fun cards -> not (valid cards))

let%test "level cards outrank ace, jokers outrank level" =
  response (group 14 1) (group 2 1)
  && response (group 2 1) [c Small_joker Black_joker]
  && not (response (group 2 1) (group 14 1))

let%test "wilds fill ordinary groups and sequences but not jokers" =
  let wild = c (r 2) Hearts in
  valid (group 3 2 @ [wild]) && valid (seq 3 4 1 @ [wild])
  && not (valid [c Small_joker Black_joker; wild])
  && not (valid [c Small_joker Black_joker; c Big_joker Red_joker; wild; wild])

let%test "wild substitution in straight flush and ten-card bomb" =
  valid ([3;4;5;6] |> List.map ~f:(fun n -> c (r n) Spades) |> fun cs -> cs @ [c (r 2) Hearts])
  && valid (group 3 8 @ [c (r 2) Hearts; c (r 2) Hearts])

let%test "strict comparison and combination shape" =
  response (group 3 2) (group 4 2)
  && not (response (group 4 2) (group 3 2))
  && not (response (group 3 2) (group 3 2))
  && not (response (group 3 2) (group 4 3))

let%test "bomb hierarchy: four, five, flush, six, joker" =
  let flush = List.map [3;4;5;6;7] ~f:(fun n -> c (r n) Hearts) in
  let jokers = [c Small_joker Black_joker; c Small_joker Black_joker; c Big_joker Red_joker; c Big_joker Red_joker] in
  response (group 14 4) (group 3 5) && response (group 14 5) flush
  && response flush (group 3 6) && not (response (group 3 6) flush)
  && response (group 3 8) jokers && not (response jokers (group 3 8))
  && response (group 3 2) (group 4 4)

let%test "illegal errors preserve original hand" =
  let state = fixture (group 3 2 @ group 4 1) (group 5 1) in
  error state (Play []) Empty_play && error state Pass Cannot_pass_on_lead
  && error state (Play (group 6 1)) Cards_not_in_hand
  && error state (Play [c (r 3) Clubs; c (r 3) Clubs]) Cards_not_in_hand
  && error state (Play (group 3 1 @ group 4 1)) Invalid_combination
  && List.length (Game_state.hand_for state North) = 3

let%test "three passes restore an active leader" =
  let state = fixture (group 3 1 @ group 4 1) (group 5 1) in
  let state = List.fold [Play (group 3 1); Pass; Pass; Pass] ~init:state ~f:apply in
  Player.equal (Game_state.current_player state) North && Option.is_none (Game_state.last_play state)

let%test "finished leader hands the next lead to partner" =
  let state = fixture (group 3 1) (group 5 1) in
  let state = List.fold [Play (group 3 1); Pass; Pass; Pass] ~init:state ~f:apply in
  Player.equal (Game_state.current_player state) South && Option.is_none (Game_state.last_play state)
  && List.equal Player.equal (Game_state.finished_players state) [North]

let%test "double finish ends the round and skips finished players" =
  let state = fixture (group 3 1) (group 5 1) in
  let state = List.fold [Play (group 3 1); Pass; Play [c (r 12) Spades]] ~init:state ~f:apply in
  match Game_state.decision state with
  | Game_over [North; South; East; West] -> error state Pass Game_is_over
  | _ -> false

let%test "HW1 initial, intermediate and terminal examples" =
  let open Hw1 in
  Decision.equal (Game_state.decision initial_state) In_progress
  && Player.equal (Game_state.current_player state_after_first_move) East
  && Decision.equal (Game_state.decision before_terminal_state) In_progress
  && Decision.equal (Game_state.decision (apply before_terminal_state move_to_terminal_state))
       (Game_state.decision terminal_state)
  && error terminal_state first_move Game_is_over

let%test "invalid deck fixtures are rejected" =
  let rejected north = try ignore (fixture north (group 5 1)); false with Invalid_argument _ -> true in
  rejected [c Small_joker Clubs] && rejected (List.init 3 ~f:(fun _ -> c (r 3) Clubs))
  && rejected (List.init 28 ~f:(fun _ -> c (r 3) Clubs))

let%expect_test "immutable transition exposes cards for UI" =
  let state = fixture (group 3 2 @ group 4 1) (group 5 1) in
  let after = apply state (Play (group 3 2)) in
  print_s [%sexp (Game_state.current_player after : Player.t)];
  print_endline (String.concat ~sep:" " (List.map (Game_state.hand_for state North) ~f:Card.to_string));
  print_endline (String.concat ~sep:" " (List.map (Game_state.hand_for after North) ~f:Card.to_string));
  [%expect {|
    East
    3C 3D 4C
    4C
    |}]

let%test_unit "200 random full-deck walks conserve cards, advance turns and terminate" =
  for seed = 0 to 199 do
    let random = Random.State.make [|seed; 93|] in
    let rec walk state turns =
      assert (turns <= 600);
      let held = List.concat_map Player.all ~f:(Game_state.hand_for state) in
      assert (List.equal Card.equal
        (List.sort (held @ Game_state.played_cards state) ~compare:Card.compare)
        (List.sort (Game_state.initial_cards state) ~compare:Card.compare));
      match Game_state.decision state with
      | Game_over order -> assert (List.length (List.dedup_and_sort order ~compare:Player.compare) = 4)
      | In_progress ->
        assert (not (List.is_empty (Game_state.hand_for state (Game_state.current_player state))));
        let moves = Game_state.legal_moves state in
        assert (not (List.is_empty moves));
        List.iter moves ~f:(fun m -> assert (Result.is_ok (Game_state.make_move state m)));
        let move = List.nth_exn moves (Random.State.int random (List.length moves)) in
        let next = apply state move in
        (match Game_state.decision next with Game_over _ -> () | In_progress ->
          assert (not (Player.equal (Game_state.current_player state) (Game_state.current_player next))
                  || (Option.is_some (Game_state.last_play state) && Option.is_none (Game_state.last_play next))));
        walk next (turns + 1)
    in walk (Hw4_alpha_beta_search.new_game ~seed ()) 0
  done

let%test "level affects group strength but keeps sequence position natural" =
  let state = Game_state.create ~level:Seven ~north:[c Ace Clubs; c Three Clubs]
    ~east:[c Seven Clubs; c Four Clubs] ~south:[c Five Clubs] ~west:[c Six Clubs] () in
  let state = apply state (Play [c Ace Clubs]) in
  Result.is_ok (Game_state.make_move state (Play [c Seven Clubs]))
  && (let cards = seq 5 5 1 in
      let extras = reserves cards in
      match extras with [a;b;c;d] ->
        let state = Game_state.create ~level:Seven ~north:(cards @ [a]) ~east:[b] ~south:[c] ~west:[d] () in
        Result.is_ok (Game_state.make_move state (Play cards)) | _ -> false)

let%test "sequences do not wrap through ace and two" =
  not (valid (List.map [12;13;14;2;3] ~f:(fun n -> c (r n) Diamonds)))

let%test "rejected equal response returns the precise error" =
  let state = fixture (group 3 2 @ group 4 1) (group 3 2 @ group 5 1) in
  error (apply state (Play (group 3 2))) (Play (group 3 2)) Must_beat_current_play

let%test "joker level is rejected and all-empty state is terminal" =
  let invalid = try ignore (Game_state.create ~level:Small_joker ~north:[] ~east:[] ~south:[] ~west:[] ()); false
    with Invalid_argument _ -> true in
  invalid && (match Game_state.decision (Game_state.create ~north:[] ~east:[] ~south:[] ~west:[] ()) with
    | Game_over _ -> true | In_progress -> false)

let%test "human can declare either legal interpretation of ambiguous wilds" =
  let cards = [c Eight Clubs; c Eight Diamonds; c Nine Clubs; c Nine Diamonds; c Two Hearts; c Two Hearts] in
  let state = lead cards in
  let choices = Game_state.interpretations state cards in
  List.mem choices (Combination.Consecutive_pairs 10) ~equal:Combination.equal
  && List.mem choices (Combination.Consecutive_triples 9) ~equal:Combination.equal
  && Result.is_ok (Game_state.make_move state (Play_as (cards, Consecutive_pairs 10)))
  && Result.is_ok (Game_state.make_move state (Play_as (cards, Consecutive_triples 9)))
  && error state (Play_as (cards, Bomb (6, 8))) Invalid_declaration

let%test "a natural straight flush cannot be declared as an ordinary straight" =
  let cards = List.map [3;4;5;6;7] ~f:(fun n -> c (r n) Diamonds) in
  let state = lead cards in
  error state (Play_as (cards, Straight 7)) Invalid_declaration
  && Result.is_ok (Game_state.make_move state (Play_as (cards, Straight_flush 7)))
