open! Core
open Hw2_tictactoe_logic

let ranks =
  [ Card.Rank.Three
  ; Four
  ; Five
  ; Six
  ; Seven
  ; Eight
  ; Nine
  ; Ten
  ; Jack
  ; Queen
  ; King
  ; Ace
  ; Two
  ]
;;

let suits = [ Card.Clubs; Diamonds; Hearts; Spades ]

let deck () =
  let suited_cards =
    List.concat_map suits ~f:(fun suit ->
      List.concat_map ranks ~f:(fun rank ->
        List.init 2 ~f:(fun _ -> ({ Card.rank; suit } : Card.t))))
  in
  let jokers =
    [ { Card.rank = Small_joker; suit = Black_joker }
    ; { Card.rank = Small_joker; suit = Black_joker }
    ; { Card.rank = Big_joker; suit = Red_joker }
    ; { Card.rank = Big_joker; suit = Red_joker }
    ]
  in
  suited_cards @ jokers
;;

let shuffled_deck () =
  let cards = deck () |> Array.of_list in
  for index = Array.length cards - 1 downto 1 do
    let other_index = Random.int (index + 1) in
    let card = cards.(index) in
    cards.(index) <- cards.(other_index);
    cards.(other_index) <- card
  done;
  Array.to_list cards
;;

let new_game ?seed () =
  (match seed with
   | None -> Random.self_init ()
   | Some seed -> Random.init seed);
  let cards = shuffled_deck () in
  let north, cards = List.split_n cards 27 in
  let east, cards = List.split_n cards 27 in
  let south, west = List.split_n cards 27 in
  Game_state.create ~north ~east ~south ~west
;;

let random_move state =
  match Game_state.decision state with
  | Decision.Game_over _ -> None
  | In_progress ->
    let hand = Game_state.hand_for state (Game_state.current_player state) in
    let candidate_moves =
      match Game_state.last_play state with
      | None -> List.map hand ~f:(fun card -> Move.Play [ card ])
      | Some [ last_card ] ->
        let higher_singles =
          List.filter hand ~f:(fun card ->
            Card.Rank.compare card.rank last_card.rank > 0)
          |> List.map ~f:(fun card -> Move.Play [ card ])
        in
        higher_singles @ [ Move.Pass ]
      | Some _ -> [ Move.Pass ]
    in
    let legal_moves =
      List.filter candidate_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.is_ok)
    in
    if List.is_empty legal_moves
    then None
    else Some (List.nth_exn legal_moves (Random.int (List.length legal_moves)))
;;

let play_computers_until ~human_player state =
  let rec loop state =
    match Game_state.decision state with
    | Decision.Game_over _ -> state
    | In_progress when Player.equal (Game_state.current_player state) human_player -> state
    | In_progress ->
      (match random_move state with
       | None -> state
       | Some move ->
         (match Game_state.make_move state move with
          | Error _ -> state
          | Ok next_state -> loop next_state))
  in
  loop state
;;