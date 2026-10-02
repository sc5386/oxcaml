open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search
open Virtual_dom
open! Bonsai.Let_syntax

type model =
  { seed : int
  ; human_moves : Move.t list
  ; selected_indices : int list
  ; message : string
  }
[@@deriving sexp, compare, equal]

module Model = struct
  type t = model [@@deriving sexp, compare, equal]
end

let new_seed () =
  Random.self_init ();
  Random.bits ()
;;

let initial_model () =
  { seed = new_seed (); human_moves = []; selected_indices = []; message = "" }
;;

let game_for_model model =
  let initial_game = new_game ~seed:model.seed () in
  List.fold model.human_moves ~init:initial_game ~f:(fun game move ->
    match Game_state.make_move game move with
    | Error _ -> game
    | Ok game -> play_computers_until ~human_player:Player.North game)
;;

let text value = Vdom.Node.text value

let player_name = function
  | Player.North -> "North"
  | East -> "East"
  | South -> "South"
  | West -> "West"
;;

let card_color_class card =
  match card.Card.suit with
  | Card.Hearts | Diamonds | Red_joker -> "playing-card red-card"
  | Clubs | Spades | Black_joker -> "playing-card black-card"
;;

let sorted_hand game player =
  Game_state.hand_for game player |> List.sort ~compare:Card.compare
;;

let selected_cards model game =
  sorted_hand game Player.North
  |> List.filter_mapi ~f:(fun index card ->
    if List.mem model.selected_indices index ~equal:Int.equal then Some card else None)
;;

let error_message = function
  | Game_state.Move_error.Game_is_over -> "The round is over."
  | Cards_not_in_hand -> "Those cards are not in your hand."
  | Empty_play -> "Select at least one card."
  | Invalid_combination -> "That is not a valid Guandan combination."
  | Must_beat_current_play -> "Play a stronger combination or pass."
  | Cannot_pass_on_lead -> "You are leading this trick; play a combination."
;;

let apply_move model game move =
  match Game_state.make_move game move with
  | Error error -> { model with message = error_message error }
  | Ok _ -> { model with human_moves = model.human_moves @ [ move ]; selected_indices = []; message = "" }
;;

let seat_node game ~player ~role =
  let current = Player.equal (Game_state.current_player game) player in
  let attrs = [ Vdom.Attr.class_ (if current then "seat active-seat" else "seat") ] in
  let hidden_cards =
    if Player.equal player Player.North
    then []
    else
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "card-stack" ]
          [ text (Int.to_string (List.length (Game_state.hand_for game player))) ]
      ]
  in
  Vdom.Node.div
    ~attrs
    ([ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "seat-role" ] [ text role ]
     ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "seat-name" ] [ text (player_name player) ]
     ]
     @ hidden_cards
     @ if current then [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "turn-label" ] [ text "TO PLAY" ] ] else [])
;;

let last_play_node game =
  match Game_state.last_play game with
  | None -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "empty-trick" ] [ text "No cards on table" ]
  | Some cards ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "table-cards" ]
      (List.map cards ~f:(fun card ->
         Vdom.Node.div
           ~attrs:[ Vdom.Attr.class_ (card_color_class card ^ " table-card") ]
           [ text (Card.to_string card) ]))
;;

let status_text game =
  match Game_state.decision game with
  | Decision.Game_over finish_order ->
    "Round complete. Finish: "
    ^ (List.map finish_order ~f:player_name |> String.concat ~sep:" / ")
  | In_progress when Player.equal (Game_state.current_player game) Player.North ->
    (match Game_state.last_play game with
     | None -> "Your lead. Select a legal combination."
     | Some _ -> "Your turn. Beat the table or pass.")
  | In_progress -> "Computer players are taking their turns."
;;

let board model set_model =
  let game = game_for_model model in
  let game_over =
    match Game_state.decision game with
    | Decision.Game_over _ -> true
    | In_progress -> false
  in
  let human_turn =
    not game_over && Player.equal (Game_state.current_player game) Player.North
  in
  let hand = sorted_hand game Player.North in
  let card_buttons =
    List.mapi hand ~f:(fun index card ->
      let selected = List.mem model.selected_indices index ~equal:Int.equal in
      let attrs =
        [ Vdom.Attr.class_
            (card_color_class card ^ if selected then " selected-card" else "")
        ]
      in
      let attrs =
        if human_turn
        then
          attrs
          @ [ Vdom.Attr.on_click (fun _ ->
               let selected_indices =
                 if selected
                 then List.filter model.selected_indices ~f:(fun item -> item <> index)
                 else index :: model.selected_indices
               in
               set_model { model with selected_indices; message = "" }) ]
        else attrs
      in
      Vdom.Node.button ~attrs [ text (Card.to_string card) ])
  in
  let can_play = human_turn && not (List.is_empty model.selected_indices) in
  let can_pass = human_turn && Option.is_some (Game_state.last_play game) in
  let play_button =
    let attrs = [ Vdom.Attr.class_ "action-button primary-action" ] in
    let attrs =
      if can_play
      then
        attrs
        @ [ Vdom.Attr.on_click (fun _ ->
               set_model (apply_move model game (Move.Play (selected_cards model game)))) ]
      else attrs @ [ Vdom.Attr.class_ "inactive-action" ]
    in
    Vdom.Node.button ~attrs [ text "Play cards" ]
  in
  let pass_button =
    let attrs = [ Vdom.Attr.class_ "action-button secondary-action" ] in
    let attrs =
      if can_pass
      then
        attrs
        @ [ Vdom.Attr.on_click (fun _ -> set_model (apply_move model game Move.Pass)) ]
      else attrs @ [ Vdom.Attr.class_ "inactive-action" ]
    in
    Vdom.Node.button ~attrs [ text "Pass" ]
  in
  let new_game_button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.class_ "new-game-button"
        ; Vdom.Attr.on_click (fun _ -> set_model (initial_model ()))
        ]
      [ text "New game" ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "app-shell" ]
    [ Vdom.Node.header
        ~attrs:[ Vdom.Attr.class_ "masthead" ]
        [ Vdom.Node.div
            [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "eyebrow" ] [ text "FOUR PLAYERS · TWO TEAMS" ]
            ; Vdom.Node.h1 [ text "Guandan" ]
            ]
        ; new_game_button
        ]
    ; Vdom.Node.main
        ~attrs:[ Vdom.Attr.class_ "game-layout" ]
        [ Vdom.Node.section
            ~attrs:[ Vdom.Attr.class_ "table-area" ]
            [ seat_node game ~player:Player.South ~role:"YOUR PARTNER"
            ; Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "middle-row" ]
                [ seat_node game ~player:Player.West ~role:"OPPONENT"
                ; Vdom.Node.section
                    ~attrs:[ Vdom.Attr.class_ "trick-area" ]
                    [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "trick-caption" ] [ text "CURRENT TRICK" ]
                    ; last_play_node game
                    ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "status-line" ] [ text (status_text game) ]
                    ]
                ; seat_node game ~player:Player.East ~role:"OPPONENT"
                ]
            ; seat_node game ~player:Player.North ~role:"YOU · NORTH"
            ]
        ; Vdom.Node.section
            ~attrs:[ Vdom.Attr.class_ "hand-area" ]
            [ Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "hand-heading" ]
                [ Vdom.Node.div
                    [ Vdom.Node.h2 [ text "Your hand" ]
                    ; Vdom.Node.span [ text (Int.to_string (List.length hand) ^ " cards") ]
                    ]
                ; Vdom.Node.div
                    ~attrs:[ Vdom.Attr.class_ "action-row" ]
                    [ play_button; pass_button ]
                ]
            ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand-cards" ] card_buttons
            ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "feedback" ] [ text model.message ]
            ]
        ]
    ]
;;

let app =
  let default_model = initial_model () in
  let%sub model, set_model = Bonsai.state ~default_model (module Model) in
  let%arr model = model
  and set_model = set_model in
  board model set_model
;;

let () = Bonsai_web.Start.start app