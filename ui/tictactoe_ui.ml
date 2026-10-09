open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

type action = Make_move of Move.t | Next_round | Return_card of Card.t
[@@deriving sexp, equal]

type 'a sexp_opaque = 'a

type model =
  { seed : int
  ; cached_game : Game_match.t sexp_opaque [@equal.ignore]
  ; human_moves : action list
  ; selected_indices : int list
  ; declaration : Combination.t option
  ; message : string
  }
[@@deriving sexp, equal]

module Model = struct
  type t = model [@@deriving sexp, equal]
end

let new_seed () =
  Random.self_init ();
  Random.bits ()
;;

let advance_computers = Game_match.play_computers_until ~human_player:Player.North
let initial_model () =
  let seed = new_seed () in
  let cached_game = Game_match.new_match ~seed () |> advance_computers in
  { seed; cached_game; human_moves = []; selected_indices = []; declaration = None; message = "" }
;;

let game_for_model model = model.cached_game
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
  Game_match.hand_for game player |> List.sort ~compare:Card.compare
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
  | Invalid_declaration -> "That declaration does not match the selected cards."
  | Cannot_pass_on_lead -> "You are leading this trick; play a combination."
;;

let apply_move model game move =
  match Game_match.make_move game move with
  | Error error -> { model with message = error_message error }
  | Ok next -> { model with cached_game = advance_computers next; human_moves = model.human_moves @ [ Make_move move ]; selected_indices = []; declaration = None; message = "" }
;;

let seat_node game ~player ~role =
  let current = Player.equal (Game_match.current_player game) player in
  let attrs = [ Vdom.Attr.class_ (if current then "seat active-seat" else "seat") ] in
  let hidden_cards =
    if Player.equal player Player.North
    then []
    else
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "card-stack" ]
          [ text (Int.to_string (List.length (Game_match.hand_for game player))) ]
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
    ^ (List.map (Game_state.finished_players game) ~f:player_name |> String.concat ~sep:" / ")
    ^ (match finish_order with
       | winner :: _ -> " — " ^ player_name winner ^ "’s team wins."
       | [] -> "")
  | In_progress when Player.equal (Game_state.current_player game) Player.North ->
    (match Game_state.last_play game with
     | None -> "Your lead. Select a legal combination."
     | Some _ -> "Your turn. Beat the table or pass.")
  | In_progress -> "Computer players are taking their turns."
;;

let board model set_model =
  let match_state = game_for_model model in
  let game = Game_match.round_state match_state in
  let game_over =
    match Game_state.decision game with
    | Decision.Game_over _ -> true
    | In_progress -> false
  in
  let human_turn =
    (Game_match.equal_phase (Game_match.phase match_state) Playing
     && not game_over && Player.equal (Game_state.current_player game) Player.North)
    || (Game_match.equal_phase (Game_match.phase match_state) Returning_tribute
        && Player.equal (Game_match.current_player match_state) Player.North)
  in
  let hand = sorted_hand match_state Player.North in
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
               set_model { model with selected_indices; declaration = None; message = "" }) ]
        else attrs
      in
      Vdom.Node.button ~attrs [ text (Card.to_string card) ])
  in
  let can_play = human_turn && not (List.is_empty model.selected_indices) in
  let returning = Game_match.equal_phase (Game_match.phase match_state) Returning_tribute in
  let can_pass = human_turn && not returning && Option.is_some (Game_state.last_play game) in
  let play_button =
    let attrs = [ Vdom.Attr.class_ "action-button primary-action" ] in
    let attrs =
      if can_play
      then
        attrs
        @ [ Vdom.Attr.on_click (fun _ ->
               let cards = selected_cards model match_state in
               if returning then match cards with
                 | [card] -> (match Game_match.return_card match_state card with
                     | Error message -> set_model {model with message}
                     | Ok next -> set_model {model with cached_game = advance_computers next; human_moves = model.human_moves @ [Return_card card]; selected_indices = []; declaration = None; message = ""})
                 | _ -> set_model {model with message = "Select exactly one card to return."}
               else set_model (apply_move model match_state
                 (match model.declaration with None -> Move.Play cards | Some d -> Move.Play_as (cards, d)))) ]
      else attrs @ [ Vdom.Attr.class_ "inactive-action" ]
    in
    Vdom.Node.button ~attrs [ text (if returning then "Return selected card" else "Play cards") ]
  in
  let pass_button =
    let attrs = [ Vdom.Attr.class_ "action-button secondary-action" ] in
    let attrs =
      if can_pass
      then
        attrs
        @ [ Vdom.Attr.on_click (fun _ -> set_model (apply_move model match_state Move.Pass)) ]
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
      [ text "New match" ]
  in
  let declarations =
    if returning || not human_turn then [] else
    let choices = Game_state.interpretations game (selected_cards model match_state) in
    if List.length choices <= 1 then [] else
      Vdom.Node.div [text "Declare this combination:"] :: List.map choices ~f:(fun declaration ->
        Vdom.Node.button ~attrs:[Vdom.Attr.class_ (if Option.equal Combination.equal model.declaration (Some declaration) then "action-button primary-action" else "action-button secondary-action");
          Vdom.Attr.on_click (fun _ -> set_model {model with declaration = Some declaration})]
          [text (Combination.to_string declaration)]) in
  let next_round_button =
    if Game_match.equal_phase (Game_match.phase match_state) Between_rounds then
      [Vdom.Node.button ~attrs:[Vdom.Attr.class_ "action-button primary-action";
          Vdom.Attr.on_click (fun _ -> set_model {model with cached_game = advance_computers (Game_match.start_next_round match_state); human_moves = model.human_moves @ [Next_round]; selected_indices = []; declaration = None; message = ""})]
        [text "Next round"]] else [] in
  let rank_name rank = match Card.Rank.natural_value rank with
    | 11 -> "J" | 12 -> "Q" | 13 -> "K" | 14 -> "A" | n -> Int.to_string n in
  let active_level = if returning then Game_match.level_for match_state (Game_match.declarer match_state |> Option.value_exn) else Game_state.level game in
  let tribute_summary =
    if Game_match.tribute_resisted match_state then "Tribute cancelled by the two red jokers."
    else Game_match.exchanges match_state |> List.map ~f:(fun e ->
      player_name e.giver ^ " gave " ^ Card.to_string e.tribute ^ " to " ^ player_name e.receiver
      ^ (match e.returned with None -> " (return pending)" | Some card -> "; returned " ^ Card.to_string card))
      |> String.concat ~sep:" · " in
  let match_status = match Game_match.phase match_state with
    | Match_over winner -> "Match complete! " ^ Game_match.Team.name winner ^ " wins at Ace."
    | Returning_tribute ->
      let exchanges = Game_match.exchanges match_state in
      let received = List.find exchanges ~f:(fun e -> Player.equal e.receiver North) in
      (match received with Some e -> "You received " ^ Card.to_string e.tribute ^ " from " ^ player_name e.giver ^ ". Select one non-wild card to return: 10 or lower, or your lowest card if all are higher."
       | None -> "Computer players are exchanging tribute.")
    | Between_rounds -> status_text game ^ " Select Next round to continue the match."
    | Playing -> status_text game in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "app-shell" ]
    [ Vdom.Node.header
        ~attrs:[ Vdom.Attr.class_ "masthead" ]
        [ Vdom.Node.div
            [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "eyebrow" ] [ text "FOUR PLAYERS · TWO TEAMS" ]
            ; Vdom.Node.h1 [ text "Guandan" ]
            ; Vdom.Node.div [ text ("Round " ^ Int.to_string (Game_match.round_number match_state) ^ " · Level " ^ rank_name active_level ^ " · Hearts of this level are wild")
            ]
            ; Vdom.Node.div [text ("Your team: " ^ rank_name (Game_match.level_for match_state North_south)
                ^ " · Opponents: " ^ rank_name (Game_match.level_for match_state East_west)) ]
            ]
        ; new_game_button
        ]
    ; Vdom.Node.main
        ~attrs:[ Vdom.Attr.class_ "game-layout" ]
        [ Vdom.Node.section
            ~attrs:[ Vdom.Attr.class_ "table-area" ]
            [ seat_node match_state ~player:Player.South ~role:"YOUR PARTNER"
            ; Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "middle-row" ]
                [ seat_node match_state ~player:Player.West ~role:"OPPONENT"
                ; Vdom.Node.section
                    ~attrs:[ Vdom.Attr.class_ "trick-area" ]
                    [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "trick-caption" ] [ text "CURRENT TRICK" ]
                    ; (if returning then Vdom.Node.div [text "TRIBUTE EXCHANGE"] else last_play_node game)
                    ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "status-line" ] [ text match_status ]
                    ; Vdom.Node.div [text tribute_summary]
                    ]
                ; seat_node match_state ~player:Player.East ~role:"OPPONENT"
                ]
            ; seat_node match_state ~player:Player.North ~role:"YOU · NORTH"
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
                    ([ play_button; pass_button ] @ next_round_button)
                ]
            ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand-cards" ] card_buttons
            ; Vdom.Node.div declarations
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