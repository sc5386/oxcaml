open! Core
open Hw2_tictactoe_logic

type player_kind = Player.t
type game_state = Game_state.t
type move = Move.t

let card rank suit : Card.t = { rank; suit }
let three_clubs = card Card.Rank.Three Card.Clubs
let three_diamonds = card Card.Rank.Three Card.Diamonds
let four_clubs = card Card.Rank.Four Card.Clubs
let five_clubs = card Card.Rank.Five Card.Clubs
let six_clubs = card Card.Rank.Six Card.Clubs
let seven_clubs = card Card.Rank.Seven Card.Clubs
let ok_exn = function Ok state -> state | Error _ -> failwith "Invalid HW1 example move"

(* Small explicit hands make these examples easy to inspect. Real deals have 27. *)
let initial_state = Game_state.create
    ~north:[ three_clubs; three_diamonds; four_clubs ]
    ~east:[ five_clubs ] ~south:[ six_clubs ] ~west:[ seven_clubs ] ()
let first_move = Move.Play [ three_clubs; three_diamonds ]
let state_after_first_move = Game_state.make_move initial_state first_move |> ok_exn
let before_terminal_state =
  Game_state.create ~north:[ three_clubs ] ~east:[ four_clubs ]
    ~south:[ five_clubs ] ~west:[ six_clubs ] ()
  |> fun s -> Game_state.make_move s (Move.Play [ three_clubs ]) |> ok_exn
  |> fun s -> Game_state.make_move s (Move.Play [ four_clubs ]) |> ok_exn
let move_to_terminal_state = Move.Play [ five_clubs ]
let terminal_state = Game_state.make_move before_terminal_state move_to_terminal_state |> ok_exn
