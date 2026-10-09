module Clock = Unix
open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search

let () =
  let argv = Sys.get_argv () in
  let games = if Array.length argv > 1 then Int.of_string argv.(1) else 1000 in
  if games <= 0 then invalid_arg "games must be positive";
  let better_wins = ref 0 and random_wins = ref 0 and moves = ref 0 and maximum = ref 0. and rounds = ref 0 in
  let start = Clock.gettimeofday () in
  for seed = 0 to games - 1 do
    let better_north = seed % 2 = 0 in
    let is_better p = Bool.equal (Player.same_team p North) better_north in
    let random = Random.State.make [|seed; 2026|] in
    let rec play match_state turns =
      if turns > 40000 || Game_match.round_number match_state > 200 then failwith "Match did not terminate within its safety bound";
      match Game_match.phase match_state with
      | Match_over winner ->
        rounds := !rounds + Game_match.round_number match_state;
        if Bool.equal (Game_match.Team.equal winner North_south) better_north then incr better_wins else incr random_wins
      | Between_rounds -> play (Game_match.start_next_round match_state) turns
      | Returning_tribute ->
        let card = Game_match.recommended_return match_state |> Option.value_exn in
        play (Game_match.return_card match_state card |> Result.ok_or_failwith) turns
      | Playing ->
        let state = Game_match.round_state match_state in
        let before = Clock.gettimeofday () in
        let better = is_better (Game_state.current_player state) in
        let move = (if better then better_move state else random_move ~random state) |> Option.value_exn in
        let elapsed = Clock.gettimeofday () -. before in
        if better then (maximum := Float.max !maximum elapsed;
          if Float.(elapsed > 2.) then failwith "Opponent exceeded two seconds");
        incr moves;
        match Game_match.make_move match_state move with
        | Error e -> raise_s [%sexp (e : Game_state.Move_error.t)]
        | Ok next -> play next (turns + 1)
    in
    play (Game_match.new_match ~seed ()) 0;
    if (seed + 1) % 100 = 0 then eprintf "Completed %d/%d full matches\n%!" (seed + 1) games
  done;
  printf "Guandan tournament: %d seeded COMPLETE MATCHES\nTeams alternate seats; initial opener is randomized.\nEach match starts at level Two and ends at an Ace match victory.\nIncludes promotion, tribute/returns, resistance and Ace failure demotion.\nBetter opponent wins: %d (%.1f%%)\nRandom opponent wins: %d\nRounds completed: %d\nLegal moves applied: %d\nLongest better-opponent decision: %.6f seconds\nElapsed: %.2f seconds\n"
    games !better_wins (100. *. Float.of_int !better_wins /. Float.of_int games)
    !random_wins !rounds !moves !maximum (Clock.gettimeofday () -. start)
