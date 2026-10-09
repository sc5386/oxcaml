module Clock = Unix
open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search

let () =
  let argv = Sys.get_argv () in
  let games = if Array.length argv > 1 then Int.of_string argv.(1) else 1000 in
  if games <= 0 then invalid_arg "games must be positive";
  let node_budget = if Array.length argv > 2 then Int.of_string argv.(2) else 320 in
  let first_seed = if Array.length argv > 3 then Int.of_string argv.(3) else 0 in
  let depths = ref 0 and searched = ref 0 and cutoffs = ref 0 in
  let better_wins = ref 0 and random_wins = ref 0 and moves = ref 0 and maximum = ref 0. and rounds = ref 0 in
  let start = Clock.gettimeofday () in
  for seed = first_seed to first_seed + games - 1 do
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
        let move = (if better then (
          let move, stats = better_move_with_stats ~max_nodes:node_budget state in
          depths := !depths + stats.completed_depth; incr searched;
          cutoffs := !cutoffs + stats.cutoffs; move)
          else random_move ~random state) |> Option.value_exn in
        let elapsed = Clock.gettimeofday () -. before in
        if better then (maximum := Float.max !maximum elapsed;
          if Float.(elapsed > 2.) then failwith "Opponent exceeded two seconds");
        incr moves;
        match Game_match.make_move match_state move with
        | Error e -> raise_s [%sexp (e : Game_state.Move_error.t)]
        | Ok next -> play next (turns + 1)
    in
    play (Game_match.new_match ~seed ()) 0;
    if (seed - first_seed + 1) % 10 = 0 then eprintf "Completed %d/%d full matches (first seed %d)\n%!" (seed - first_seed + 1) games first_seed
  done;
  printf "Seeds: %d through %d\nSearch decisions: %d\nSum of completed depths: %d\n" first_seed (first_seed + games - 1) !searched !depths;
  printf "AI: iterative-deepening team minimax with alpha-beta pruning\nNode budget per decision: %d\nMean completed search depth: %.2f\nAlpha-beta cutoffs: %d\n"
    node_budget (Float.of_int !depths /. Float.of_int !searched) !cutoffs;
  printf "Guandan tournament: %d seeded COMPLETE MATCHES\nTeams alternate seats; initial opener is randomized.\nEach match starts at level Two and ends at an Ace match victory.\nIncludes promotion, tribute/returns, resistance and Ace failure demotion.\nBetter opponent wins: %d (%.1f%%)\nRandom opponent wins: %d\nRounds completed: %d\nLegal moves applied: %d\nLongest better-opponent decision: %.6f seconds\nElapsed: %.2f seconds\n"
    games !better_wins (100. *. Float.of_int !better_wins /. Float.of_int games)
    !random_wins !rounds !moves !maximum (Clock.gettimeofday () -. start)
