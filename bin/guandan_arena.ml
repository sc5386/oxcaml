module Clock = Unix
open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Bot_arena
let () =
  let argv = Sys.get_argv () in
  let games = if Array.length argv > 1 then Int.of_string argv.(1) else 10 in
  let configuration = List.mapi Player.all ~f:(fun i p -> p,
    if Array.length argv > i + 2 then agent_of_key argv.(i + 2)
    else if Player.same_team p North then Minimax else Random) in
  let start = Clock.gettimeofday () in
  let rec run arena = if arena.finished then arena else
    let next = step arena in
    if next.completed <> arena.completed then eprintf "Completed %d/%d complete matches\n%!" next.completed games;
    run next in
  let final = run (create ~seed:0 ~games ~configuration ()) in
  printf "Completed %d full matches from Two to Ace. N/S wins: %d; E/W wins: %d. Rounds: %d; moves: %d.\nWall time: %.3fs; AI decision time: %.3fs\n"
    final.completed final.north_south_wins final.east_west_wins final.rounds final.steps
    (Clock.gettimeofday () -. start) final.decision_seconds;
  List.iter final.stats ~f:(fun (p, s) ->
    printf "%s / %s: moves=%d total=%.3fs mean=%.6fs max=%.6fs mean-depth=%.2f nodes=%d cutoffs=%d\n"
      (Sexp.to_string [%sexp (p : Player.t)])
      (agent_name (List.Assoc.find_exn configuration p ~equal:Player.equal)) s.moves s.total_seconds
      (s.total_seconds /. Float.of_int (Int.max 1 s.moves)) s.max_seconds
      (Float.of_int s.depth_sum /. Float.of_int (Int.max 1 s.moves)) s.nodes s.cutoffs)
