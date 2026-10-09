open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Bot_arena

let%test_unit "each of three agents is independently assignable to every seat" =
  List.iter agents ~f:(fun agent ->
    let configuration = List.map Player.all ~f:(fun p -> p, agent) in
    let arena = create ~seed:42 ~games:1 ~configuration () in
    let next = step arena in
    assert (next.steps = 1);
    assert (List.sum (module Int) next.stats ~f:(fun (_, s) -> s.moves) = 1);
    assert (Float.(next.decision_seconds >= 0.));
    assert (List.for_all next.stats ~f:(fun (_, s) -> Float.(s.max_seconds <= 2.))))

let%test_unit "mixed four-seat configuration accumulates correct per-seat telemetry" =
  let configuration : (Player.t * agent) list = [North, Minimax; East, Heuristic; South, Random; West, Minimax] in
  let initial = create ~seed:6 ~games:1 ~configuration () in
  let final = List.fold (List.init 24 ~f:Fn.id) ~init:initial ~f:(fun state _ -> step state) in
  assert (final.steps > 0);
  assert (List.equal (fun (p,a) (q,b) -> Player.equal p q && equal_agent a b) initial.configuration final.configuration);
  assert (List.sum (module Int) final.stats ~f:(fun (_, s) -> s.moves) = final.steps);
  assert (Float.(abs (List.sum (module Float) final.stats ~f:(fun (_, s) -> s.total_seconds) -. final.decision_seconds) < 0.000001));
  List.iter final.stats ~f:(fun (player, stats) ->
    let agent = List.Assoc.find_exn configuration player ~equal:Player.equal in
    if not (equal_agent agent Minimax) then assert (stats.nodes = 0 && stats.cutoffs = 0))

let%test_unit "fully automated arena completes two full matches and then stops" =
  let configuration = List.map Player.all ~f:(fun p -> p, Random) in
  let rec finish arena = if arena.finished then arena else finish (step arena) in
  let final = finish (create ~seed:13 ~games:2 ~configuration ()) in
  assert (final.completed = 2);
  assert (final.north_south_wins + final.east_west_wins = 2);
  assert (final.rounds >= 2);
  assert ((step final).steps = final.steps);
  match Game_match.phase final.game with Match_over _ -> () | _ -> assert false

let%test "invalid match count and missing seat are rejected" =
  let bad games configuration = try ignore (create ~seed:0 ~games ~configuration ()); false with _ -> true in
  bad 0 [Player.North, Random; East, Random; South, Random; West, Random]
  && bad 1 [Player.North, Random]
