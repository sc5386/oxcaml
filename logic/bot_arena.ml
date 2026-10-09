module Clock = Unix
open! Core
open Hw2_tictactoe_logic
module AI = Hw4_alpha_beta_search

type agent = Random | Heuristic | Minimax [@@deriving sexp, equal]
let agent_name = function Random -> "Random" | Heuristic -> "Original heuristic" | Minimax -> "Minimax + alpha-beta"
let agents = [Random; Heuristic; Minimax]
let agent_key = function Random -> "random" | Heuristic -> "heuristic" | Minimax -> "minimax"
let agent_of_key = function "random" -> Random | "heuristic" -> Heuristic | "minimax" -> Minimax | _ -> invalid_arg "Unknown agent"

type seat_stats = { moves : int; total_seconds : float; max_seconds : float; depth_sum : int; nodes : int; cutoffs : int }
let empty_stats = {moves = 0; total_seconds = 0.; max_seconds = 0.; depth_sum = 0; nodes = 0; cutoffs = 0}
type t = { game : Game_match.t; configuration : (Player.t * agent) list; seed : int;
           target : int; completed : int; north_south_wins : int; east_west_wins : int;
           rounds : int; steps : int; stats : (Player.t * seat_stats) list;
           decision_seconds : float; started_at : float; wall_seconds : float; last_action : string; finished : bool; max_nodes : int }
let create ?(max_nodes = 320) ~seed ~games ~configuration () =
  if games < 1 || games > 1000 then invalid_arg "Games must be 1 through 1000";
  List.iter Player.all ~f:(fun p -> ignore (List.Assoc.find_exn configuration p ~equal:Player.equal : agent));
  {game = Game_match.new_match ~seed (); configuration; seed; target = games; completed = 0;
   north_south_wins = 0; east_west_wins = 0; rounds = 0; steps = 0;
   stats = List.map Player.all ~f:(fun p -> p, empty_stats); decision_seconds = 0.;
   started_at = Clock.gettimeofday (); wall_seconds = 0.;
   last_action = "Ready"; finished = false; max_nodes}

let advance t =
  if t.finished then t else begin
  if t.steps > t.target * 40000 || Game_match.round_number t.game > 200 then
    failwith "Simulation exceeded its termination bound";
  match Game_match.phase t.game with
  | Match_over winner ->
    let completed = t.completed + 1 in
    {t with completed; rounds = t.rounds + Game_match.round_number t.game;
      north_south_wins = t.north_south_wins + (if Game_match.Team.equal winner North_south then 1 else 0);
      east_west_wins = t.east_west_wins + (if Game_match.Team.equal winner East_west then 1 else 0);
      finished = completed = t.target;
      game = if completed = t.target then t.game else Game_match.new_match ~seed:(t.seed + completed) ();
      last_action = "Match " ^ Int.to_string completed ^ " won by " ^ Game_match.Team.name winner}
  | Between_rounds -> {t with game = Game_match.start_next_round t.game; last_action = "Next round dealt"}
  | Returning_tribute ->
    let card = Game_match.recommended_return t.game |> Option.value_exn in
    {t with game = Game_match.return_card t.game card |> Result.ok_or_failwith; last_action = "Returned " ^ Card.to_string card}
  | Playing ->
    let state = Game_match.round_state t.game in
    let player = Game_state.current_player state in
    let agent = List.Assoc.find_exn t.configuration player ~equal:Player.equal in
    let before = Clock.gettimeofday () in
    let move, search = match agent with
      | Minimax -> let move, stats = AI.better_move_with_stats ~max_nodes:t.max_nodes state in move, Some stats
      | Heuristic -> AI.heuristic_move state, None
      | Random -> AI.random_move ~random:(Random.State.make [|t.seed; t.completed; t.steps|]) state, None in
    let elapsed = Clock.gettimeofday () -. before in
    if Float.(elapsed > 2.) then failwith (sprintf "%s took %.3fs, exceeding the two-second decision limit" (agent_name agent) elapsed);
    let move = Option.value_exn move in
    let game = Game_match.make_move t.game move |> Result.ok |> Option.value_exn in
    let stats = List.map t.stats ~f:(fun (p, stats) -> if not (Player.equal p player) then p, stats else
      p, {moves = stats.moves + 1; total_seconds = stats.total_seconds +. elapsed;
          max_seconds = Float.max stats.max_seconds elapsed;
          depth_sum = stats.depth_sum + Option.value_map search ~default:0 ~f:(fun s -> s.completed_depth);
          nodes = stats.nodes + Option.value_map search ~default:0 ~f:(fun s -> s.nodes);
          cutoffs = stats.cutoffs + Option.value_map search ~default:0 ~f:(fun s -> s.cutoffs)}) in
    {t with game; stats; steps = t.steps + 1; decision_seconds = t.decision_seconds +. elapsed;
      last_action = Sexp.to_string [%sexp (player : Player.t)] ^ " · " ^ agent_name agent ^ " · "
        ^ (match move with Pass -> "Pass" | Play cards | Play_as (cards, _) ->
          "Played " ^ String.concat ~sep:" " (List.map cards ~f:Card.to_string))}

  end

let step t =
  if t.finished then t else
  let next = advance t in
  {next with wall_seconds = Clock.gettimeofday () -. t.started_at}
