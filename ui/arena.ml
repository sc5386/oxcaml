open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax
module Arena = Bot_arena

type 'a sexp_opaque = 'a
type model = { configuration : (Player.t * Arena.agent) list; games : int; seed : int;
               arena : Arena.t option sexp_opaque [@equal.ignore]; running : bool; revision : int; failure : string }
[@@deriving sexp, equal]
module Model = struct type t = model [@@deriving sexp, equal] end
module Action = struct type t = Tick | Set of model [@@deriving sexp_of] end
let player_name p = Sexp.to_string [%sexp (p : Player.t)]
let text = Vdom.Node.text
let default_model = {configuration = [North, Minimax; East, Random; South, Minimax; West, Random];
                     games = 1; seed = 0; arena = None; running = false; revision = 0; failure = ""}
let next model = match model.arena with
  | None -> model
  | Some arena ->
    (try let arena = Arena.step arena in
      {model with arena = Some arena; running = model.running && not arena.finished;
        revision = model.revision + 1; failure = ""}
     with error -> {model with running = false; revision = model.revision + 1;
       failure = "Simulation paused: " ^ Exn.to_string error ^ ". Resume to retry the current action."})
let component =
  let%sub model, inject = Bonsai.state_machine0 (module Model) (module Action) ~default_model
    ~apply_action:(fun ~inject:_ ~schedule_event:_ model action -> match action with
      | Tick -> if model.running then next model else model
      | Set model -> model) in
  let set_model = Bonsai.Value.map inject ~f:(fun inject -> fun model -> inject (Action.Set model)) in
  let%sub () = Bonsai.Clock.every ~when_to_start_next_effect:`Wait_period_after_previous_effect_finishes_blocking
    (Time_ns.Span.of_ms 20.) (Bonsai.Value.map inject ~f:(fun inject -> inject Action.Tick)) in
  let%arr model = model and set_model = set_model in
  let button label callback = Vdom.Node.button ~attrs:[Vdom.Attr.class_ "action-button secondary-action";
    Vdom.Attr.on_click (fun _ -> callback ())] [text label] in
  let fields = List.map Player.all ~f:(fun player ->
    let chosen = List.Assoc.find_exn model.configuration player ~equal:Player.equal in
    Vdom.Node.label [text (player_name player ^ " agent ");
      Vdom.Node.select ~attrs:[(if model.running then Vdom.Attr.disabled else Vdom.Attr.empty);
        Vdom.Attr.on_change (fun _ key -> set_model {model with configuration = List.Assoc.add model.configuration
          ~equal:Player.equal player (Arena.agent_of_key key)})]
        (List.map Arena.agents ~f:(fun agent -> Vdom.Node.option
          ~attrs:[Vdom.Attr.value (Arena.agent_key agent); (if Arena.equal_agent chosen agent then Vdom.Attr.selected else Vdom.Attr.empty)]
          [text (Arena.agent_name agent)]))]) in
  let stats = match model.arena with None -> [text "Configure all four seats, then Start automated games."]
    | Some arena ->
      let header = Vdom.Node.tr (List.map ["Seat / agent"; "Moves"; "Total AI time"; "Mean / max time";
        "Mean completed depth"; "Nodes / cutoffs"] ~f:(fun label -> Vdom.Node.th [text label])) in
      let rows = List.map arena.stats ~f:(fun (player, s) ->
        let count = Float.of_int (Int.max 1 s.moves) in
        Vdom.Node.tr (List.map [player_name player ^ " / " ^ Arena.agent_name (List.Assoc.find_exn arena.configuration player ~equal:Player.equal);
          Int.to_string s.moves; sprintf "%.3fs" s.total_seconds;
          sprintf "%.5fs / %.5fs" (s.total_seconds /. count) s.max_seconds;
          sprintf "%.2f" (Float.of_int s.depth_sum /. count); sprintf "%d / %d" s.nodes s.cutoffs]
          ~f:(fun value -> Vdom.Node.td [text value]))) in
      [Vdom.Node.h2 [text (if arena.finished then "Simulation complete" else if model.running then "Simulation running" else "Simulation paused")];
       Vdom.Node.p [text (sprintf "Complete matches: %d/%d · N/S wins: %d · E/W wins: %d · Finished rounds: %d · Moves: %d · Total AI decision time: %.3fs · Elapsed wall time: %.3fs"
         arena.completed arena.target arena.north_south_wins arena.east_west_wins arena.rounds arena.steps arena.decision_seconds arena.wall_seconds)];
       Vdom.Node.p [text (sprintf "Current match round: %d · N/S level: %d · E/W level: %d"
         (Game_match.round_number arena.game) (Card.Rank.natural_value (Game_match.level_for arena.game North_south))
         (Card.Rank.natural_value (Game_match.level_for arena.game East_west)))];
       Vdom.Node.p [text arena.last_action]; Vdom.Node.table (header :: rows)] in
  Vdom.Node.div ~attrs:[Vdom.Attr.class_ "app-shell arena-shell"]
    ([Vdom.Node.h1 [text "Bot arena"];
      Vdom.Node.p [text "Choose each seat independently. North/South and East/West are partners. Games run automatically through level promotion, tribute and victory at Ace."];
      Vdom.Node.p [text "Random picks a legal move. Original heuristic scores one move. Minimax + alpha-beta explores future turns with iterative deepening (320 nodes, depth up to 6, 1.9-second budget). Its hidden deal is sampled, not revealed."]]
     @ [Vdom.Node.div ~attrs:[Vdom.Attr.class_ "arena-config"] fields] @ [Vdom.Node.label [text " Complete matches "; Vdom.Node.input ~attrs:[Vdom.Attr.type_ "number";
         Vdom.Attr.value (Int.to_string model.games); (if model.running then Vdom.Attr.disabled else Vdom.Attr.empty);
         Vdom.Attr.on_change (fun _ value -> match Int.of_string_opt value with
           | Some games when games >= 1 && games <= 1000 -> set_model {model with games}
           | _ -> Vdom.Effect.Ignore)] ()];
       Vdom.Node.label [text " Seed "; Vdom.Node.input ~attrs:[Vdom.Attr.type_ "number"; Vdom.Attr.value (Int.to_string model.seed);
         (if model.running then Vdom.Attr.disabled else Vdom.Attr.empty); Vdom.Attr.on_change (fun _ value -> match Int.of_string_opt value with
           | Some seed -> set_model {model with seed} | None -> Vdom.Effect.Ignore)] ()];
       button "Start automated games" (fun () -> set_model {model with running = true;
         arena = Some (Arena.create ~seed:model.seed ~games:model.games ~configuration:model.configuration ()); revision = model.revision + 1; failure = ""});
       button "Pause" (fun () -> set_model {model with running = false});
       button "Resume" (fun () -> set_model {model with running = Option.exists model.arena ~f:(fun a -> not a.finished)});
       button "Step one action" (fun () -> if model.running then Vdom.Effect.Ignore else set_model (next model));
       Vdom.Node.p [text "Per-seat timing measures bot computation, excluding the browser's display interval. Elapsed wall time includes pauses. Keep this tab open while running; Pause takes effect after the current decision. Refresh clears the simulation."]]
     @ (if String.is_empty model.failure then [] else [Vdom.Node.p [text model.failure]]) @ stats)
