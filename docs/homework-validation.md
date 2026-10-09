# Homework validation

Verified locally on 2026-10-09 with OCaml 4.14.2, Core v0.16.2, PPX Jane v0.16.0,
Bonsai v0.16.0 and js_of_ocaml 5.9.1.

- `dune runtest` passes 49 inline unit/expect tests. They include 200 seeded
  random full-deck round walks, 100 timed opponent decisions, explicit wild
  declarations, promotion, tribute/returns, resistance, Ace victory and demotion,
  and 40 complete matches with conservation checks through every phase.
- `dune exec bin/guandan_tournament.exe -- 1000` completed **1,000 full matches**
  from level Two to an Ace match winner, including 5,385 rounds and 446,727 legal
  moves. Teams alternate seats and the initial opening player is randomized.
- Stronger team: 1,000 match wins; random team: zero. This measures performance
  against this random baseline, not against skilled human play.
- Maximum measured stronger decision: 0.008929 seconds; complete tournament
  runtime: 247.25 seconds on this machine.
- `dune build --profile release ui/tictactoe_ui.bc.js` succeeds; the generated browser bundle is
  copied into `generated_js/`, where `index.html` loads it.
- Local browser checks confirmed card selection, legal pairs, automatic computer
  responses, passes, round completion, Next round, and persistent team promotion
  from level Two to Three in round Two and Four in round Three.
- Dependency manifest passed `opam lint`; container setup passed shell syntax and
  JSON parsing checks. A new remote Codespace and remote GitHub Actions workflow
  have not been launched during this local validation.

The complete tournament output is in `tournament-results.txt`. Seeds make outcomes
reproducible; measured timings depend on the machine and its load.
