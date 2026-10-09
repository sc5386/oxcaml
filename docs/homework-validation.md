# Homework validation

Verified locally on 2026-10-09 using OCaml 4.14.2, Core/PPX Jane/Bonsai v0.16 and js_of_ocaml 5.9.1.

- All **60 inline unit/expect tests pass**. They include 200 random full-deck walks,
  100 timed search decisions, 40 complete matches with conservation checks, rules
  and match progression tests, and mixed-agent arena tests.
- Actual search tests distinguish depth-one greedy evaluation from depth-two
  adversarial minimax, compare alpha-beta with exhaustive minimax on 100 generated
  trees, observe pruning, test minimizing and consecutive maximizing turns, and
  verify interrupted iterative deepening retains its last completed decision.
- **1,000 complete search-versus-random matches** finished from level Two to Ace
  using seeds 0–999 in four disjoint ranges. Search won 1000; random won 0.
  The run completed 7,767 rounds and 811,758 legal moves.
- 390,554 search decisions averaged **5.1824 completed depths**;
  alpha-beta pruned 21,100,620 branches. Maximum measured search time was
  **0.130341 seconds**. Longest concurrent shard took 363.51s;
  the sum of the four concurrent runtimes was 1433.00s.
- Both native tools and the release browser bundle build. The checked-in JS was
  rebuilt from the source; it includes the Bot arena tab.
- Browser verification exercised independent seat selection, all three agent
  types, Start, Pause, single-step, Resume, automatic tribute/new rounds, and a
  six-round complete match ending at Ace. That browser run averaged over five
  completed search depths; its longest search decision was 1.877s.
- Bot arena exposes wins, moves, completed rounds, total AI time, elapsed wall time,
  per-seat total/mean/max time, search depth, nodes and cutoffs. Native custom-seat
  runner verification is recorded in the local mixed-agent report.
- CI runs tests, the 1,000-match comparison and browser build; pipeline failures
  are propagated. A remote CI run and public-site deployment have not been verified.

Search uses one sampled possible hidden deal based on the observer's hand, public
plays and hand counts. It does not consult actual hidden ranks. It searches the
representative move catalogue, not every physical subset. Default node/depth/time
bounds can interrupt deepening; statistics show the last completed depth honestly.
The heuristic remains separately selectable and is also used at search leaves
and for move ordering. This implements minimax, alpha-beta and iterative deepening;
it does not claim A*, neural networks or Monte Carlo tree search.

See `tournament-results.txt` for all four raw shard reports. Timings depend on
machine and load; a clock-interrupted decision can alter a seeded outcome.
