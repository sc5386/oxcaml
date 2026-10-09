# Guandan in OCaml

A complete four-seat, two-team Guandan match, from level Two through victory at Ace. North and South are partners;
East and West are partners. The browser puts you in North's seat and uses the
stronger computer opponent for the other seats.

## Homework implementation

- **HW1 — states:** `logic/hw1.ml` contains small Guandan initial, intermediate,
  pre-terminal and terminal fixtures. These are exercised by HW3 tests.
- **HW2 — transitions:** `logic/hw2_tictactoe_logic.ml` provides immutable
  `make_move`, validates card ownership and combinations, removes cards, skips
  finished players, handles passes and partner lead transfer, and ends a round
  on a team double finish or the third individual finish.
- **HW3 — tests and CI:** unit and expect tests cover legal/illegal transitions,
  wilds, sequence boundaries, bomb ordering, fixtures and terminal behavior.
  200 seeded full-deck random walks check conservation, progress, legal moves
  and termination. Match tests cover scoring, tribute, resistance, Ace victory
  and demotion; 40 complete matches check conservation through every phase.
  GitHub Actions runs these checks on pushes and PRs.
- **HW4 — opponents:** all three agents remain selectable: uniform random,
  the original one-step heuristic, and **iterative-deepening team minimax with
  alpha-beta pruning**. Search maximizes the acting team and minimizes the other
  team, including turns skipped after a player finishes. It deepens through
  complete iterations, keeps the last completed decision when interrupted, and
  uses a heuristic only for leaf evaluation, move ordering and legal fallback.
  Default limits are 320 visited nodes, depth 6 and 1.9 seconds, with a margin
  for returning within two seconds. It samples unknown hands from the unseen
  double deck using only the bot's own cards, public plays and hand counts;
  it does not read actual opponents' hidden ranks. This is bounded search on a
  sampled deal, not a solution of the full imperfect-information game.
  Tests compare alpha-beta against exhaustive minimax, prove a future response
  changes the greedy choice, observe pruning, verify deadline interruption and
  check real Guandan legality and timing. The tournament runs 1,000 complete
  matches from Two to Ace with alternating team seats.

## Test bots in the Bot arena

Open the website and select **Bot arena**. Choose **Random**, **Original heuristic**
or **Minimax + alpha-beta** independently for North, East, South and West.
North/South and East/West are partners. Choose 1–1,000 complete matches and a seed,
then **Start automated games**. The bots automatically play every move, return
tribute, start new rounds and continue until an Ace match victory.

**Pause**, **Resume** and **Step one action** let you inspect progression. The
summary shows team wins, rounds, moves, per-seat total/mean/maximum computation
time, completed search depths, nodes and cutoffs. Times exclude the browser's
rendering interval. Refresh clears the in-memory simulation. A paused run keeps
its original configuration; changed selections apply to a newly started run.

For a faster native batch with any four-seat configuration:

```sh
dune exec --profile release bin/guandan_arena.exe -- 10 minimax heuristic random minimax
```

Arguments are the match count followed by North, East, South and West. Valid
agent keys are `random`, `heuristic` and `minimax`. It prints per-seat statistics.
For the assignment's search-versus-random comparison:

```sh
dune exec --profile release bin/guandan_tournament.exe -- 1000
```

The tournament accepts an optional node budget and first seed after the match
count, allowing disjoint seed ranges to run in parallel. Seeds reproduce the
sampled hidden deals and random choices. Time interruption can change results
on a slower or busy machine; timing measurements always depend on the machine.

Legacy `tictactoe` filenames are retained to preserve the course module/build
names; the game logic and HW1 fixtures are Guandan.

## Rules and scope

The engine uses two standard decks including jokers (108 cards, 27 per seat).
Supported plays are singles, pairs, triples, full houses, five-card straights,
three consecutive pairs, two consecutive triples, four-to-ten-card bombs,
five-card straight flushes and the four-joker bomb. Ordinary groups rank the
current level above Ace and below jokers. Sequences use natural 2–A ranks and
allow Ace at the low end. Heart level cards are wild for ordinary ranks only.
Bombs with four/five cards are below straight flushes, which are below bombs
with six or more cards; the four-joker bomb is highest.

Rules reference: https://www.pagat.com/climbing/guan_dan.html

`Game_state.create ?level ~north ~east ~south ~west ()` supports explicit small
fixtures and validates card types, hand sizes and the double-deck multiplicity.
The default level is Two. Empty fixture hands count as already finished.
`finished_players` contains actual finishers; the terminal decision appends
unplayed seats in canonical order when a double finish ends the round early.
Ambiguous wild plays can be explicitly declared with `Move.Play_as`; the browser
shows interpretation buttons. `Move.Play` defaults to the strongest valid
interpretation that beats the current trick. The legal move catalogue contains representatives for
rank patterns and straight-flush suits; it collapses equivalent copies and suit
choices, rather than enumerating every physical-card subset. Humans can submit
any valid combination through `make_move`.

## Complete match flow

`logic/game_match.ml` carries team levels and round history, redeals, applies
tribute, accepts return cards and chooses the next opening player. The UI keeps
the current match state in memory; selecting cards does not replay past rounds.
Choose **Next round** after a round; choose one return card when prompted.
**New match** resets both teams to level Two. Refreshing also starts a new match.

The default rules use three-level promotion for a double finish, two for a
first/third finish and one for first/fourth. Winning declarers at Ace must finish
first/second or first/third. Three failed Ace attempts demote a team to Two.
Red jokers can cancel tribute. Tributes use the highest non-wild cards; return
cards are 10 or lower and non-wild, excluding the received tribute. If no low
return card exists, the lowest available non-wild rank is eligible. Tribute ties
use a deterministic seat order, and equal-ranked highest tribute cards are
chosen automatically. The human chooses their return card.

`Game_match.rules` also supports four-level double-finish promotion, unrestricted
return-card rank, requiring a double finish at Ace, and demotion when Ace
declarers lose to a finishing play consisting entirely of Aces. These are rule
variants, not missing match phases. The defaults are shared by the UI and tests.

## Browser-only development with Codespaces

On this repository, select **Code → Codespaces → Create codespace**. The container
installs OCaml 4.14.2, dependencies and editor support, runs tests, and rebuilds
the browser bundle automatically. First setup can take several minutes.
After setup:

```sh
eval "$(opam env --switch=guandan)"
dune runtest
dune exec --profile release bin/guandan_tournament.exe -- 1000
dune build --profile release ui/tictactoe_ui.bc.js
cp _build/default/ui/tictactoe_ui.bc.js generated_js/
python3 -m http.server 8000
```

Open forwarded port 8000 in your browser. For local development with an existing
opam installation, initialize it, create the `guandan` switch with OCaml 4.14.2,
and run `opam install . --deps-only --with-test --yes` before these commands.

The checked-in `generated_js` file is what the static website executes; changing
OCaml source alone does not update it. Rebuild and copy it before publishing.
GitHub Actions also supplies the newly built JS and tournament report as a
workflow artifact; it does not deploy changes automatically.
