# chessvision (development version)

## New features

* **Live game tracking.** Follow a game as it is played rather than analysing
  one frozen position. Share the window you are playing in, or paste a fresh
  screenshot after each move, and the app keeps the running game: the move
  list, an evaluation per move, a graph of the evaluation across the game, and
  a list of the moves where it turned.

* A position only joins the game when exactly one sequence of legal moves
  explains how it got there. That single rule does the work of a pile of
  ad-hoc filters: it throws out misread squares, ignores frames caught
  mid-animation, recovers a ply that was never seen on screen, and handles
  castling, en passant and promotion correctly because chess.js generates the
  moves rather than a square-by-square diff guessing at them. Readings that
  cannot be explained are counted and shown, not quietly appended - a tracker
  that invents moves is worse than one that says it lost the thread.

* Which way round the board is, and whose turn it is, are settled by the game
  instead of guessed from pixels. A screenshot can show neither, so both
  readings are put to the same gate and the one that continues the game legally
  is the one that was right.

* Clicking a move - in the list, on the graph, or in the turning points -
  rewinds the board to just before it and offers to ask the Blunder Radar
  whether the mistake was predictable. After `3.Qh5 Nf6` it reports that a 1500
  player plays `Nf6` here 6% of the time and that it is the single largest
  source of risk in the position: what the move cost and why it was tempting,
  in one place.

* Screen capture is `getDisplayMedia` - the browser's own dialog, one choice of
  screen, window or tab, and the board marked once by dragging over a still
  frame. Frames are compared in the browser and only a settled, changed picture
  is sent, so the cost is roughly one recognition per move rather than one per
  tick. Mark the region roughly on the board's edge: it is grown outward a
  little before use, since slicing into the board is fatal to recognition,
  but a very loose region is no good either - past a margin of about 5% the
  edge detection stops finding the board. It needs a current browser over
  HTTPS or localhost, and the app's window has to stay visible, because
  browsers throttle timers in a hidden tab.
  A web page cannot read the screen unprompted, or watch the clipboard while
  another window has focus; neither is pretended at, and pasting after each
  move is a first-class alternative rather than a fallback.

* **Blunder Radar.** An engine tells you what is best; this tells you what is
  likely to go *wrong*. For an opponent of a chosen strength (1100/1500/1900),
  it scores every legal move by `P(a human plays it) x centipawns it loses`,
  combining [Maia](https://github.com/CSSLab/maia-chess) human-move
  probabilities with Stockfish evaluations. Known traps score roughly 2000x a
  quiet position: Legal's mate 6724 cp, Scholar's mate 1673 cp, versus 3.0 cp
  for the starting position.

* Board arrows show the engine's choice in blue and the moves a human is
  likely to get wrong in orange/red. Arrows are picked by *risk contribution*
  rather than raw probability, because in a trap the dangerous move is by
  construction not the most likely one - after `3.Qh5`, the losing `Nf6` is
  only the fourth most probable reply, and would otherwise never be drawn.

* The interactive board now lives on the analyze page instead of a separate
  tab, and a recognized screenshot loads onto it automatically. Reading a
  position and exploring it is one task; splitting it across tabs meant a
  round trip and a button in between.

* lc0 and the Maia networks are built into the container image, so the
  Blunder Radar works on a deployed instance and not just locally.

## Bug fixes

* The interactive board keeps its move history when a tracked game drives it.
  Inferred moves are played onto it one at a time instead of the position being
  set outright, so its move numbers agree with the game's rather than
  restarting from wherever the last jump landed.

* "Open in board" no longer requires the piece set to have been recognized
  confidently. It now only requires the position to be structurally legal, so
  a Chess.com or custom-theme screenshot can still be sent to the board and
  corrected by hand rather than being a dead end.

* Misread squares can be fixed by dragging pieces on the board directly,
  instead of needing a full recalibration of the piece set.

* R package versions in the container image are installed from `renv.lock`
  rather than a hand-maintained list in the `Dockerfile`, so local
  development and the deployed image can no longer drift apart. `rsvg` - a
  required dependency - was missing from the lockfile entirely.

* Container builds retry transient package-mirror failures instead of failing
  the whole build over a single unreachable file.
