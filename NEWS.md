# chessvision (development version)

## New features

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
