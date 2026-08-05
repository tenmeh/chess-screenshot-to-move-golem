# Architecture plan: a one-stop chess package for CRAN

Status: proposal. Nothing here is built yet.

## The claim we are making

Every chess package on CRAN starts from a position you already have. `ply`
parses a FEN, `chess2plyrs` builds a game move by move, `chess` wraps
python-chess. None of them can *get* you the position.

We can get it from a screenshot, from a FEN, from a PGN, and - once video
lands - from a recording of a game being played. Four ways in, one table out.
That is the whole pitch, and the architecture below exists to make it true
rather than merely claimed.

## Two packages, not one

| | `tanmai` | `tanmaiapp` |
| --- | --- | --- |
| Where | CRAN | GitHub |
| What | recognition, game model, analysis | the Shiny app |
| Depends on | magick, rsvg, V8, jsonlite | `tanmai`, shiny, bslib, golem |
| Licence | GPL-3 | AGPL-3 |
| Engines | optional, detected at runtime | baked into the image |

Roughly 2,900 of the current 4,957 lines are already engine- and Shiny-free and
move essentially unchanged. The ~2,000 lines of `mod_*.R`, `app_*.R` and
`golem_utils.R` stay behind.

Three packages (core / engines / app) is the obvious alternative and is
over-engineering at this size. Engines can be optional *within* the core
package; that is a solved pattern and costs one `Suggests` line.

## The data model

This is the load-bearing decision. Everything else is arrangement.

**One row per ply.** A game is a data frame:

```
ply         int   1, 2, 3, ...
move_no     int   1, 1, 2, 2, ...
side        chr   "w" / "b"
san         chr   "Nf3"
uci         chr   "g1f3"
fen_before  chr
fen_after   chr
cp          dbl   evaluation after the move, always from White's point of view
cp_loss     dbl   centipawns lost against the best move
best_uci    chr
best_san    chr
class       fct   best / good / inaccuracy / mistake / blunder
human_p     dbl   probability a human of the given rating plays this move
```

Everything downstream is `dplyr` over that frame. Accuracy is a `summarise()`.
The evaluation graph is a `ggplot()`. Turning points are `slice_max(cp_loss)`.
None of it needs to know where the game came from, which is precisely why four
input adapters cost almost nothing.

**One row per recognised board**, for the recognition side:

```
fen, turn, orientation, piece_set, confident, margin, median_occ, valid, source
```

with an optional 64-row companion carrying per-square confidence. Nothing else
on CRAN reports where recognition was unsure, and it is the most defensible
thing we ship.

## Public API

Adapters - all return one of the two shapes above:

```r
read_board(image, ...)   # screenshot, path, or magick object
read_fen(x)              # a string, four fields or six
read_pgn(x)              # text or path, one game or a database
read_video(path)         # mp4 / gif / directory of frames      (phase 3)
read_lichess(user, n)    # public API                           (phase 5)
read_chesscom(user, n)   # public API                           (phase 5)
```

Verbs:

```r
evaluate(game, depth =, engine =)   # fills cp, best_*, cp_loss, class
accuracy(game)                      # per-player summary
annotate(game)                      # ECO code, opening name, game phase
blunder_risk(position, rating)      # Maia x Stockfish
estimate_rating(game, side)
```

Output:

```r
plot(position)      # a board diagram
plot(game)          # the evaluation graph
write_pgn(game, path)
animate(game)       # gif
```

`read_*` never needs an engine. `evaluate()` and everything downstream of it
does, and says so plainly when one is missing.

## Dependencies

**Imports** - all verified current on CRAN (checked 2026-08-05): magick 2.9.1,
rsvg 2.7.0, V8 8.2.0, jsonlite, tools, utils, stats.

**Suggests** - processx (engine transport), testthat, knitr, rmarkdown,
ggplot2.

**Never a hard dependency**: Stockfish, lc0, Maia weights. These are found at
runtime, and their absence degrades features rather than breaking the package.

On V8: `rchess` drove chess.js through V8 and is archived, which reads as a
warning but is not one. V8 shipped 8.2.0 in April 2026 and carries a large
reverse-dependency tree. `rchess` was archived for maintenance neglect, not for
its dependency choice. We keep chess.js because the alternative does not exist:
`ply` was evaluated and returns UCI without SAN, and its PGN reader returns
headers and a ply count *without the moves*, so it cannot back a move list, a
turning-point table, or PGN import.

## What CRAN will object to

Concrete, in the order it will bite:

1. **Filesystem writes.** The piece-set cache writes to `tools::R_user_dir()`.
   That is permitted, but only on explicit user action - never at load time,
   never from an example, test or vignette. Default state must be "works with
   the bundled set alone".
2. **Network.** Downloading piece sets must be behind `\donttest{}` *and* an
   interactivity or environment-variable guard. CRAN runs `\donttest` in its
   extended checks, so `\donttest` alone is not a shield.
3. **Example runtime.** Every example must finish in a few seconds. Anything
   touching an engine is guarded, not timed.
4. **Vignettes** must build with no network and no engine. Precompute the
   engine-dependent figures and check in the results.
5. **Package size.** Currently ~870 KB across `R/`, `inst/`, `man/`, `tests/`
   and `vignettes/`. Comfortably inside CRAN's 5 MB guidance, and the 38 piece
   sets stay out of the tarball by design.

## Testing under those constraints

The existing suite already has the right shape and should be the rule: tests
*render their own boards* with `render_position()` and recognise them back. No
fixtures, no network, no engine, and the negative cases (held-out piece sets)
are synthesised the same way.

Engine-dependent tests use `skip_if_not(has_engine())`. A skip is not a
failure, so watch the test *count*, not just the colour - that has bitten this
project before.

## Licensing

The bundled cburnett SVGs in `inst/svg/` are **GPL-3.0**, and they are not
incidental: they are the default templates and what every recognition test
renders against. So the core package cannot be MIT without dropping the art,
and dropping it would leave a package that cannot work or test out of the box.

**Recommendation: GPL-3 for `tanmai`, AGPL-3 stays on `tanmaiapp`.** GPL-3 is
far friendlier to library use than AGPL, it matches the rest of the CRAN chess
ecosystem (`chess`, `chess2plyrs`, `chessResults` are all GPL), and AGPL keeps
doing its job where it actually means something - the network-served app.

## Phases

**1. Extract and submit.** Move the recognition and game-model code, define the
two data frames, ship `read_board()`, `read_fen()`, `read_pgn()`, the plot
methods and `write_pgn()`. Get `R CMD check --as-cran` clean *with no engine
installed at all*. This is submittable on its own and is already more than any
chess package on CRAN offers.

**2. The engine layer.** `evaluate()`, `accuracy()`, `annotate()`. Optional
throughout.

**3. Video to PGN.** The existing frame-by-frame tracker with a different frame
source. Largest capability jump for the least new code.

**4. The human layer.** `blunder_risk()`, `estimate_rating()`. Maia weights are
a download-on-demand or a separate data package - never in the tarball.

**5. API clients.** Lichess and Chess.com archives.

Phase 1 is the only one that has to be right the first time; CRAN is far more
forgiving of added features than of a changed API.

## Open questions

- Package name: `tanmai` is free on CRAN (verified), as are `boardreader`,
  `chessshot` and `maia`.
- Do we return base data frames or tibbles? Tibbles cost a dependency and buy
  printing; base frames keep Imports minimal.
- `estimate_rating()` is right ~75% of the time unconditionally and ~94% when
  it chooses to answer. That needs to be in the documentation, not just the
  vignette.
