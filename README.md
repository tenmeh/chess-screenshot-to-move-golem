# chessvision (golem/Shiny)

<!-- badges: start -->
[![R-CMD-check](https://github.com/tenmeh/chess-screenshot-to-move-golem/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tenmeh/chess-screenshot-to-move-golem/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A [golem](https://thinkr-open.github.io/golem/)-structured R Shiny app: drop or
paste a chess board screenshot, get the best move. This is an R/Shiny port of
[chess-screenshot-to-move](https://github.com/tenmeh/chess-screenshot-to-move)
(the original Python/Streamlit version) - same pipeline, same bundled
templates, reimplemented natively in R.

```
screenshot -> crop into 64 squares -> recognize pieces -> FEN -> Stockfish -> best move
                                                           |
                                                           +-> interactive board with live evaluation
```

**Interactive analysis board.** Recognized positions can be opened on a
draggable board that re-evaluates continuously: an eval bar, a running score
and depth, the engine's top three lines in SAN, and its best move drawn as an
arrow. One Stockfish process is kept alive per session and searched with
`go infinite`; the UI drains its output on a timer, so the search deepens live
without ever blocking Shiny's single thread.

**Zero calibration.** The piece set is auto-detected per screenshot: cburnett
(Lichess's default) is bundled, and one click in the *Piece sets* tab fetches
every other recognizable Lichess set (38 of them - merida, alpha, staunty,
horsey, maestro, ...) straight from lichess.org's public repository into your
local cache. Every square of the screenshot is scored against every template of
every installed set in a single matrix multiply; the best-matching set wins and
classifies the board.

**Orientation is determined, not guessed.** Signals are consulted
strongest-first:

1. an explicit **Board orientation** choice in the UI - always wins;
2. **where the armies sit** - the two sides start on opposite ends and
   essentially never swap, so this is decisive for real positions;
3. **piece brightness** during calibration - white pieces are light-filled, so
   the brighter end of a starting position is white's;
4. coordinate labels, then FEN legality, as last resorts.

Calibration additionally asserts that the white templates really are brighter
than the black ones, so a misjudged orientation can never silently teach the
recognizer white pieces from black artwork (which would corrupt every later
reading).

Piece art is downloaded at runtime for personal use and never redistributed
with this package - several sets are CC BY-NC-SA or freeware, which don't mix
with an MIT package. `piece_set_manifest()` records each set's license.
Truly unknown sets (Chess.com themes, custom art) still have the one-shot
manual calibration fallback.

## How it differs from the Python version

- **Piece recognition**: same background-subtracted normalized-cross-correlation
  approach, reimplemented in R (`magick` for image handling). Templates are
  bundled as `inst/app/templates.rds`, built from the app's own SVG-compositing
  pipeline (`data-raw/build_default_templates.R`) rather than round-tripped
  from Python - this avoids any cross-renderer mismatch between what built the
  templates and what recognizes squares at runtime.
- **Chess rules** (FEN validity, SAN move notation): R has no mature native
  chess-rules package, so the app embeds [chess.js](https://github.com/jhlywa/chess.js)
  (MIT-licensed, `inst/js/chess.js`) and runs it via the `V8` package - this
  plays the same role `python-chess` played in the original.
- **Engine**: Stockfish is downloaded on first use (same official release
  binaries) and driven directly over UCI via `processx`, rather than through a
  wrapper library.
- **UI**: drag-drop (`fileInput`) plus a custom clipboard-paste zone
  (`inst/app/www/paste.js` + `Shiny.setInputValue`), since Shiny has no
  built-in paste-image widget.

## Setup

```r
install.packages(c(
  "golem", "shiny", "magick", "processx", "jsonlite", "V8",
  "pkgload", "here"  # dev-only
))
```

Rtools/compilation is not required - all of the above install as prebuilt
binaries on Windows/macOS.

## Run

```r
pkgload::load_all(".")
shiny::runApp(shiny::shinyApp(ui = app_ui, server = app_server))
```

or, once installed as a package:

```r
chessvision::run_app()
```

or during development:

```r
source("dev/run_dev.R")
```

## Tests

```r
devtools::test()    # or devtools::check() to run them inside R CMD check
```

The suite pins the failures this project actually hit - orientation detection
and the colour-swap guard, the autocrop off-by-one across board sizes, FEN and
castling assembly, and the confidence gate - so a regression fails loudly
instead of quietly producing a wrong FEN. Each test was mutation-checked:
reintroducing the original bug makes it fail. Tests use only the bundled
cburnett art, so they need no network access and no chess engine, and CI runs
them on every push and pull request to `main`.

## Layout

```
R/
  app_ui.R, app_server.R, run_app.R  # golem entry points
  mod_analyze.R                      # Analyze tab (paste -> FEN -> best move)
  mod_board.R                        # Board tab (interactive + live eval)
  mod_calibrate.R                    # Calibrate tab (teach a new piece set)
  fct_detect.R                       # screenshot -> 64 squares + orientation
  fct_recognize.R                    # template matching
  fct_fen.R                          # squares -> FEN, castling inference
  fct_chessjs.R                      # chess.js/V8 bridge (validity, SAN)
  fct_engine.R                       # Stockfish download + one-shot UCI query
  fct_engine_session.R               # persistent engine, MultiPV, live polling
  fct_render.R                       # draw a position from any SVG piece set
  fct_piecesets.R                    # runtime piece-set download + manifest
  golem_utils.R                      # shared UI/reactive helpers
inst/
  app/templates.rds                  # bundled cburnett (Lichess) templates
  app/www/                           # paste.js, board.js, styles.css,
                                     #   chessboard.js + css (MIT, vendored)
  svg/                               # cburnett piece SVGs (from python-chess)
  js/chess.js                        # bundled chess.js (MIT), used by V8 and
                                     #   served to the browser for legality
data-raw/build_default_templates.R   # regenerate inst/app/templates.rds
```

## Known limitations

- **Side to move** can't be read from a still image - the UI asks for it.
- **En passant** is not inferred; **castling rights** are granted only when
  king and rook sit on their home squares.
- When the screenshot's piece set isn't installed, recognition **says so**
  instead of emitting a confident wrong FEN: a low-confidence match (small
  winner-vs-runner-up margin, or a badly-unmatched square) triggers an
  "unrecognized piece set" warning pointing you to download the Lichess sets
  or calibrate. Chess.com's sets are proprietary and can't be auto-fetched;
  one-shot manual calibration covers them (and any custom theme).
- Stockfish and piece sets are downloaded to
  `tools::R_user_dir("chessvision", "cache")` on first use - needs an
  internet connection once.
- A few Lichess sets are hard on template matching because their *own* pieces
  are nearly identical: `firi`'s black and white rooks are 98% similar, and
  `governor` and `pirouetti` are close behind. They read correctly from a clean
  screenshot but can misread a heavily compressed or downscaled one. `mono`,
  `letter` and `disguised` are excluded outright - their pieces are ambiguous
  by design. `reillycraig` is excluded automatically because its SVGs do not
  rasterize.
- **Chess.com piece art is deliberately not downloaded.** It is proprietary and
  its terms prohibit scraping, so bundling or auto-fetching it would be a
  licensing problem. Use the one-shot manual calibration instead - it is
  designed for exactly this and works for any custom theme.
