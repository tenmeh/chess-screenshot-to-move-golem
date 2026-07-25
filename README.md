# chessvision (golem/Shiny)

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
~20 more popular Lichess sets (merida, alpha, staunty, horsey, maestro, ...)
straight from lichess.org's public repository into your local cache. Every
square of the screenshot is scored against every template of every installed
set in a single matrix multiply; the best-matching set wins and classifies the
board. Orientation (white/black on the bottom) is detected automatically from
the coordinate labels, with a legality check as backstop.

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
