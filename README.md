# chessvision (golem/Shiny)

A [golem](https://thinkr-open.github.io/golem/)-structured R Shiny app: drop or
paste a chess board screenshot, get the best move. This is an R/Shiny port of
[chess-screenshot-to-move](https://github.com/tenmeh/chess-screenshot-to-move)
(the original Python/Streamlit version) - same pipeline, same bundled
templates, reimplemented natively in R.

```
screenshot -> crop into 64 squares -> recognize pieces -> FEN -> Stockfish -> best move
```

Works out of the box on **Lichess** (cburnett piece set, bundled). Orientation
(white/black on the bottom) is detected automatically from the coordinate
labels, with a legality check as backstop. Other sites (Chess.com, custom
themes) need a one-time calibration - see the **Calibrate** tab in the app.

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
  mod_calibrate.R                    # Calibrate tab (teach a new piece set)
  fct_detect.R                       # screenshot -> 64 squares + orientation
  fct_recognize.R                    # template matching
  fct_fen.R                          # squares -> FEN, castling inference
  fct_chessjs.R                      # chess.js/V8 bridge (validity, SAN)
  fct_engine.R                       # Stockfish download + UCI
  fct_render.R                       # draw a position from the SVG piece set
  golem_utils.R                      # shared UI/reactive helpers
inst/
  app/templates.rds                  # bundled cburnett (Lichess) templates
  app/www/                           # paste.js, styles.css
  svg/                               # cburnett piece SVGs (from python-chess)
  js/chess.js                        # bundled chess.js (MIT)
data-raw/build_default_templates.R   # regenerate inst/app/templates.rds
```

## Known limitations

- **Side to move** can't be read from a still image - the UI asks for it.
- **En passant** is not inferred; **castling rights** are granted only when
  king and rook sit on their home squares.
- A wildly wrong or illegal recognized position from a non-Lichess site means
  you need to calibrate on that site's actual pieces (Calibrate tab).
- Stockfish is downloaded to `tools::R_user_dir("chessvision", "cache")` on
  first use - needs an internet connection once.
