#' Render a chess position using the bundled cburnett SVG piece set (magick)
#' @noRd

LIGHT_SQ <- "#dee3e6"
DARK_SQ <- "#8ca2ad"

#' The standard starting position as a 64-symbol grid (row-major, top first).
#' @noRd
START_SYMBOLS <- c(
  "r", "n", "b", "q", "k", "b", "n", "r",
  "p", "p", "p", "p", "p", "p", "p", "p",
  rep(".", 32),
  "P", "P", "P", "P", "P", "P", "P", "P",
  "R", "N", "B", "Q", "K", "B", "N", "R"
)

#' @noRd
piece_svg_path <- function(symbol, set_dir = app_sys("svg")) {
  color <- if (symbol == toupper(symbol)) "w" else "b"
  file.path(set_dir, paste0(color, toupper(symbol), ".svg"))
}

#' Render an 8x8 checkerboard with pieces placed from a symbol grid.
#' `symbols`: 64 entries, row-major, top row first ("." for empty).
#' `set_dir`: directory holding the 12 piece SVGs (defaults to bundled cburnett).
#' @noRd
render_position <- function(symbols, size = 512L, set_dir = app_sys("svg")) {
  sq <- as.integer(size / 8)
  board <- image_blank(size, size, color = LIGHT_SQ)
  # paint dark squares
  for (r in 0:7) {
    for (c in 0:7) {
      if ((r + c) %% 2 == 1) {
        dark_sq <- image_blank(sq, sq, color = DARK_SQ)
        board <- image_composite(board, dark_sq, offset = geometry_point(c * sq, r * sq))
      }
    }
  }
  # composite pieces
  piece_cache <- new.env()
  for (r in 0:7) {
    for (c in 0:7) {
      sym <- symbols[r * 8 + c + 1]
      if (sym == ".") next
      if (is.null(piece_cache[[sym]])) {
        img <- image_read(piece_svg_path(sym, set_dir), density = sq * 2)
        img <- image_resize(img, paste0(sq, "x", sq))
        piece_cache[[sym]] <- img
      }
      board <- image_composite(board, piece_cache[[sym]], offset = geometry_point(c * sq, r * sq))
    }
  }
  board
}
