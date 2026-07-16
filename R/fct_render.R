#' Render a chess position using the bundled cburnett SVG piece set (magick)
#' @noRd

LIGHT_SQ <- "#dee3e6"
DARK_SQ <- "#8ca2ad"

#' @noRd
piece_svg_path <- function(symbol) {
  color <- if (symbol == toupper(symbol)) "w" else "b"
  app_sys("svg", paste0(color, toupper(symbol), ".svg"))
}

#' Render an 8x8 checkerboard with pieces placed from a symbol grid.
#' `symbols`: 64 entries, row-major, top row first ("." for empty).
#' @noRd
render_position <- function(symbols, size = 512L) {
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
        img <- image_read(piece_svg_path(sym), density = sq * 2)
        img <- image_resize(img, paste0(sq, "x", sq))
        piece_cache[[sym]] <- img
      }
      board <- image_composite(board, piece_cache[[sym]], offset = geometry_point(c * sq, r * sq))
    }
  }
  board
}
