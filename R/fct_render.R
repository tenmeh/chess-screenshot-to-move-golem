# Render a chess position from an SVG piece set using magick.

LIGHT_SQ <- "#dee3e6"
DARK_SQ <- "#8ca2ad"

# The standard starting position as a 64-symbol grid (row-major, top first).
START_SYMBOLS <- c(
  "r", "n", "b", "q", "k", "b", "n", "r",
  "p", "p", "p", "p", "p", "p", "p", "p",
  rep(".", 32),
  "P", "P", "P", "P", "P", "P", "P", "P",
  "R", "N", "B", "Q", "K", "B", "N", "R"
)

#' Resolve the image file path for a piece symbol within a set
#'
#' Most sets ship SVG, but a few use webp or png, so the extension is resolved
#' from what the set directory actually contains.
#'
#' @param symbol A FEN piece letter (uppercase = white, lowercase = black).
#' @param set_dir Directory holding the 12 piece images (defaults to bundled
#'   cburnett).
#' @return The file path of the matching image (e.g. `wN.svg` for "N").
piece_svg_path <- function(symbol, set_dir = app_sys("svg")) {
  color <- if (symbol == toupper(symbol)) "w" else "b"
  base <- paste0(color, toupper(symbol))
  ext <- piece_set_ext(set_dir)
  if (is.null(ext)) ext <- "svg"
  file.path(set_dir, paste0(base, ".", ext))
}

#' Render an 8x8 board with pieces placed from a symbol grid
#'
#' @param symbols Character vector of 64 symbols, row-major, top row first,
#'   with "." for an empty square.
#' @param size Board side length in pixels.
#' @param set_dir Directory holding the 12 piece SVGs (defaults to bundled
#'   cburnett).
#' @return A magick image of the rendered board.
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
