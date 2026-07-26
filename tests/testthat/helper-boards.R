# Shared helpers. Everything here uses only the bundled cburnett art, so the
# suite needs no network access and no chess engine.

MID_FEN <- "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R"

#' Expand a FEN placement field into a 64-symbol grid
fen_symbols <- function(placement) {
  syms <- character(0)
  for (row in strsplit(placement, "/")[[1]]) {
    for (ch in strsplit(row, "")[[1]]) {
      syms <- if (grepl("[0-9]", ch)) {
        c(syms, rep(".", as.integer(ch)))
      } else {
        c(syms, ch)
      }
    }
  }
  syms
}

#' Render a position with the bundled piece set
board_of <- function(symbols, size = 512) {
  render_position(symbols, size = size)
}

#' Squares of a rendered position, without auto-cropping
squares_of <- function(symbols, size = 512) {
  split_board(prepare_board(board_of(symbols, size), autocrop = FALSE))
}

#' The bundled cburnett library, as the only installed set
cburnett_libs <- function() {
  list(cburnett = load_template_library(app_sys("app", "templates.rds")))
}
