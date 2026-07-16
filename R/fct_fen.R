#' Assemble a FEN from a recognized 8x8 grid of piece symbols
#' Port of chessvision/fen.py
#' @noRd

#' symbols: 64 entries, row-major, top row first ("." for empty)
#' @noRd
grid_to_placement <- function(symbols) {
  if (length(symbols) != 64) stop("expected 64 squares")
  ranks <- character(8)
  for (row in 0:7) {
    fen_row <- ""
    empties <- 0L
    for (col in 0:7) {
      s <- symbols[row * 8 + col + 1]
      if (s == ".") {
        empties <- empties + 1L
      } else {
        if (empties > 0) {
          fen_row <- paste0(fen_row, empties)
          empties <- 0L
        }
        fen_row <- paste0(fen_row, s)
      }
    }
    if (empties > 0) fen_row <- paste0(fen_row, empties)
    ranks[row + 1] <- fen_row
  }
  paste(ranks, collapse = "/")
}

#' Grant castling only when king and matching rook sit on home squares.
#' `symbols` here is already in standard (white-bottom) orientation.
#' Index of row-major square (0-indexed row/col) -> 1-indexed R vector position.
#' @noRd
sq_idx <- function(row, col) row * 8 + col + 1

#' @noRd
infer_castling <- function(symbols) {
  rights <- ""
  if (symbols[sq_idx(7, 4)] == "K" && symbols[sq_idx(7, 7)] == "R") rights <- paste0(rights, "K")
  if (symbols[sq_idx(7, 4)] == "K" && symbols[sq_idx(7, 0)] == "R") rights <- paste0(rights, "Q")
  if (symbols[sq_idx(0, 4)] == "k" && symbols[sq_idx(0, 7)] == "r") rights <- paste0(rights, "k")
  if (symbols[sq_idx(0, 4)] == "k" && symbols[sq_idx(0, 0)] == "r") rights <- paste0(rights, "q")
  if (rights == "") "-" else rights
}

#' Build a full FEN string from recognized symbols.
#' `flip = TRUE` means the screenshot had black on the bottom.
#' @noRd
build_fen <- function(symbols, turn = "w", flip = FALSE) {
  if (flip) symbols <- rev(symbols)
  placement <- grid_to_placement(symbols)
  castling <- infer_castling(symbols)
  fen <- paste(placement, turn, castling, "-", "0", "1")
  list(fen = fen, placement = placement)
}
