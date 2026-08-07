# Assemble a FEN from a recognized 8x8 grid of piece symbols.
# Port of tanmai/fen.py.

#' Convert a 64-symbol grid to a FEN piece-placement field
#'
#' @param symbols Character vector of 64 entries, row-major, top row first,
#'   using FEN piece letters and "." for an empty square.
#' @return The FEN placement string (ranks separated by "/").
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

#' Map a 0-indexed board row/column to a 1-indexed symbol-vector position
#'
#' @param row 0-indexed board row (0 = top rank shown).
#' @param col 0-indexed board column (0 = leftmost file shown).
#' @return The 1-indexed position into a row-major 64-symbol vector.
sq_idx <- function(row, col) row * 8 + col + 1

#' Infer castling rights from piece placement
#'
#' Grants a castling right only when the king and the matching rook sit on
#' their home squares. `symbols` must already be in standard (white-bottom)
#' orientation.
#'
#' @param symbols Character vector of 64 symbols, row-major, top row first.
#' @return A FEN castling field (e.g. "KQkq"), or "-" if no rights.
infer_castling <- function(symbols) {
  rights <- ""
  if (symbols[sq_idx(7, 4)] == "K" && symbols[sq_idx(7, 7)] == "R") rights <- paste0(rights, "K")
  if (symbols[sq_idx(7, 4)] == "K" && symbols[sq_idx(7, 0)] == "R") rights <- paste0(rights, "Q")
  if (symbols[sq_idx(0, 4)] == "k" && symbols[sq_idx(0, 7)] == "r") rights <- paste0(rights, "k")
  if (symbols[sq_idx(0, 4)] == "k" && symbols[sq_idx(0, 0)] == "r") rights <- paste0(rights, "q")
  if (rights == "") "-" else rights
}

#' Build a full FEN string from recognized symbols
#'
#' @param symbols Character vector of 64 symbols, row-major, top row first.
#' @param turn Side to move, "w" or "b".
#' @param flip `TRUE` if the screenshot had black on the bottom (the symbols are
#'   reversed to the standard white-bottom frame before assembly).
#' @return A list with `fen` (full FEN string) and `placement` (the placement
#'   field only).
build_fen <- function(symbols, turn = "w", flip = FALSE) {
  if (flip) symbols <- rev(symbols)
  placement <- grid_to_placement(symbols)
  castling <- infer_castling(symbols)
  fen <- paste(placement, turn, castling, "-", "0", "1")
  list(fen = fen, placement = placement)
}

#' Fill in the FEN fields people leave off
#'
#' Chess sites copy a FEN to the clipboard with the trailing fields trimmed -
#' most often the two clocks, sometimes everything after the placement. chess.js
#' validates strictly and rejects those outright, so they are completed here
#' before validation rather than reported to the user as malformed.
#'
#' Defaults are the conservative ones: White to move, no castling rights, no en
#' passant square, clocks at zero. Castling in particular is *not* inferred from
#' where the kings and rooks sit - a FEN that omits it is not claiming those
#' rights, and inventing them would silently change what the position means.
#'
#' @param fen A FEN string with one to six fields.
#' @return A six-field FEN string.
#' @examples
#' fen_complete("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -")
#' @export
fen_complete <- function(fen) {
  parts <- strsplit(trimws(fen), "[[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(fen)
  }
  out <- c(parts[1], "w", "-", "-", "0", "1")
  if (length(parts) > 1L) out[2:min(6L, length(parts))] <- parts[2:min(6L, length(parts))]
  paste(out, collapse = " ")
}
