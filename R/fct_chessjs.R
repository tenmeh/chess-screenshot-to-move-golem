# Chess rules (legality, SAN) via the bundled chess.js library run in V8.
#
# Mirrors the role python-chess played in the original app: FEN validity
# checking and converting a UCI move to SAN. chess.js is a CommonJS build, so
# we provide a minimal module/exports shim before sourcing it.

#' Create a chess.js V8 context
#'
#' @return A V8 context with `ChessCtor` and `validateFenJS` bound, ready for
#'   [validate_fen()] and [fen_move_to_san()].
new_chess_context <- function() {
  ctx <- V8::v8()
  ctx$eval("var module = {exports: {}}; var exports = module.exports;")
  invisible(ctx$source(app_sys("js/chess.js")))
  ctx$eval("var ChessCtor = module.exports.Chess;")
  ctx$eval("var validateFenJS = module.exports.validateFen;")
  ctx
}

#' Validate a FEN via chess.js
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param fen A FEN string.
#' @return The parsed chess.js validation result (a list with `ok` and, on
#'   failure, `error`).
validate_fen <- function(ctx, fen) {
  ctx$assign("fenTmp", fen)
  res <- ctx$eval("JSON.stringify(validateFenJS(fenTmp))")
  jsonlite::fromJSON(res)
}

#' Test whether a FEN is legal
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param fen A FEN string.
#' @return `TRUE` if the FEN is legal, `FALSE` otherwise.
is_valid_fen <- function(ctx, fen) {
  isTRUE(tryCatch(validate_fen(ctx, fen)$ok, error = function(e) FALSE))
}

#' Convert a UCI move to SAN for a position
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param fen A FEN string of the position before the move.
#' @param uci_move A UCI move string (e.g. "e2e4", "e7e8q").
#' @return The move in SAN (e.g. "e4", "e8=Q"), or `NULL` if illegal.
fen_move_to_san <- function(ctx, fen, uci_move) {
  from <- substr(uci_move, 1, 2)
  to <- substr(uci_move, 3, 4)
  promo <- if (nchar(uci_move) > 4) substr(uci_move, 5, 5) else NA
  ctx$assign("fenTmp2", fen)
  ctx$eval("var gTmp = new ChessCtor(fenTmp2);")
  move <- if (!is.na(promo)) {
    list(from = from, to = to, promotion = promo)
  } else {
    list(from = from, to = to)
  }
  ctx$assign("moveTmp", move, auto_unbox = TRUE)
  res <- ctx$eval("(function(){ var m = gTmp.move(moveTmp); return m ? JSON.stringify({san: m.san}) : JSON.stringify({san: null}); })()")
  jsonlite::fromJSON(res)$san
}
