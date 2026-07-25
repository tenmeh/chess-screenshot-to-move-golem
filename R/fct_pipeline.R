# End-to-end: board image -> recognized position, with auto orientation.
# Port of chessvision/pipeline.py.

#' Resolve board orientation, preferring a legal position
#'
#' Legality is primary; the coordinate hint breaks ties. A wrong 180-degree
#' flip scrambles the whole board, so orientation is never flipped on a weak
#' signal: if exactly one orientation is legal it is taken, and only when both
#' (or neither) are legal does the coordinate hint decide, then the default of
#' white-on-bottom.
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param symbols Character vector of 64 recognized symbols, image order.
#' @param turn Side to move, "w" or "b".
#' @param flip_hint Orientation hint from [detect_flip()] (`TRUE`/`FALSE`/`NA`).
#' @return A list with `fen`, `placement`, `flip` (chosen orientation) and
#'   `valid` (whether the chosen FEN is legal).
resolve_orientation <- function(ctx, symbols, turn, flip_hint) {
  cand_false <- build_fen(symbols, turn = turn, flip = FALSE)
  cand_true <- build_fen(symbols, turn = turn, flip = TRUE)
  legal_false <- is_valid_fen(ctx, cand_false$fen)
  legal_true <- is_valid_fen(ctx, cand_true$fen)

  if (legal_false && !legal_true) {
    flip <- FALSE
  } else if (legal_true && !legal_false) {
    flip <- TRUE
  } else if (!is.na(flip_hint)) {
    flip <- flip_hint
  } else {
    flip <- FALSE
  }

  list(
    fen = if (flip) cand_true$fen else cand_false$fen,
    placement = if (flip) cand_true$placement else cand_false$placement,
    flip = flip,
    valid = if (flip) legal_true else legal_false
  )
}

#' Recognize a board screenshot end-to-end
#'
#' Crops and splits the screenshot, auto-detects the piece set, reads the
#' symbols, and resolves orientation into a FEN.
#'
#' @param image A magick image of the raw screenshot.
#' @param libs Named list of template libraries to auto-detect across (default:
#'   every locally available set, via [load_all_template_libraries()]).
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param turn Side to move, "w" or "b".
#' @param autocrop Whether to trim near-uniform margins before splitting.
#' @return A list describing the recognition: `board_img`, `symbols`,
#'   `display_symbols` (white-bottom frame matching `fen`), `fen`, `placement`,
#'   `flip`, `flip_detected`, `valid`, `set`, `set_scores`, `set_confident`,
#'   `set_margin` and `set_min_occ`.
recognize_position <- function(image, libs = load_all_template_libraries(),
                               ctx, turn = "w", autocrop = TRUE) {
  board_img <- prepare_board(image, autocrop)
  squares <- split_board(board_img)
  rec <- recognize_symbols_auto(squares, libs)
  symbols <- rec$symbols
  flip_detected <- detect_flip(board_img)
  resolved <- resolve_orientation(ctx, symbols, turn, flip_detected)

  list(
    board_img = board_img,
    symbols = symbols,
    # symbols in the same (white-bottom) orientation as `placement`/`fen`,
    # for rendering a preview that matches what the FEN describes.
    display_symbols = if (resolved$flip) rev(symbols) else symbols,
    fen = resolved$fen,
    placement = resolved$placement,
    flip = resolved$flip,
    flip_detected = flip_detected,
    valid = resolved$valid,
    set = rec$set,
    set_scores = rec$scores,
    set_confident = rec$confident,
    set_margin = rec$margin,
    set_min_occ = rec$min_occ
  )
}
