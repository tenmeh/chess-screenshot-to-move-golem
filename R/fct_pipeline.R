#' End-to-end: board image -> recognized position, with auto orientation.
#' Port of chessvision/pipeline.py.
#' @noRd

#' Pick orientation. Legality is primary; the coordinate hint breaks ties.
#'
#' A wrong 180-degree flip scrambles the whole board, so we never flip on a
#' weak signal: if exactly one orientation is legal we take it, and only when
#' both (or neither) are legal do we defer to the coordinate hint, then to the
#' default of white-on-bottom.
#' @noRd
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

#' image: a magick image (the raw screenshot).
#'
#' `libs`: named list of template libraries to auto-detect across (default:
#' every locally available set). Returns a list: board_img, symbols,
#' display_symbols, fen, placement, flip, flip_detected, valid, set,
#' set_scores.
#' @noRd
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
    set_scores = rec$scores
  )
}
