# Board orientation: which colour is on the bottom of the screenshot.
#
# Reading the tiny rank digits is fragile - font, colour and placement vary by
# site, and plenty of screenshots have no coordinates at all. So coordinate ink
# is now the *last* resort. The signals below are ordered by how hard they are
# to fool:
#
#   1. explicit user choice          - deterministic, always wins
#   2. army position (piece mass)    - white and black start on opposite ends
#      and essentially never swap ends, so "which half holds the white army"
#      is a very strong signal for any real position
#   3. piece brightness (start pos)  - white pieces are light-filled, black are
#      dark-filled, so for a *starting position* the brighter half is white's
#   4. coordinate label ink          - the old heuristic
#   5. legality of the resulting FEN - tiebreak only

#' Mean background-subtracted brightness of the pieces in a set of squares
#'
#' White pieces are light-filled and black pieces dark-filled, so once the
#' square background is removed a white piece leaves a much higher (less
#' negative) mean residual than the same black piece. This holds across piece
#' sets because it is a property of the pieces, not of the artwork style.
#'
#' @param squares A list of 64 magick square images, row-major, top row first.
#' @param idx 1-based indices of the squares to measure.
#' @return The mean residual over occupied squares, or `NA_real_` if none are
#'   occupied.
squares_mean_brightness <- function(squares, idx) {
  vals <- c()
  for (i in idx) {
    core <- sq_core(to_gray_matrix(squares[[i]]))
    # Skip empty squares: they carry no colour information.
    if (sq_energy(core) < 8) next
    vals <- c(vals, mean(core))
  }
  if (!length(vals)) NA_real_ else mean(vals)
}

#' Detect orientation of a *starting position* from piece brightness
#'
#' Compares the two back ranks at the top of the image against the two at the
#' bottom. Whichever end is brighter holds the white army.
#'
#' @param squares A list of 64 magick square images, row-major, top row first.
#' @return `TRUE` if black is on the bottom, `FALSE` if white is on the bottom,
#'   or `NA` if the two ends can't be told apart.
detect_flip_start_brightness <- function(squares) {
  top <- squares_mean_brightness(squares, 1:16) # image rows 1-2
  bottom <- squares_mean_brightness(squares, 49:64) # image rows 7-8
  if (is.na(top) || is.na(bottom)) {
    return(NA)
  }
  # Require a clear difference; near-equal means we genuinely cannot tell.
  if (abs(top - bottom) < 5) {
    return(NA)
  }
  top > bottom # brighter top => white is on top => black on the bottom
}

#' Detect orientation from where each army sits
#'
#' Uses recognized symbols: the mean image row of the white pieces versus the
#' black pieces. Rows are numbered from the top, so the army with the larger
#' mean row is the one on the bottom.
#'
#' @param symbols Character vector of 64 recognized symbols in image order.
#' @return `TRUE` if black is on the bottom, `FALSE` if white is on the bottom,
#'   or `NA` when one side has no pieces or the two are too close to call.
detect_flip_piece_mass <- function(symbols) {
  if (length(symbols) != 64) {
    return(NA)
  }
  rows <- rep(0:7, each = 8)
  is_white <- symbols %in% c("P", "N", "B", "R", "Q", "K")
  is_black <- symbols %in% c("p", "n", "b", "r", "q", "k")
  if (!any(is_white) || !any(is_black)) {
    return(NA)
  }
  wm <- mean(rows[is_white])
  bm <- mean(rows[is_black])
  # A whole rank of separation is ~1.0; require a modest but real gap so a
  # wild middlegame with armies intermingled falls through to another signal.
  if (abs(wm - bm) < 0.5) {
    return(NA)
  }
  wm < bm # white higher up the image => white on top => black on the bottom
}

#' Assert (and if necessary repair) the white/black brightness invariant
#'
#' A template library learned from a board whose orientation was misjudged has
#' every white template built from a black piece and vice versa. That is
#' catastrophic and silent, so check it explicitly: across the six piece kinds,
#' the white templates must be brighter than the black ones.
#'
#' @param lib A template library from [build_from_start_position()].
#' @return A list with the (possibly colour-corrected) `lib` and `swapped`,
#'   indicating whether the colours had to be exchanged.
fix_template_colour_swap <- function(lib) {
  kinds <- c("P", "N", "B", "R", "Q", "K")
  have <- kinds[kinds %in% names(lib$pieces) & tolower(kinds) %in% names(lib$pieces)]
  if (!length(have)) {
    return(list(lib = lib, swapped = FALSE))
  }
  white <- mean(vapply(lib$pieces[have], mean, numeric(1)))
  black <- mean(vapply(lib$pieces[tolower(have)], mean, numeric(1)))
  if (white >= black) {
    return(list(lib = lib, swapped = FALSE))
  }
  # Colours are inverted: exchange each piece's template with its counterpart.
  fixed <- lib$pieces
  for (k in have) {
    tmp <- fixed[[k]]
    fixed[[k]] <- lib$pieces[[tolower(k)]]
    fixed[[tolower(k)]] <- tmp
  }
  lib$pieces <- fixed
  list(lib = lib, swapped = TRUE)
}

#' Resolve the orientation of a calibration (starting-position) screenshot
#'
#' @param squares A list of 64 magick square images, row-major, top row first.
#' @param board_img The normalized board image, for the coordinate fallback.
#' @param manual One of "auto", "white" (white on bottom) or "black".
#' @return A list with `flip` (`TRUE` if black is on the bottom) and `source`,
#'   naming the signal that decided it.
resolve_calibration_flip <- function(squares, board_img, manual = "auto") {
  if (identical(manual, "white")) {
    return(list(flip = FALSE, source = "you chose white on the bottom"))
  }
  if (identical(manual, "black")) {
    return(list(flip = TRUE, source = "you chose black on the bottom"))
  }
  bright <- detect_flip_start_brightness(squares)
  if (!is.na(bright)) {
    return(list(flip = bright, source = "piece brightness"))
  }
  coord <- detect_flip(board_img)
  if (!is.na(coord)) {
    return(list(flip = coord, source = "coordinate labels"))
  }
  list(flip = FALSE, source = "assumed (no reliable signal)")
}
