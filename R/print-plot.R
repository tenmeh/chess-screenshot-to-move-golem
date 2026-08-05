# How the two data frames show themselves.
#
# They are ordinary data frames and behave like ordinary data frames - the
# whole point of the design is that dplyr, ggplot2 and `[` all work without
# knowing anything about chess. These methods only make the console output
# readable; nothing in the package depends on them.

#' @export
print.tanmai_position <- function(x, ...) {
  cat("<tanmai position>", if (isTRUE(x$valid)) "" else "  (not a legal position)", "\n", sep = "")
  cat("  fen        ", x$fen, "\n", sep = "")
  cat("  to move    ", if (identical(x$turn, "w")) "white" else "black", "\n", sep = "")
  if (identical(x$source, "screenshot")) {
    cat("  piece set  ", x$piece_set, "\n", sep = "")
    cat("  orientation", " ", x$orientation, " on the bottom (", x$orientation_source, ")\n", sep = "")
    # The confidence flag is the reason this adapter is different from the
    # others, so it is stated rather than left in a column the user has to go
    # looking for.
    if (isTRUE(x$confident)) {
      cat("  confidence ", sprintf(
        " recognised (margin %.2f, typical square %.2f)\n",
        x$margin, x$median_occ
      ), sep = "")
    } else {
      cat("  confidence ", sprintf(
        " LOW - the piece set does not match anything installed (margin %.2f, typical square %.2f)\n",
        x$margin, x$median_occ
      ), sep = "")
      cat("               treat the FEN as a guess to correct, not as fact\n")
    }
  }
  invisible(x)
}

#' @export
print.tanmai_game <- function(x, ...) {
  h <- attr(x, "headers")
  who <- function(k) if (!is.null(h) && k %in% names(h) && !identical(unname(h[[k]]), "?")) unname(h[[k]]) else NULL
  white <- who("White")
  black <- who("Black")
  cat("<tanmai game>  ", nrow(x), " plies", sep = "")
  if (!is.null(white) && !is.null(black)) cat("  -  ", white, " vs ", black, sep = "")
  cat("\n")
  scored <- sum(!is.na(x$cp))
  cat("  evaluated  ", scored, "/", nrow(x),
    if (scored == 0L) "  (call evaluate() to fill these in)" else "", "\n\n",
    sep = ""
  )
  NextMethod()
}

#' Draw a position or a game
#'
#' `plot()` on a position draws the board. `plot()` on a game draws the
#' evaluation across the game, which needs [evaluate()] to have been run first.
#'
#' @param x A [tanmai_position] or [tanmai_game].
#' @param size Board size in pixels.
#' @param set_dir Directory of piece art to draw with. Defaults to the bundled
#'   set.
#' @param ... Unused.
#' @return Invisibly, `x`.
#' @examples
#' plot(read_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
#' @export
plot.tanmai_position <- function(x, size = 512L, set_dir = NULL, ...) {
  img <- render_position(
    fen_to_symbols(x$fen),
    size = size,
    set_dir = set_dir %||% default_piece_dir()
  )
  # graphics::plot() on a magick image needs the raster, and going through
  # rasterImage keeps this working in a plain R session with no magick viewer.
  graphics::plot(c(0, 1), c(0, 1),
    type = "n", axes = FALSE, xlab = "", ylab = "",
    asp = 1, main = NULL
  )
  graphics::rasterImage(grDevices::as.raster(img), 0, 0, 1, 1)
  invisible(x)
}

#' @rdname plot.tanmai_position
#' @export
plot.tanmai_game <- function(x, ...) {
  if (all(is.na(x$cp))) {
    stop(
      "This game has no evaluations yet. Run evaluate() on it first.",
      call. = FALSE
    )
  }
  cp <- x$cp / 100 # centipawns are an engine unit; pawns are a human one
  graphics::plot(x$ply, cp,
    type = "l", xlab = "ply", ylab = "evaluation (pawns, + is White)",
    ...
  )
  graphics::abline(h = 0, lty = 3)
  invisible(x)
}
