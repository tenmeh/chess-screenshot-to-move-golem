# The public way in.
#
# Four things can give you a chess position - a screenshot, a FEN, a PGN, and
# (later) a video - and they all produce one of two data frames. Everything
# downstream reads those frames and never asks where they came from, which is
# what makes a fourth input adapter nearly free to add.
#
# The engine columns are present but empty here. Filling them is `evaluate()`'s
# job, and it needs Stockfish; reading a position does not, and must keep
# working on a machine that has no engine at all.

# chess.js lives in a V8 context that is expensive to build and safe to reuse,
# so the package keeps exactly one and builds it on first use. Not at load
# time: starting a JavaScript runtime as a side effect of `library(tanmai)`
# would be rude, and would run on CRAN's check machines for no reason.
.tanmai <- new.env(parent = emptyenv())

chess_ctx <- function() {
  if (is.null(.tanmai$ctx)) .tanmai$ctx <- new_chess_context()
  .tanmai$ctx
}

# The bundled cburnett art, which is always present. Stated as system.file()
# rather than golem's app_sys() because the library half of this package must
# not depend on a Shiny framework to find its own files.
default_piece_dir <- function() system.file("svg", package = "tanmai")

#' The columns every position carries
#'
#' @param fen,turn The position and side to move.
#' @param source Which adapter produced this row.
#' @param ... Recognition fields, defaulted for adapters that do not read art.
#' @return A one-row `tanmai_position` data frame.
#' @noRd
new_position <- function(fen, turn, source,
                         orientation = "white", piece_set = NA_character_,
                         confident = NA, margin = NA_real_,
                         median_occ = NA_real_, valid = TRUE,
                         orientation_source = NA_character_) {
  structure(
    data.frame(
      fen = fen, turn = turn, orientation = orientation,
      piece_set = piece_set, confident = confident, margin = margin,
      median_occ = median_occ, valid = valid,
      orientation_source = orientation_source, source = source,
      stringsAsFactors = FALSE
    ),
    class = c("tanmai_position", "data.frame")
  )
}

MOVE_CLASSES <- c("best", "good", "inaccuracy", "mistake", "blunder")

#' Read a position from a FEN string
#'
#' The fast path: if you already have the position as text, recognition is a
#' step to skip rather than endure.
#'
#' Four-field FENs are accepted. That is not laxity - it is what chess websites
#' actually put on the clipboard, and chess.js fills in the castling, en
#' passant and clock fields itself.
#'
#' @param x A FEN string.
#' @return A one-row [tanmai_position] data frame.
#' @examples
#' read_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
#' @export
read_fen <- function(x) {
  if (!is.character(x) || length(x) != 1L || !nzchar(trimws(x))) {
    stop("`x` must be a single non-empty FEN string.", call. = FALSE)
  }
  x <- fen_complete(trimws(x))
  check <- validate_fen(chess_ctx(), x)
  if (!isTRUE(check$ok)) {
    # chess.js names the offending field. That is far more use than a generic
    # "invalid FEN", so it is passed through rather than replaced.
    stop("Not a readable FEN: ", check$error, call. = FALSE)
  }
  new_position(
    fen = x, turn = fen_turn(x), source = "fen",
    orientation = if (identical(fen_turn(x), "b")) "black" else "white"
  )
}

#' Read a position from a screenshot of a board
#'
#' Crops the board out of the screenshot, works out which piece set it is drawn
#' in, reads the 64 squares, and decides which way round the board is.
#'
#' Unlike every other way of getting a position, this one can be wrong, so it
#' reports how sure it is. `confident` is `FALSE` when the art does not match
#' any installed piece set well enough to trust - typically a Chess.com or
#' custom theme. The `fen` is still returned in that case, because a
#' best guess you can correct beats no answer at all, but it is flagged rather
#' than presented as fact.
#'
#' @param image A screenshot: a file path, or a `magick-image`.
#' @param turn Side to move, `"w"` or `"b"`. A still image cannot show whose
#'   turn it is, so it has to be told.
#' @param orientation One of `"auto"`, `"white"` or `"black"`. `"auto"` decides
#'   from where the two armies sit, which is reliable for real positions.
#' @param autocrop Trim the surrounding page before splitting. Leave on unless
#'   the screenshot is already cropped tight to the board.
#' @param piece_sets Template libraries to match against. Defaults to every set
#'   installed locally.
#' @return A one-row [tanmai_position] data frame.
#' @examples
#' # Render a board and read it straight back.
#' board <- render_position(fen_to_symbols(
#'   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
#' ))
#' read_board(board)
#' @export
read_board <- function(image, turn = c("w", "b"), orientation = c("auto", "white", "black"),
                       autocrop = TRUE, piece_sets = NULL) {
  turn <- match.arg(turn)
  orientation <- match.arg(orientation)
  if (is.character(image)) {
    if (!file.exists(image)) stop("No such file: ", image, call. = FALSE)
    image <- magick::image_read(image)
  }
  if (!inherits(image, "magick-image")) {
    stop("`image` must be a file path or a magick image.", call. = FALSE)
  }
  libs <- piece_sets %||% load_all_template_libraries()
  res <- recognize_position(
    image, libs, chess_ctx(),
    turn = turn, autocrop = autocrop, orientation = orientation
  )
  new_position(
    fen = res$fen, turn = turn, source = "screenshot",
    orientation = if (res$flip) "black" else "white",
    piece_set = res$set, confident = res$set_confident,
    margin = res$set_margin, median_occ = res$set_median_occ,
    valid = res$valid, orientation_source = res$flip_source
  )
}

#' Read a game from PGN
#'
#' Returns one row per ply - the shape everything else in the package reads.
#' Comments, annotation glyphs and variations are tolerated, and a `[FEN]`
#' header is honoured, so a study or an endgame is not silently replayed from
#' the standard opening position.
#'
#' The evaluation columns (`cp`, `cp_loss`, `best_uci`, `best_san`, `class`)
#' come back as `NA`. Filling them needs an engine and is [evaluate()]'s job.
#'
#' @param x PGN text, or a path to a `.pgn` file.
#' @return A [tanmai_game] data frame, one row per ply, with the PGN's header
#'   tags attached as a `headers` attribute.
#' @examples
#' game <- read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")
#' game[, c("ply", "side", "san", "uci")]
#' @export
read_pgn <- function(x) {
  if (!is.character(x) || !length(x)) {
    stop("`x` must be PGN text or a path to a .pgn file.", call. = FALSE)
  }
  # A path and a one-line PGN are both character vectors of length one, so the
  # cheapest reliable discriminator is whether the file exists.
  if (length(x) == 1L && !grepl("\n", x) && file.exists(x)) {
    x <- readLines(x, warn = FALSE)
  }
  res <- game_from_pgn(chess_ctx(), x)
  if (!is.null(res$error)) stop("Could not read that PGN: ", res$error, call. = FALSE)

  game_to_frame(res$game, headers = res$headers)
}

#' Turn an internal game record into the public game data frame
#'
#' Shared by every adapter that produces a game, so a game watched frame by
#' frame off a video and a game read out of a PGN file are the same object -
#' which is what lets [evaluate()], [accuracy()] and the rest be written once.
#'
#' @param g An internal game record (see [game_new()]).
#' @param headers Optional named character vector of PGN header tags.
#' @return A [tanmai_game] data frame.
#' @noRd
game_to_frame <- function(g, headers = NULL) {
  n <- length(g$ucis)
  if (!n) {
    # A game with no moves is a legitimate answer - a recording where nothing
    # was ever readable, say - and must come back as an empty frame of the
    # right shape rather than as an error or a one-row frame of NA.
    empty <- data.frame(
      ply = integer(0), move_no = integer(0), side = character(0),
      san = character(0), uci = character(0),
      fen_before = character(0), fen_after = character(0),
      cp = numeric(0), cp_loss = numeric(0),
      best_uci = character(0), best_san = character(0),
      class = factor(character(0), levels = MOVE_CLASSES),
      human_p = numeric(0), stringsAsFactors = FALSE
    )
    return(structure(empty, headers = headers, class = c("tanmai_game", "data.frame")))
  }
  # fens is always one longer than ucis: it carries the position *before* each
  # move plus the final one. Splitting it into before/after here means no
  # consumer has to remember that.
  out <- data.frame(
    ply = seq_len(n),
    move_no = (seq_len(n) + 1L) %/% 2L,
    # A game that starts from a set-up position need not start with White to
    # move, so this is derived from the position rather than assumed.
    side = vapply(g$fens[seq_len(n)], fen_turn, character(1), USE.NAMES = FALSE),
    san = g$sans,
    uci = g$ucis,
    fen_before = g$fens[seq_len(n)],
    fen_after = g$fens[-1L],
    cp = NA_real_,
    cp_loss = NA_real_,
    best_uci = NA_character_,
    best_san = NA_character_,
    class = factor(NA_character_, levels = MOVE_CLASSES),
    human_p = NA_real_,
    stringsAsFactors = FALSE
  )
  structure(out, headers = headers, class = c("tanmai_game", "data.frame"))
}

#' Expand a FEN placement field into 64 symbols
#'
#' @param placement A FEN placement field (the part before the first space).
#' @return A character vector of 64 piece symbols, `"."` for an empty square,
#'   in image order (top row first).
#' @examples
#' fen_to_symbols("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")[1:8]
#' @export
fen_to_symbols <- function(placement) {
  placement <- sub(" .*$", "", placement)
  rows <- strsplit(placement, "/", fixed = TRUE)[[1]]
  if (length(rows) != 8L) {
    stop("A FEN placement field needs 8 ranks, got ", length(rows), ".", call. = FALSE)
  }
  out <- character(0)
  for (row in rows) {
    for (ch in strsplit(row, "", fixed = TRUE)[[1]]) {
      out <- if (grepl("[1-8]", ch)) c(out, rep(".", as.integer(ch))) else c(out, ch)
    }
  }
  if (length(out) != 64L) {
    stop("That placement field describes ", length(out), " squares, not 64.", call. = FALSE)
  }
  out
}
