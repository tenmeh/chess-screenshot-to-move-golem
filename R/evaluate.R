# Filling in the engine columns.
#
# Reading a game never needs an engine; understanding one does. Everything here
# is therefore optional at every level: the package installs without Stockfish,
# loads without it, and reads positions and games without it. Only these
# functions require it, and they say so plainly rather than failing obscurely.
#
# One call per *position*, not two per move. `engine_session_eval()` returns the
# evaluation and the best move together, and the position after move i is the
# position before move i+1, so a game of n moves costs n+1 searches rather than
# the 2n a naive "score the move, then score the alternative" loop would use.

# Checkmate, on the same scale the engine uses for its own mate scores, so a
# mate outranks any amount of material. Negative for the side to move: the
# player with no legal reply is the one who has lost.
MATE_CP <- 10000

#' Is a chess engine available?
#'
#' Everything that reads a position works without one. Everything that judges a
#' position needs one.
#'
#' @return `TRUE` if a Stockfish binary can be found.
#' @examples
#' has_engine()
#' @export
has_engine <- function() !is.null(find_local_engine())

engine_required <- function() {
  if (!has_engine()) {
    stop(
      "No chess engine found. Install Stockfish and make sure it is on your ",
      "PATH, or set TANMAI_STOCKFISH to its location. Reading positions and ",
      "games does not need one; evaluating them does.",
      call. = FALSE
    )
  }
}

#' Evaluate a position or a game with a chess engine
#'
#' For a game this fills in the columns [read_pgn()] leaves empty: `cp`,
#' `cp_loss`, `best_uci`, `best_san` and `class`. For a position it returns the
#' evaluation and the best move.
#'
#' `cp` is always from **White's** point of view, so the sign means the same
#' thing down the whole column and the evaluation graph does not flip every
#' ply. `cp_loss` is from the **mover's**, because "you lost 3 pawns" is only
#' meaningful that way for both colours.
#'
#' @param x A [tanmai_game] or [tanmai_position].
#' @param movetime Milliseconds of thinking time per position. The default is
#'   deliberately small: a game is many positions, and doubling this doubles the
#'   wait.
#' @param engine_path Path to a Stockfish binary. Found automatically if `NULL`.
#' @param verbose Report progress while working through a long game.
#' @param ... Unused.
#' @return `x` with the engine columns filled in.
#' @examples
#' \donttest{
#' if (has_engine()) {
#'   game <- evaluate(read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6"))
#'   game[, c("san", "cp", "cp_loss", "class")]
#' }
#' }
#' @export
evaluate <- function(x, ...) UseMethod("evaluate")

#' @rdname evaluate
#' @export
evaluate.tanmai_game <- function(x, movetime = 300L, engine_path = NULL,
                                 verbose = FALSE, ...) {
  engine_required()
  n <- nrow(x)
  if (!n) {
    return(x)
  }

  # Every position in the game: the one before each move, plus the final one.
  fens <- c(x$fen_before, x$fen_after[n])

  sess <- engine_session_start(multipv = 1L, engine_path = engine_path)
  if (is.null(sess)) stop("Could not start the engine.", call. = FALSE)
  on.exit(engine_session_stop(sess), add = TRUE)

  ctx <- chess_ctx()
  cp_stm <- rep(NA_real_, length(fens))
  best <- rep(NA_character_, length(fens))
  for (i in seq_along(fens)) {
    if (verbose && i %% 10L == 0L) {
      message("  evaluated ", i, "/", length(fens), " positions")
    }
    # A finished position has no move to search, so the engine reports no
    # principal variation and would leave this NA - which would then propagate
    # into cp_loss and class and quietly blank out the most important ply in
    # the game, the one that ended it. A mate is not an unknown evaluation.
    status <- position_status(ctx, fens[i])
    if (!identical(status, "ongoing")) {
      cp_stm[i] <- if (identical(status, "checkmate")) -MATE_CP else 0
      next
    }
    res <- engine_session_eval(sess, fens[i], movetime_ms = movetime)
    if (is.null(res)) next
    cp_stm[i] <- res$cp
    best[i] <- res$best %||% NA_character_
  }

  # The engine answers from the side to move's point of view; the column is
  # defined from White's. Convert once, here, so nothing downstream has to
  # remember which convention it is holding.
  turns <- vapply(fens, fen_turn, character(1), USE.NAMES = FALSE)
  cp_white <- ifelse(turns == "b", -cp_stm, cp_stm)

  x$cp <- cp_white[-1L] # the evaluation *after* each move
  x$best_uci <- best[seq_len(n)] # the best move in the position before it

  # Loss is measured from the mover's point of view: how much the evaluation
  # moved against them.
  mover_sign <- ifelse(x$side == "b", -1, 1)
  x$cp_loss <- pmax(0, (cp_white[seq_len(n)] - cp_white[-1L]) * mover_sign)

  x$best_san <- vapply(seq_len(n), function(i) {
    if (is.na(x$best_uci[i])) {
      return(NA_character_)
    }
    san <- tryCatch(fen_move_to_san(ctx, x$fen_before[i], x$best_uci[i]),
      error = function(e) NA_character_
    )
    if (is.null(san) || !length(san)) NA_character_ else as.character(san)
  }, character(1))

  x$class <- classify_move(x$cp_loss, played = x$uci, best = x$best_uci)
  x
}

#' @rdname evaluate
#' @export
evaluate.tanmai_position <- function(x, movetime = 1000L, engine_path = NULL, ...) {
  engine_required()
  sess <- engine_session_start(multipv = 1L, engine_path = engine_path)
  if (is.null(sess)) stop("Could not start the engine.", call. = FALSE)
  on.exit(engine_session_stop(sess), add = TRUE)

  res <- engine_session_eval(sess, x$fen, movetime_ms = movetime)
  if (is.null(res)) {
    x$cp <- NA_real_
    x$best_uci <- NA_character_
    return(x)
  }
  x$cp <- if (identical(x$turn, "b")) -res$cp else res$cp
  x$best_uci <- res$best %||% NA_character_
  x$best_san <- tryCatch(fen_move_to_san(chess_ctx(), x$fen, x$best_uci),
    error = function(e) NA_character_
  )
  x
}

#' Classify how good each move was
#'
#' Thresholds follow the convention players already know from Lichess and
#' Chess.com, so "blunder" here means what it means everywhere else. The one
#' addition is `best`: a move that *is* the engine's choice is worth
#' distinguishing from one that merely lost nothing measurable.
#'
#' @param cp_loss Centipawns lost, from the mover's point of view.
#' @param played,best UCI moves actually played and preferred.
#' @return A factor with levels best / good / inaccuracy / mistake / blunder.
#' @noRd
classify_move <- function(cp_loss, played, best) {
  out <- rep(NA_character_, length(cp_loss))
  known <- !is.na(cp_loss)
  out[known] <- "good"
  out[known & cp_loss >= 50] <- "inaccuracy"
  out[known & cp_loss >= 100] <- "mistake"
  out[known & cp_loss >= 200] <- "blunder"
  matched <- known & !is.na(played) & !is.na(best) & played == best
  out[matched] <- "best"
  factor(out, levels = MOVE_CLASSES)
}

#' How accurately each player played
#'
#' Returns one row per side: how many moves they made, their average
#' centipawn loss, how many inaccuracies, mistakes and blunders, and an
#' accuracy percentage.
#'
#' The accuracy figure uses Lichess's published formula, so it is comparable
#' with the number they show rather than being a private invention. It works by
#' converting each evaluation to an expected win percentage first - which is
#' the point, because losing half a pawn matters enormously in a level position
#' and not at all when you are already winning by a rook.
#'
#' @param game A [tanmai_game] that has been through [evaluate()].
#' @return A data frame with one row per side.
#' @examples
#' \donttest{
#' if (has_engine()) {
#'   accuracy(evaluate(read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")))
#' }
#' }
#' @export
accuracy <- function(game) {
  if (!inherits(game, "tanmai_game")) {
    stop("`game` must be a tanmai_game, as returned by read_pgn().", call. = FALSE)
  }
  if (all(is.na(game$cp))) {
    stop("This game has not been evaluated. Run evaluate() on it first.", call. = FALSE)
  }

  # Evaluation -> expected win percentage for the side to move.
  win_pct <- function(cp) 50 + 50 * (2 / (1 + exp(-0.00368208 * cp)) - 1)

  n <- nrow(game)
  mover_sign <- ifelse(game$side == "b", -1, 1)
  cp_before <- c(NA_real_, game$cp[-n])
  # The first move has no "before" in the table, so take it from fen_before.
  cp_before[1] <- game$cp[1] + game$cp_loss[1] * mover_sign[1]

  before <- win_pct(cp_before * mover_sign)
  after <- win_pct(game$cp * mover_sign)
  drop <- pmax(0, before - after)
  per_move <- 103.1668 * exp(-0.04354 * drop) - 3.1669
  per_move <- pmin(100, pmax(0, per_move))

  sides <- c("w", "b")
  do.call(rbind, lapply(sides, function(s) {
    i <- game$side == s & !is.na(game$cp_loss)
    if (!any(i)) {
      return(NULL)
    }
    data.frame(
      side = s,
      moves = sum(i),
      acpl = round(mean(game$cp_loss[i]), 1),
      accuracy = round(mean(per_move[i], na.rm = TRUE), 1),
      inaccuracies = sum(game$class[i] == "inaccuracy", na.rm = TRUE),
      mistakes = sum(game$class[i] == "mistake", na.rm = TRUE),
      blunders = sum(game$class[i] == "blunder", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

#' Where the game turned
#'
#' The moves that cost the most, worst first. A game is usually decided by two
#' or three of them, and reading a whole evaluation graph to find them is work
#' a function can do.
#'
#' @param game A [tanmai_game] that has been through [evaluate()].
#' @param min_loss Ignore moves that cost less than this, in centipawns.
#' @param n How many to return.
#' @return The rows of `game` that cost the most, worst first.
#' @examples
#' \donttest{
#' if (has_engine()) {
#'   turning_points(evaluate(read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")))
#' }
#' }
#' @export
turning_points <- function(game, min_loss = 100, n = 5L) {
  if (!inherits(game, "tanmai_game")) {
    stop("`game` must be a tanmai_game, as returned by read_pgn().", call. = FALSE)
  }
  if (all(is.na(game$cp_loss))) {
    stop("This game has not been evaluated. Run evaluate() on it first.", call. = FALSE)
  }
  hit <- which(!is.na(game$cp_loss) & game$cp_loss >= min_loss)
  if (!length(hit)) {
    return(game[0, ])
  }
  hit <- hit[order(-game$cp_loss[hit])]
  game[utils::head(hit, n), ]
}
