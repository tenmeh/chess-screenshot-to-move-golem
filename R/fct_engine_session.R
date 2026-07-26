# Persistent Stockfish session for the interactive board.
#
# The one-shot best_move_uci() in fct_engine.R spawns a process per query, which
# is fine for a single "Analyze" click but far too slow for live evaluation.
# Here one engine process is kept alive for the whole Shiny session, searching
# with `go infinite` and MultiPV while the UI drains its output non-blockingly.

#' Start a persistent Stockfish process
#'
#' @param multipv Number of principal variations to report.
#' @param engine_path Optional path to a Stockfish binary (defaults to the
#'   cached/downloaded one via [ensure_stockfish()]).
#' @return An engine session list with the process handle and mutable state,
#'   or `NULL` if no engine binary could be obtained.
engine_session_start <- function(multipv = 3L, engine_path = NULL) {
  path <- tryCatch(
    if (is.null(engine_path)) ensure_stockfish() else engine_path,
    error = function(e) NULL
  )
  if (is.null(path)) {
    return(NULL)
  }
  p <- tryCatch(
    processx::process$new(path, stdin = "|", stdout = "|", stderr = "|"),
    error = function(e) NULL
  )
  if (is.null(p)) {
    return(NULL)
  }

  p$write_input("uci\n")
  ok <- tryCatch(
    {
      wait_for_line(p, "uciok", timeout = 20)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    try(p$kill(), silent = TRUE)
    return(NULL)
  }
  p$write_input(sprintf("setoption name MultiPV value %d\n", as.integer(multipv)))
  p$write_input("isready\n")
  tryCatch(wait_for_line(p, "readyok", timeout = 20), error = function(e) NULL)

  list(
    process = p,
    multipv = as.integer(multipv),
    # `state` is an environment so callers can mutate it without reassignment.
    state = new.env(parent = emptyenv())
  )
}

#' Stop a persistent engine session
#'
#' @param sess An engine session from [engine_session_start()].
#' @return Invisibly `NULL`; kills the process if it is still alive.
engine_session_stop <- function(sess) {
  if (!is.null(sess) && !is.null(sess$process)) {
    try(sess$process$kill(), silent = TRUE)
  }
  invisible(NULL)
}

#' Point a persistent engine at a position and start an infinite search
#'
#' Any in-flight search is stopped first and its leftover output drained, so
#' stale `info` lines can't be mistaken for results for the new position.
#'
#' @param sess An engine session from [engine_session_start()].
#' @param fen The FEN to analyse.
#' @return Invisibly `TRUE` when the search was (re)started.
engine_session_analyse <- function(sess, fen) {
  if (is.null(sess)) {
    return(invisible(FALSE))
  }
  p <- sess$process
  if (!p$is_alive()) {
    return(invisible(FALSE))
  }

  # Stop the previous search, then synchronise with isready/readyok: the engine
  # answers readyok only after it has finished emitting everything for the old
  # search, so draining up to that point guarantees no stale `info` line can
  # land in the new position's results.
  p$write_input("stop\n")
  p$write_input("isready\n")
  deadline <- Sys.time() + 2
  repeat {
    p$poll_io(50)
    out <- p$read_output_lines()
    if (any(startsWith(out, "readyok")) || Sys.time() > deadline) break
  }

  sess$state$fen <- fen
  sess$state$depth <- 0L
  sess$state$lines <- list()
  sess$state$bestmove <- NULL

  p$write_input(sprintf("position fen %s\n", fen))
  p$write_input("go infinite\n")
  invisible(TRUE)
}

#' Parse one UCI info line into a principal-variation record
#'
#' @param line A UCI "info ..." line.
#' @return A list with `multipv`, `depth`, `cp`, `mate`, `pv` (UCI moves) and
#'   `first` (first move of the PV), or `NULL` if the line has no PV.
parse_info_line <- function(line) {
  toks <- strsplit(trimws(line), "\\s+")[[1]]
  grab_int <- function(key) {
    i <- which(toks == key)
    if (!length(i) || i[1] + 1 > length(toks)) {
      return(NA_integer_)
    }
    suppressWarnings(as.integer(toks[i[1] + 1]))
  }
  pv_i <- which(toks == "pv")
  if (!length(pv_i)) {
    return(NULL)
  }
  pv <- toks[(pv_i[1] + 1):length(toks)]
  if (!length(pv)) {
    return(NULL)
  }

  cp <- NA_integer_
  mate <- NA_integer_
  s <- which(toks == "score")
  if (length(s) && s[1] + 2 <= length(toks)) {
    kind <- toks[s[1] + 1]
    val <- suppressWarnings(as.integer(toks[s[1] + 2]))
    if (identical(kind, "cp")) cp <- val else if (identical(kind, "mate")) mate <- val
  }

  mpv <- grab_int("multipv")
  list(
    multipv = if (is.na(mpv)) 1L else mpv,
    depth = grab_int("depth"),
    cp = cp,
    mate = mate,
    pv = pv,
    first = pv[1]
  )
}

#' Drain pending engine output and fold it into the session state
#'
#' Non-blocking: reads whatever is buffered and returns immediately, so it is
#' safe to call from a Shiny observer on a timer.
#'
#' @param sess An engine session from [engine_session_start()].
#' @return A list with `depth`, `lines` (list of PV records ordered by
#'   multipv), `fen` and `bestmove`, or `NULL` if there is no live session.
engine_session_poll <- function(sess) {
  if (is.null(sess) || !sess$process$is_alive()) {
    return(NULL)
  }
  p <- sess$process
  p$poll_io(0)
  out <- p$read_output_lines()
  for (line in out) {
    if (startsWith(line, "info")) {
      rec <- parse_info_line(line)
      if (is.null(rec) || is.na(rec$depth)) next
      # Keyed by multipv so a deeper iteration replaces the shallower one.
      sess$state$lines[[as.character(rec$multipv)]] <- rec
      if (rec$depth > (sess$state$depth %||% 0L)) sess$state$depth <- rec$depth
    } else if (startsWith(line, "bestmove")) {
      toks <- strsplit(line, "\\s+")[[1]]
      sess$state$bestmove <- toks[2]
    }
  }
  keys <- names(sess$state$lines)
  ordered <- if (length(keys)) {
    sess$state$lines[order(as.integer(keys))]
  } else {
    list()
  }
  list(
    depth = sess$state$depth %||% 0L,
    lines = ordered,
    fen = sess$state$fen,
    bestmove = sess$state$bestmove
  )
}

#' Null-coalescing helper
#'
#' Base R gained `%||%` in 4.4.0; this local copy keeps the package working on
#' older releases. Deliberately not documented into an .Rd file: roxygen
#' derives the topic name from the operator, which is not a legal \\name.
#'
#' @param a A value that may be `NULL`.
#' @param b Fallback used when `a` is `NULL`.
#' @return `a` unless it is `NULL`, in which case `b`.
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Format an engine score for display
#'
#' @param cp Centipawn score (may be `NA`).
#' @param mate Moves-to-mate score (may be `NA`).
#' @param turn Side to move, "w" or "b"; scores are converted to White's point
#'   of view so the eval bar has a fixed meaning.
#' @return A short string such as "+1.24" or "M3", or "-" when unscored.
format_score <- function(cp, mate, turn = "w") {
  sign <- if (identical(turn, "b")) -1 else 1
  if (!is.na(mate)) {
    m <- mate * sign
    return(sprintf("M%s%d", if (m < 0) "-" else "", abs(m)))
  }
  if (!is.na(cp)) {
    return(sprintf("%+.2f", (cp * sign) / 100))
  }
  "-"
}

#' Convert a score to a 0-100 eval-bar percentage (White's share)
#'
#' @param cp Centipawn score (may be `NA`).
#' @param mate Moves-to-mate score (may be `NA`).
#' @param turn Side to move, "w" or "b".
#' @return A number in 0-100 giving White's share of the bar.
eval_bar_pct <- function(cp, mate, turn = "w") {
  sign <- if (identical(turn, "b")) -1 else 1
  if (!is.na(mate)) {
    return(if (mate * sign > 0) 100 else 0)
  }
  if (is.na(cp)) {
    return(50)
  }
  # Logistic squash: same shape Lichess uses, so small edges stay legible.
  x <- (cp * sign) / 100
  100 / (1 + exp(-0.4 * x))
}
