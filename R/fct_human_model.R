# Human move models: what a player of a given strength is likely to play,
# as opposed to what is objectively best.
#
# The implementation is Maia (CSSLab), a network trained to predict human moves
# at a target rating rather than to win. It is run through lc0 with search
# switched off (`nodes 1`), so a query is a single neural-net forward pass and
# returns the policy head directly - the probability the modelled human plays
# each legal move. That is milliseconds per position, which is what makes
# scoring every candidate move affordable.
#
# Everything downstream depends only on `human_move_probabilities()`, so the
# model behind it can be swapped without touching the Blunder Radar metrics.

MAIA_RATINGS <- c(1100L, 1500L, 1900L)

LC0_ASSETS_WINDOWS <- c(
  "https://github.com/LeelaChessZero/lc0/releases/download/v0.32.1/lc0-v0.32.1-windows-cpu-dnnl.zip",
  "https://github.com/LeelaChessZero/lc0/releases/download/v0.32.1/lc0-v0.32.1-windows-cpu-openblas.zip"
)
MAIA_WEIGHTS_BASE <-
  "https://github.com/CSSLab/maia-chess/raw/master/maia_weights"

#' Directory holding lc0 and the Maia weight files
#'
#' @return The cache directory path (created if needed).
maia_dir <- function() {
  dir <- file.path(tools::R_user_dir("chessvision", "cache"), "maia")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Locate the lc0 binary
#'
#' Checks an explicit override, then PATH, then the usual install locations and
#' the download cache. There is no official Linux release of lc0, so container
#' images build it from source and put it on PATH.
#'
#' @return Path to lc0, or `NULL` if it is not available.
find_lc0 <- function() {
  configured <- Sys.getenv("CHESSVISION_LC0", "")
  if (nzchar(configured) && file.exists(configured)) {
    return(configured)
  }
  on_path <- Sys.which("lc0")
  if (nzchar(on_path)) {
    return(unname(on_path))
  }
  for (candidate in c("/usr/local/bin/lc0", "/usr/bin/lc0", "/opt/lc0/lc0")) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }
  pattern <- if (.Platform$OS.type == "windows") "^lc0\\.exe$" else "^lc0$"
  hits <- list.files(maia_dir(), pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(hits)) hits[1] else NULL
}

#' Path to a Maia weights file for a rating
#'
#' @param rating One of 1100, 1500 or 1900.
#' @return The expected weights path (which may not exist yet).
maia_weights_path <- function(rating = 1500L) {
  rating <- match_maia_rating(rating)
  file.path(maia_dir(), sprintf("maia-%d.pb.gz", rating))
}

#' Snap a requested rating to the nearest available Maia network
#'
#' @param rating A numeric rating.
#' @return The closest rating for which a Maia network exists (1100, 1500 or
#'   1900); 1500 when `rating` is missing.
match_maia_rating <- function(rating) {
  rating <- suppressWarnings(as.integer(rating))
  if (is.na(rating)) {
    return(1500L)
  }
  MAIA_RATINGS[which.min(abs(MAIA_RATINGS - rating))]
}

#' Download lc0 and the Maia weights if they are not already present
#'
#' Intended for local development; deployed images bake these in.
#'
#' @param ratings Ratings whose weights should be fetched.
#' @return `TRUE` if lc0 and at least one weights file are available.
ensure_maia <- function(ratings = MAIA_RATINGS) {
  for (rating in ratings) {
    dest <- maia_weights_path(rating)
    if (!file.exists(dest)) {
      tryCatch(
        utils::download.file(
          sprintf("%s/maia-%d.pb.gz", MAIA_WEIGHTS_BASE, match_maia_rating(rating)),
          dest,
          mode = "wb", quiet = TRUE
        ),
        error = function(e) NULL
      )
    }
  }

  if (is.null(find_lc0()) && .Platform$OS.type == "windows") {
    for (url in LC0_ASSETS_WINDOWS) {
      archive <- file.path(maia_dir(), basename(url))
      ok <- tryCatch(
        {
          utils::download.file(url, archive, mode = "wb", quiet = TRUE)
          utils::unzip(archive, exdir = file.path(maia_dir(), "lc0"))
          file.remove(archive)
          TRUE
        },
        error = function(e) FALSE
      )
      if (ok && !is.null(find_lc0())) break
    }
  }

  !is.null(find_lc0()) && any(file.exists(vapply(ratings, maia_weights_path, character(1))))
}

#' Is a human model usable right now?
#'
#' @param rating Desired rating.
#' @return `TRUE` when both lc0 and the weights for `rating` are present.
human_model_available <- function(rating = 1500L) {
  !is.null(find_lc0()) && file.exists(maia_weights_path(rating))
}

#' Start a persistent Maia session
#'
#' Loading the network costs a second or two, so the process is kept alive and
#' reused; each subsequent query is a single forward pass.
#'
#' @param rating Target rating; snapped to the nearest available network.
#' @return A session list, or `NULL` if lc0 or the weights are missing.
maia_session_start <- function(rating = 1500L) {
  rating <- match_maia_rating(rating)
  exe <- find_lc0()
  weights <- maia_weights_path(rating)
  if (is.null(exe) || !file.exists(weights)) {
    return(NULL)
  }

  p <- tryCatch(
    processx::process$new(exe, stdin = "|", stdout = "|", stderr = "|"),
    error = function(e) NULL
  )
  if (is.null(p)) {
    return(NULL)
  }

  p$write_input("uci\n")
  ok <- tryCatch(
    {
      wait_for_line(p, "uciok", timeout = 30)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    try(p$kill(), silent = TRUE)
    return(NULL)
  }

  p$write_input(sprintf("setoption name WeightsFile value %s\n", normalizePath(weights)))
  # Report the policy for every legal move, not just the chosen one.
  p$write_input("setoption name VerboseMoveStats value true\n")
  # lc0's default temperature (1.359) flattens the distribution; 1.0 is the
  # network's own calibrated output, i.e. the actual probability a human of
  # this rating plays the move.
  p$write_input("setoption name PolicyTemperature value 1.0\n")
  p$write_input("isready\n")
  tryCatch(wait_for_line(p, "readyok", timeout = 30), error = function(e) NULL)

  list(process = p, rating = rating)
}

#' Stop a Maia session
#'
#' @param sess A session from [maia_session_start()].
#' @return Invisibly `NULL`.
maia_session_stop <- function(sess) {
  if (!is.null(sess) && !is.null(sess$process)) {
    try(sess$process$kill(), silent = TRUE)
  }
  invisible(NULL)
}

#' Parse lc0's verbose move statistics into per-move probabilities
#'
#' @param lines Output lines from a completed `go nodes 1` search.
#' @return A data frame of `move` (UCI) and `prob`, ordered by decreasing
#'   probability.
parse_policy_lines <- function(lines) {
  keep <- grepl("^info string [a-h][1-8][a-h][1-8]", lines)
  if (!any(keep)) {
    return(data.frame(move = character(0), prob = numeric(0)))
  }
  pol <- lines[keep]
  move <- sub("^info string ([a-h][1-8][a-h][1-8][qrbnQRBN]?).*$", "\\1", pol)
  prob <- suppressWarnings(
    as.numeric(sub(".*\\(P: *([0-9.]+)%\\).*", "\\1", pol)) / 100
  )
  ok <- !is.na(prob)
  out <- data.frame(move = move[ok], prob = prob[ok], stringsAsFactors = FALSE)
  out[order(-out$prob), ]
}

#' Probability a modelled human plays each legal move
#'
#' @param sess A session from [maia_session_start()].
#' @param fen Position to evaluate.
#' @return A data frame of `move` and `prob`, ordered by decreasing
#'   probability; zero rows if the query fails.
human_move_probabilities <- function(sess, fen) {
  empty <- data.frame(move = character(0), prob = numeric(0))
  if (is.null(sess) || !sess$process$is_alive()) {
    return(empty)
  }
  p <- sess$process
  p$poll_io(0)
  invisible(p$read_output_lines()) # drop anything left over

  p$write_input(sprintf("position fen %s\n", fen))
  p$write_input("go nodes 1\n")

  lines <- character(0)
  deadline <- Sys.time() + 10
  repeat {
    if (Sys.time() > deadline) {
      return(empty)
    }
    p$poll_io(100)
    out <- p$read_output_lines()
    if (length(out)) lines <- c(lines, out)
    if (any(startsWith(out, "bestmove"))) break
  }
  parse_policy_lines(lines)
}
