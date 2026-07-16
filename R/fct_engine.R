#' Stockfish engine: download + UCI communication (port of chessvision/engine.py)
#' @noRd

STOCKFISH_ASSETS_WINDOWS <- c(
  "https://github.com/official-stockfish/Stockfish/releases/download/sf_17/stockfish-windows-x86-64.zip",
  "https://github.com/official-stockfish/Stockfish/releases/download/sf_16.1/stockfish-windows-x86-64.zip"
)
STOCKFISH_ASSETS_LINUX <- c(
  "https://github.com/official-stockfish/Stockfish/releases/download/sf_17/stockfish-ubuntu-x86-64.tar"
)
STOCKFISH_ASSETS_MAC <- c(
  "https://github.com/official-stockfish/Stockfish/releases/download/sf_17/stockfish-macos-m1-apple-silicon.tar"
)

#' @noRd
stockfish_bin_dir <- function() {
  dir <- tools::R_user_dir("chessvision", "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' @noRd
find_local_engine <- function() {
  dir <- stockfish_bin_dir()
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  files <- files[grepl("stockfish", basename(files), ignore.case = TRUE)]
  if (.Platform$OS.type == "windows") {
    files <- files[grepl("\\.exe$", files, ignore.case = TRUE)]
  } else {
    files <- files[!grepl("\\.", basename(files))] # extension-less unix binary
  }
  if (length(files) == 0) return(NULL)
  files[1]
}

#' Return a path to a Stockfish binary, downloading it on first use.
#' @noRd
ensure_stockfish <- function() {
  local <- find_local_engine()
  if (!is.null(local)) return(local)

  dir <- stockfish_bin_dir()
  sysname <- Sys.info()[["sysname"]]
  assets <- if (.Platform$OS.type == "windows") {
    STOCKFISH_ASSETS_WINDOWS
  } else if (sysname == "Darwin") {
    STOCKFISH_ASSETS_MAC
  } else {
    STOCKFISH_ASSETS_LINUX
  }

  last_err <- NULL
  for (url in assets) {
    archive <- file.path(dir, basename(url))
    result <- tryCatch(
      {
        utils::download.file(url, archive, mode = "wb", quiet = TRUE)
        if (grepl("\\.zip$", archive)) {
          utils::unzip(archive, exdir = dir)
        } else {
          utils::untar(archive, exdir = dir)
        }
        file.remove(archive)
        find_local_engine()
      },
      error = function(e) {
        last_err <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(result)) {
      if (.Platform$OS.type != "windows") Sys.chmod(result, "0755")
      return(result)
    }
  }
  stop("Could not obtain a Stockfish binary. Last error: ", last_err)
}

#' @noRd
wait_for_line <- function(p, target, timeout = 10) {
  deadline <- Sys.time() + timeout
  seen <- c()
  repeat {
    if (Sys.time() > deadline) {
      stop(sprintf("Timed out waiting for '%s' from engine. Saw: %s",
                    target, paste(seen, collapse = " | ")))
    }
    p$poll_io(200)
    out <- p$read_output_lines()
    if (length(out) > 0) seen <- c(seen, out)
    hit <- out[startsWith(out, target)]
    if (length(hit) > 0) return(hit[1])
  }
}

#' @noRd
parse_uci_score <- function(line, current) {
  toks <- strsplit(line, "\\s+")[[1]]
  idx <- which(toks == "score")
  if (length(idx) == 0) return(current)
  kind <- toks[idx + 1]
  val <- suppressWarnings(as.integer(toks[idx + 2]))
  if (is.na(val)) return(current)
  if (kind == "cp") list(cp = val, mate = NA_integer_) else list(cp = NA_integer_, mate = val)
}

#' Ask Stockfish for the best move in a position.
#'
#' @param fen full FEN string
#' @param movetime_ms engine thinking time in milliseconds
#' @return list(move, ponder, score_cp, score_mate) - move/ponder are UCI
#'   strings (e.g. "e2e4"), score is from the side-to-move's point of view.
#' @noRd
best_move_uci <- function(fen, movetime_ms = 1000L, engine_path = NULL) {
  if (is.null(engine_path)) engine_path <- ensure_stockfish()
  p <- processx::process$new(engine_path, stdin = "|", stdout = "|", stderr = "|")
  on.exit(try(p$kill(), silent = TRUE), add = TRUE)

  p$write_input("uci\n")
  wait_for_line(p, "uciok")
  p$write_input("isready\n")
  wait_for_line(p, "readyok")
  p$write_input(sprintf("position fen %s\n", fen))
  p$write_input(sprintf("go movetime %d\n", as.integer(movetime_ms)))

  score <- list(cp = NA_integer_, mate = NA_integer_)
  bestmove <- NULL
  ponder <- NULL
  deadline <- Sys.time() + (movetime_ms / 1000 + 15)
  repeat {
    if (Sys.time() > deadline) stop("Engine timed out computing a move")
    p$poll_io(200)
    lines <- p$read_output_lines()
    for (line in lines) {
      if (startsWith(line, "info")) {
        score <- parse_uci_score(line, score)
      } else if (startsWith(line, "bestmove")) {
        toks <- strsplit(line, "\\s+")[[1]]
        bestmove <- toks[2]
        if (length(toks) >= 4 && toks[3] == "ponder") ponder <- toks[4]
      }
    }
    if (!is.null(bestmove)) break
  }

  list(move = bestmove, ponder = ponder, score_cp = score$cp, score_mate = score$mate)
}
