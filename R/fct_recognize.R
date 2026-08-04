# Template-based piece recognition (port of tanmai/recognize.py).
#
# Each square is matched in background-subtracted form: the background is
# estimated from the square's own four corners, then subtracted, so board
# themes, last-move highlights, and coordinate labels don't throw it off.

MARGIN <- 6L
CORNER <- MARGIN + 4L # 10
PIECE_SYMBOLS <- c("P", "N", "B", "R", "Q", "K", "p", "n", "b", "r", "q", "k")

# Confidence gates for auto-detection (empirically calibrated; see
# recognize_symbols_auto). Known sets clear both by a wide margin even after
# jpeg/scale/blur degradation; an unknown piece set fails both.
MARGIN_GATE <- 0.06 # winner-vs-runnerup separation (known >=0.12, unknown <0.01)
MIN_OCC_GATE <- 0.60 # worst occupied-square match (known >=0.88, unknown <=0.40)

#' Estimate a square's background level from its four corners
#'
#' @param mat A `SQUARE_PX` by `SQUARE_PX` grayscale square matrix.
#' @return The median corner value (the estimated background level).
sq_background <- function(mat) {
  n <- nrow(mat) # 64
  c <- CORNER
  corners <- c(
    as.vector(mat[1:c, 1:c]),
    as.vector(mat[(n - c + 1):n, 1:c]),
    as.vector(mat[1:c, (n - c + 1):n]),
    as.vector(mat[(n - c + 1):n, (n - c + 1):n])
  )
  stats::median(corners)
}

#' Background-subtracted central region of a square
#'
#' @param mat A `SQUARE_PX` by `SQUARE_PX` grayscale square matrix.
#' @return The inner region with the estimated background subtracted.
sq_core <- function(mat) {
  bg <- sq_background(mat)
  resid <- mat - bg
  n <- nrow(mat)
  resid[(MARGIN + 1):(n - MARGIN), (MARGIN + 1):(n - MARGIN)]
}

#' Mean absolute energy of a square core
#'
#' @param core A background-subtracted square core from [sq_core()].
#' @return The mean absolute value (used to decide empty vs occupied).
sq_energy <- function(core) mean(abs(core))

#' Zero-mean, unit-norm flatten of a matrix
#'
#' @param v A numeric matrix or vector.
#' @return A zero-mean vector scaled to unit norm (unchanged if near-constant).
normalize_vec <- function(v) {
  v <- as.vector(v) - mean(as.vector(v))
  nrm <- sqrt(sum(v^2))
  if (nrm > 1e-6) v / nrm else v
}

#' Load a template library from an RDS file
#'
#' @param path Path to a saved template library.
#' @return A list with `pieces`, `norm_pieces` (normalized templates) and
#'   `empty_threshold`.
load_template_library <- function(path) {
  raw <- readRDS(path)
  norm_pieces <- lapply(raw$pieces, normalize_vec)
  list(
    pieces = raw$pieces,
    norm_pieces = norm_pieces,
    empty_threshold = raw$empty_threshold
  )
}

#' Save a template library to an RDS file
#'
#' @param lib A template library (as built by [build_from_start_position()]).
#' @param path Destination path.
#' @return Invisibly, the result of [saveRDS()].
save_template_library <- function(lib, path) {
  saveRDS(list(pieces = lib$pieces, empty_threshold = lib$empty_threshold), path)
}

#' Classify one square against a single template library
#'
#' @param square_img A magick image of one square.
#' @param lib A template library from [load_template_library()].
#' @return A FEN piece symbol, or "." for an empty square.
classify_square <- function(square_img, lib) {
  mat <- to_gray_matrix(square_img)
  core <- sq_core(mat)
  if (sq_energy(core) < lib$empty_threshold) {
    return(".")
  }
  # Transposed for consistency with build_from_start_position(), which stores
  # templates the same way.
  probe <- normalize_vec(t(core))
  best_sym <- "."
  best_score <- -Inf
  for (sym in names(lib$norm_pieces)) {
    score <- sum(probe * lib$norm_pieces[[sym]])
    if (score > best_score) {
      best_score <- score
      best_sym <- sym
    }
  }
  best_sym
}

#' Classify all 64 squares against a single template library
#'
#' @param squares A list of 64 magick square images, row-major, top row first.
#' @param lib A template library from [load_template_library()].
#' @return A character vector of 64 piece symbols ("." for empty).
recognize_symbols <- function(squares, lib) {
  vapply(squares, classify_square, character(1), lib = lib)
}

#' Calibrate a template library from a starting-position board
#'
#' @param squares A list of 64 square images of the standard starting position
#'   (white on bottom, row-major top-first).
#' @return A template library list with `pieces` and `empty_threshold`.
build_from_start_position <- function(squares) {
  start_symbols <- START_SYMBOLS

  piece_cores <- setNames(vector("list", length(PIECE_SYMBOLS)), PIECE_SYMBOLS)
  empty_energies <- c()
  piece_energies <- c()

  for (i in seq_len(64)) {
    mat <- to_gray_matrix(squares[[i]])
    core <- sq_core(mat)
    sym <- start_symbols[i]
    if (sym == ".") {
      empty_energies <- c(empty_energies, sq_energy(core))
    } else {
      piece_cores[[sym]] <- c(piece_cores[[sym]], list(t(core))) # store as [y,x]
      piece_energies <- c(piece_energies, sq_energy(core))
    }
  }

  pieces <- list()
  for (sym in PIECE_SYMBOLS) {
    if (length(piece_cores[[sym]]) > 0) {
      arrs <- simplify2array(piece_cores[[sym]]) # dim (52,52,n)
      pieces[[sym]] <- apply(arrs, c(1, 2), mean)
    }
  }

  hi_empty <- if (length(empty_energies)) max(empty_energies) else 0
  lo_piece <- if (length(piece_energies)) min(piece_energies) else hi_empty + 1
  threshold <- (hi_empty + lo_piece) / 2

  list(pieces = pieces, empty_threshold = threshold)
}

# ---------------------------------------------------------------------------
# Multi-set recognition: score all 64 squares against every template of every
# available set in a single matrix multiply, pick the best-matching set, and
# classify with it.
# ---------------------------------------------------------------------------

#' Load every available template library
#'
#' @return A named list of template libraries, including the user's manual
#'   calibration as "custom" when present.
load_all_template_libraries <- function() {
  libs <- list()
  for (name in available_piece_sets()) {
    path <- set_templates_path(name)
    if (file.exists(path)) libs[[name]] <- load_template_library(path)
  }
  custom <- custom_templates_path()
  if (file.exists(custom)) libs[["custom"]] <- load_template_library(custom)
  libs
}

#' Stack several libraries' normalized templates into one matrix
#'
#' @param libs A named list of template libraries.
#' @return A matrix whose rows are normalized templates, with a "meta"
#'   attribute (a data frame mapping each row to its `set` and `symbol`).
template_matrix <- function(libs) {
  rows <- list()
  set <- character(0)
  symbol <- character(0)
  for (name in names(libs)) {
    for (sym in names(libs[[name]]$norm_pieces)) {
      rows[[length(rows) + 1]] <- libs[[name]]$norm_pieces[[sym]]
      set <- c(set, name)
      symbol <- c(symbol, sym)
    }
  }
  m <- do.call(rbind, rows)
  attr(m, "meta") <- data.frame(
    set = set, symbol = symbol,
    stringsAsFactors = FALSE
  )
  m
}

#' Recognize 64 squares against all available sets at once
#'
#' Scores every square against every template of every installed set in a
#' single matrix multiply, then picks the best-matching set and reports
#' confidence signals (see the inline comments for how they are calibrated).
#'
#' @param squares A list of 64 magick square images, row-major, top row first.
#' @param libs Named list of template libraries to score against (default:
#'   every locally available set).
#' @return A list with `symbols` (from the winning set), `set`, `scores`
#'   (per-set mean similarity), `margin` (winner minus runner-up), `min_occ`
#'   (worst occupied-square match) and `confident` (whether both gates pass).
recognize_symbols_auto <- function(squares, libs = load_all_template_libraries()) {
  if (length(libs) == 0) stop("no template libraries available")

  # 64 probe vectors (normalized, transposed cores) + per-square energies.
  cores <- lapply(squares, function(sq) sq_core(to_gray_matrix(sq)))
  energies <- vapply(cores, sq_energy, numeric(1))
  probes <- do.call(rbind, lapply(cores, function(cr) normalize_vec(t(cr))))

  tm <- template_matrix(libs)
  meta <- attr(tm, "meta")
  sim <- probes %*% t(tm) # 64 x n_templates, one BLAS call

  set_scores <- numeric(length(libs))
  names(set_scores) <- names(libs)
  set_symbols <- list()
  set_min_occ <- numeric(length(libs))
  names(set_min_occ) <- names(libs)

  for (name in names(libs)) {
    cols <- which(meta$set == name)
    sub <- sim[, cols, drop = FALSE]
    best_idx <- max.col(sub, ties.method = "first")
    best_sim <- sub[cbind(seq_len(64), best_idx)]
    occupied <- energies >= libs[[name]]$empty_threshold

    syms <- rep(".", 64)
    syms[occupied] <- meta$symbol[cols][best_idx[occupied]]
    set_symbols[[name]] <- syms
    # Mean similarity over occupied squares; a set that can't see any pieces
    # can't win.
    set_scores[name] <- if (any(occupied)) mean(best_sim[occupied]) else -Inf
    set_min_occ[name] <- if (any(occupied)) min(best_sim[occupied]) else -Inf
  }

  scores <- sort(set_scores, decreasing = TRUE)
  best_set <- names(scores)[1]
  # Confidence signals (see data-raw notes / commit message for calibration):
  #  - margin: how far the winning set stands out from the runner-up. When the
  #    true set is installed one set wins decisively (>=0.12); when it isn't,
  #    every set fits the garbage equally and margin collapses (<0.01).
  #    Renderer-independent, but needs >=2 sets installed.
  #  - min_occ: worst per-square match for the winning set. Known sets explain
  #    every occupied square well (>=0.88 even degraded); an unknown set leaves
  #    some square badly unmatched (<=0.4). Works with any number of sets.
  margin <- if (length(scores) >= 2) scores[1] - scores[2] else NA_real_
  min_occ <- set_min_occ[best_set]
  confident <- (is.na(margin) || margin >= MARGIN_GATE) && (min_occ >= MIN_OCC_GATE)

  list(
    symbols = set_symbols[[best_set]],
    set = best_set,
    scores = scores,
    margin = margin,
    min_occ = unname(min_occ),
    confident = confident
  )
}
