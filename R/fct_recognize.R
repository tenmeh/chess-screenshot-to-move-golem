#' Template-based piece recognition (port of chessvision/recognize.py)
#'
#' Each square is matched in background-subtracted form: the background is
#' estimated from the square's own four corners, then subtracted, so board
#' themes, last-move highlights, and coordinate labels don't throw it off.
#' @noRd

MARGIN <- 6L
CORNER <- MARGIN + 4L # 10
PIECE_SYMBOLS <- c("P", "N", "B", "R", "Q", "K", "p", "n", "b", "r", "q", "k")

#' @noRd
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

#' @noRd
sq_core <- function(mat) {
  bg <- sq_background(mat)
  resid <- mat - bg
  n <- nrow(mat)
  resid[(MARGIN + 1):(n - MARGIN), (MARGIN + 1):(n - MARGIN)]
}

#' @noRd
sq_energy <- function(core) mean(abs(core))

#' @noRd
normalize_vec <- function(v) {
  v <- as.vector(v) - mean(as.vector(v))
  nrm <- sqrt(sum(v^2))
  if (nrm > 1e-6) v / nrm else v
}

#' Load the bundled (or calibrated) template library
#' @noRd
load_template_library <- function(path) {
  raw <- readRDS(path)
  norm_pieces <- lapply(raw$pieces, normalize_vec)
  list(
    pieces = raw$pieces,
    norm_pieces = norm_pieces,
    empty_threshold = raw$empty_threshold
  )
}

#' @noRd
save_template_library <- function(lib, path) {
  saveRDS(list(pieces = lib$pieces, empty_threshold = lib$empty_threshold), path)
}

#' Classify one square (magick image) as a piece symbol or "." for empty
#' @noRd
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

#' Classify all 64 squares (row-major, top row first) into piece symbols
#' @noRd
recognize_symbols <- function(squares, lib) {
  vapply(squares, classify_square, character(1), lib = lib)
}

#' Calibrate a template library from 64 squares of the standard starting
#' position (white on bottom, row-major top-first). Used both to build the
#' bundled default (cburnett) templates and for user calibration against a
#' different piece set/site.
#' @noRd
build_from_start_position <- function(squares) {
  start_symbols <- c(
    "r", "n", "b", "q", "k", "b", "n", "r",
    "p", "p", "p", "p", "p", "p", "p", "p",
    rep(".", 8), rep(".", 8), rep(".", 8), rep(".", 8),
    "P", "P", "P", "P", "P", "P", "P", "P",
    "R", "N", "B", "Q", "K", "B", "N", "R"
  )

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
