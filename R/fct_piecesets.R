# Runtime piece-set management.
#
# Piece art is fetched at runtime from the Lichess repository into the user's
# cache and never redistributed with this package - several sets are
# CC BY-NC-SA or freeware licenses that are incompatible with bundling in an
# MIT package, but fine for a user to download for personal use. Only cburnett
# (GPLv2+) ships in inst/svg as the built-in default.

PIECE_SET_BASE_URL <-
  "https://raw.githubusercontent.com/lichess-org/lila/master/public/piece"

#' Curated manifest of supported Lichess piece sets
#'
#' The popular Lichess sets with distinct piece glyphs. Gimmick sets (mono,
#' disguised) are excluded because their pieces are visually ambiguous, which
#' breaks recognition by design. License strings are from lila's COPYING.md.
#'
#' @return A data frame with `name` and `license` columns.
piece_set_manifest <- function() {
  data.frame(
    name = c(
      "merida", "alpha", "chessnut", "fantasy", "spatial", "celtic",
      "staunty", "maestro", "cardinal", "california", "caliente", "fresca",
      "gioco", "tatiana", "horsey", "kiwen-suwi", "leipzig", "companion",
      "chess7", "pirouetti", "rhosgfx"
    ),
    license = c(
      "GPLv2+", "personal use only", "Apache 2.0", "MIT", "MIT", "MIT",
      "CC BY-NC-SA 4.0", "CC BY-NC-SA 4.0", "CC BY-NC-SA 4.0",
      "CC BY-NC-SA 4.0", "CC BY-NC-SA 4.0", "CC BY-NC-SA 4.0",
      "CC BY-NC-SA 4.0", "CC BY-NC-SA 4.0", "CC BY-NC-SA 4.0",
      "CC BY 4.0", "freeware", "freeware",
      "freeware", "AGPLv3+", "CC0 1.0"
    ),
    stringsAsFactors = FALSE
  )
}

PIECE_FILES <- c(
  "wK", "wQ", "wR", "wB", "wN", "wP",
  "bK", "bQ", "bR", "bB", "bN", "bP"
)

#' Cache directory for downloaded piece sets
#'
#' @return The piece-sets cache directory path (created if needed).
piece_sets_dir <- function() {
  dir <- file.path(tools::R_user_dir("chessvision", "cache"), "piece_sets")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Locate the directory holding a set's 12 SVGs
#'
#' @param name A piece-set name (or "cburnett" for the bundled default).
#' @return The directory path, or `NULL` if the set isn't available locally.
piece_set_path <- function(name) {
  if (name == "cburnett") {
    return(app_sys("svg"))
  }
  dir <- file.path(piece_sets_dir(), name)
  if (all(file.exists(file.path(dir, paste0(PIECE_FILES, ".svg"))))) dir else NULL
}

#' Download one piece set's 12 SVGs into the cache
#'
#' Validates that every SVG actually renders under magick before declaring the
#' set usable, and removes a partial download on failure.
#'
#' @param name A piece-set name from [piece_set_manifest()].
#' @param quiet Whether to suppress download progress messages.
#' @return `TRUE` on success, `FALSE` otherwise.
fetch_piece_set <- function(name, quiet = TRUE) {
  if (!is.null(piece_set_path(name))) {
    return(TRUE)
  }
  dir <- file.path(piece_sets_dir(), name)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ok <- TRUE
  for (pf in PIECE_FILES) {
    url <- sprintf("%s/%s/%s.svg", PIECE_SET_BASE_URL, name, pf)
    dest <- file.path(dir, paste0(pf, ".svg"))
    res <- tryCatch(
      {
        utils::download.file(url, dest, mode = "wb", quiet = quiet)
        img <- image_read(dest, density = 128) # render check
        rm(img)
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (!res) {
      ok <- FALSE
      break
    }
  }
  if (!ok) {
    unlink(dir, recursive = TRUE) # don't leave a half-set behind
  }
  ok
}

#' Names of all piece sets available locally
#'
#' @return A character vector of set names (bundled cburnett plus any
#'   downloaded manifest sets).
available_piece_sets <- function() {
  sets <- "cburnett"
  for (name in piece_set_manifest()$name) {
    if (!is.null(piece_set_path(name))) sets <- c(sets, name)
  }
  sets
}

#' Path where a set's calibrated template library lives
#'
#' @param name A piece-set name (or "cburnett" for the bundled default).
#' @return The template-library RDS path for that set.
set_templates_path <- function(name) {
  if (name == "cburnett") {
    return(app_sys("app", "templates.rds"))
  }
  file.path(piece_sets_dir(), name, "templates.rds")
}

#' Build and cache a set's template library
#'
#' Renders the standard starting position with the set's own SVGs and
#' calibrates on it - the exact pipeline used at recognition time.
#'
#' @param name A locally-available piece-set name.
#' @param force Rebuild even if a cached library already exists.
#' @return `TRUE` on success, `FALSE` if the set is unavailable or incomplete.
build_set_templates <- function(name, force = FALSE) {
  out <- set_templates_path(name)
  if (file.exists(out) && !force) {
    return(TRUE)
  }
  set_dir <- piece_set_path(name)
  if (is.null(set_dir)) {
    return(FALSE)
  }
  board <- render_position(START_SYMBOLS, size = 512, set_dir = set_dir)
  squares <- split_board(board)
  lib <- build_from_start_position(squares)
  if (length(lib$pieces) < 12) {
    return(FALSE)
  }
  save_template_library(lib, out)
  TRUE
}

#' Fetch and build templates for every set in the manifest
#'
#' @param progress Optional callback `function(name, i, n)` for UI feedback.
#' @return A named logical vector (`TRUE` = set ready for auto-detection).
setup_piece_sets <- function(progress = NULL) {
  manifest <- piece_set_manifest()
  n <- nrow(manifest)
  ready <- logical(n)
  names(ready) <- manifest$name
  for (i in seq_len(n)) {
    name <- manifest$name[i]
    if (!is.null(progress)) progress(name, i, n)
    ready[i] <- fetch_piece_set(name) && build_set_templates(name)
  }
  ready
}
