#' Runtime piece-set management.
#'
#' Piece art is fetched at runtime from the Lichess repository into the user's
#' cache and never redistributed with this package - several sets are
#' CC BY-NC-SA or freeware licenses that are incompatible with bundling in an
#' MIT package, but fine for a user to download for personal use. Only
#' cburnett (GPLv2+) ships in inst/svg as the built-in default.
#' @noRd

PIECE_SET_BASE_URL <-
  "https://raw.githubusercontent.com/lichess-org/lila/master/public/piece"

#' Curated piece sets: the popular Lichess sets with distinct piece glyphs.
#' Gimmick sets (mono, disguised) are excluded - their pieces are visually
#' ambiguous, which breaks recognition by design.
#' License strings from lila's COPYING.md.
#' @noRd
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

PIECE_FILES <- c("wK", "wQ", "wR", "wB", "wN", "wP",
                 "bK", "bQ", "bR", "bB", "bN", "bP")

#' @noRd
piece_sets_dir <- function() {
  dir <- file.path(tools::R_user_dir("chessvision", "cache"), "piece_sets")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Directory holding a set's 12 SVGs: the bundled set from inst/, downloaded
#' sets from the cache. Returns NULL if the set isn't available locally.
#' @noRd
piece_set_path <- function(name) {
  if (name == "cburnett") {
    return(app_sys("svg"))
  }
  dir <- file.path(piece_sets_dir(), name)
  if (all(file.exists(file.path(dir, paste0(PIECE_FILES, ".svg"))))) dir else NULL
}

#' Download one piece set (12 SVGs) into the cache. Validates that every SVG
#' actually renders under magick before declaring the set usable.
#' Returns TRUE on success.
#' @noRd
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

#' Names of all sets available locally (bundled + downloaded + "custom" if the
#' user calibrated one manually).
#' @noRd
available_piece_sets <- function() {
  sets <- "cburnett"
  for (name in piece_set_manifest()$name) {
    if (!is.null(piece_set_path(name))) sets <- c(sets, name)
  }
  sets
}

#' Path where a set's calibrated template library lives.
#' @noRd
set_templates_path <- function(name) {
  if (name == "cburnett") {
    return(app_sys("app", "templates.rds"))
  }
  file.path(piece_sets_dir(), name, "templates.rds")
}

#' Build (and cache) the template library for a locally-available set by
#' rendering the standard starting position with that set's own SVGs and
#' calibrating on it - the exact pipeline used at recognition time.
#' @noRd
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

#' Fetch + build templates for every set in the manifest. Returns a named
#' logical vector (TRUE = set ready). `progress` is an optional function
#' (name, i, n) for UI feedback.
#' @noRd
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
