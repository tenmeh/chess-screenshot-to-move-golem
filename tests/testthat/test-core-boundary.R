# The boundary between the reusable core and the Shiny app.
#
# Everything in R/fct_*.R is the part of this package that could stand on its
# own: recognition, FEN assembly, engine and Maia sessions, blunder risk,
# rating estimation, game tracking. None of it should need a UI toolkit. The
# app layer - app_*, mod_*, golem_utils - is free to.
#
# That separation is currently free, and this keeps it that way. It is not
# theoretical tidiness: a Shiny app package cannot go to CRAN (vendored
# JavaScript, runtime binary downloads, a compiled engine), so if the core is
# ever to be published on its own, it has to stay clean. Discovering a
# tangle at that point is expensive; discovering it in the commit that
# introduced it is not.
#
# The first time this was checked by hand, the grep gave a false negative and
# reported the core clean when `eval_graph_svg()` was building `tags$svg` in
# it. Hence a test rather than periodic eyeballing.

#' Locate the package's R/ directory, wherever the tests are running from
#'
#' `devtools::test()` runs beside the source; `R CMD check` unpacks a tarball
#' and runs from a different depth. Walking up until a DESCRIPTION for this
#' package turns up handles both, and returns NULL when the source genuinely
#' is not available (an installed-package test run).
find_pkg_r_dir <- function() {
  dir <- normalizePath(testthat::test_path("."), mustWork = FALSE)
  for (i in 1:6) {
    desc <- file.path(dir, "DESCRIPTION")
    r_dir <- file.path(dir, "R")
    if (file.exists(desc) && dir.exists(r_dir)) {
      pkg <- tryCatch(read.dcf(desc, "Package")[[1]], error = function(e) NA_character_)
      if (identical(pkg, "chessvision")) {
        return(r_dir)
      }
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  NULL
}

test_that("the core layer builds no UI", {
  r_dir <- find_pkg_r_dir()
  skip_if(is.null(r_dir), "package source not available in this run")

  core <- list.files(r_dir, pattern = "^fct_.*\\.R$", full.names = TRUE)
  expect_gt(length(core), 10) # the glob still matches something

  # Constructors and namespaces that only exist to build a user interface.
  # Comments are stripped first so prose about "a Shiny input id" does not
  # count as a dependency on one.
  ui_calls <- paste(
    "tags\\$", "shiny::", "htmltools::", "\\bHTML\\(",
    "\\bdiv\\(", "\\bspan\\(", "\\btagList\\(",
    "\\brenderUI\\(", "\\bmoduleServer\\(", "\\breactive\\(",
    "\\bobserveEvent\\(", "\\bNS\\(",
    sep = "|"
  )

  offenders <- character(0)
  for (f in core) {
    code <- sub("#.*$", "", readLines(f, warn = FALSE))
    hits <- grep(ui_calls, code)
    if (length(hits)) {
      offenders <- c(offenders, sprintf(
        "%s:%d  %s", basename(f), hits, trimws(code[hits])
      ))
    }
  }

  expect_identical(
    offenders, character(0),
    info = paste0(
      "R/fct_*.R must not construct UI. Move the rendering into the app ",
      "layer (golem_utils.R, or the module that needs it) and leave the ",
      "arithmetic in the core, as eval_graph_geometry()/eval_graph_svg() do.",
      "\nFound:\n", paste(offenders, collapse = "\n")
    )
  )
})

test_that("the graph geometry is computed without a UI toolkit", {
  # The half of the evaluation graph that was extracted back into the core:
  # pure numbers, independently checkable.
  g <- eval_graph_geometry(c(0, 100, -50), width = 400, height = 100)

  expect_equal(g$n, 3)
  expect_length(g$xs, 3)
  expect_equal(g$xs[1], 0)
  expect_equal(g$xs[3], 400)
  expect_equal(g$mid, 50)
  expect_type(g$points, "character")

  # A better position for White must sit higher up the graph than a worse one,
  # y growing downward.
  expect_lt(g$ys[2], g$ys[1]) # +1.00 is above level
  expect_gt(g$ys[3], g$ys[1]) # -0.50 is below it

  expect_null(eval_graph_geometry(numeric(0)))
  expect_null(eval_graph_geometry(c(NA_real_, NA_real_)))
})

test_that("unevaluated positions carry the last value forward", {
  # Without this the line would break, or drop to zero, every time the engine
  # fell behind a fast game.
  filled <- eval_graph_geometry(c(200, NA, NA), width = 300, height = 100)
  flat <- eval_graph_geometry(c(200, 200, 200), width = 300, height = 100)
  expect_equal(filled$ys, flat$ys)

  # Before the first measurement there is nothing to carry, so it sits level.
  leading <- eval_graph_geometry(c(NA, 0), width = 300, height = 100)
  expect_equal(leading$ys[1], leading$ys[2])
})
