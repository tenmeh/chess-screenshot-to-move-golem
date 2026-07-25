#' Build the bundled default (cburnett) template library.
#'
#' Renders the standard starting position using the app's own SVG-compositing
#' pipeline (fct_render.R), then calibrates against it using the app's own
#' square-extraction pipeline (fct_detect.R / fct_recognize.R). Templates and
#' runtime recognition are produced by the exact same code path, so there is
#' no cross-library rendering mismatch to worry about.
#'
#' Run from the package root:
#'   Rscript data-raw/build_default_templates.R
library(magick)

pkg_root <- "C:/Users/tchan/chess-golem"
app_sys <- function(...) file.path(pkg_root, "inst", ...)

source(file.path(pkg_root, "R", "fct_detect.R"))
source(file.path(pkg_root, "R", "fct_recognize.R"))
source(file.path(pkg_root, "R", "fct_render.R"))

start_symbols <- c(
  "r", "n", "b", "q", "k", "b", "n", "r",
  "p", "p", "p", "p", "p", "p", "p", "p",
  rep(".", 8), rep(".", 8), rep(".", 8), rep(".", 8),
  "P", "P", "P", "P", "P", "P", "P", "P",
  "R", "N", "B", "Q", "K", "B", "N", "R"
)

board <- render_position(start_symbols, size = 512)
squares <- split_board(board)
lib <- build_from_start_position(squares)

cat(sprintf(
  "Built %d/12 templates. Empty threshold: %.3f\n",
  length(lib$pieces), lib$empty_threshold
))

out_path <- app_sys("app", "templates.rds")
save_template_library(lib, out_path)
cat("Saved to", out_path, "\n")
