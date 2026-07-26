# Rebuild the bundled default (cburnett) template library.
#
# Renders the standard starting position with the app's own compositing
# pipeline, then calibrates against it with the app's own square-extraction
# pipeline, so the templates and runtime recognition come from the exact same
# code path and there is no cross-renderer mismatch.
#
# Run from the package root:
#   Rscript data-raw/build_default_templates.R

pkgload::load_all(quiet = TRUE)

board <- render_position(START_SYMBOLS, size = 512)
lib <- build_from_start_position(split_board(board))

# Guard against ever shipping a colour-inverted library.
guard <- fix_template_colour_swap(lib)
if (guard$swapped) stop("templates came out colour-inverted; refusing to save")

cat(sprintf(
  "Built %d/12 templates. Empty threshold: %.3f\n",
  length(lib$pieces), lib$empty_threshold
))

out_path <- app_sys("app", "templates.rds")
save_template_library(lib, out_path)
cat("Saved to", out_path, "\n")
