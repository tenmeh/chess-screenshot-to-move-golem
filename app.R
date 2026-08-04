# Deployment entrypoint.
#
# Works both locally (`shiny::runApp()` / RStudio's Run App) and inside a
# container. Cloud Run and most container hosts inject the port to listen on
# via $PORT and require binding to 0.0.0.0 rather than localhost.

if (requireNamespace("tanmai", quietly = TRUE)) {
  library(tanmai)
} else {
  # Not installed: running straight from a source checkout.
  pkgload::load_all(quiet = TRUE)
}

options(
  shiny.host = Sys.getenv("SHINY_HOST", "0.0.0.0"),
  shiny.port = as.integer(Sys.getenv("PORT", "8080"))
)

run_app()
