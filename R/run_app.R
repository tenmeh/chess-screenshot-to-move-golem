#' Run the chessvision Shiny application
#'
#' @param ... Arguments passed to [golem::with_golem_options()].
#' @return Called for its side effect; runs the Shiny app.
#' @export
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
}
