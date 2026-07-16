#' Run the chessvision Shiny application
#'
#' @param ... arguments to pass to golem::with_golem_options()
#' @export
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
}
