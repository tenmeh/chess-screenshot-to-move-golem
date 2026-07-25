#' Top-level Shiny UI
#'
#' @param request The Shiny request object (unused; present for the golem
#'   bookmarking signature).
#' @return A Shiny UI tag list.
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      titlePanel("Chess Screenshot -> Best Move"),
      tabsetPanel(
        tabPanel("Analyze position", mod_analyze_ui("analyze")),
        tabPanel("Piece sets & calibration", mod_calibrate_ui("calibrate"))
      )
    )
  )
}
