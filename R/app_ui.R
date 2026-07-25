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
        id = "main_tabs",
        tabPanel("Analyze position", value = "analyze", mod_analyze_ui("analyze")),
        tabPanel("Board", value = "board", mod_board_ui("board")),
        tabPanel("Piece sets & calibration", value = "calibrate", mod_calibrate_ui("calibrate"))
      )
    )
  )
}
