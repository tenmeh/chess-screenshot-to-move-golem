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
        # The board lives on the same tab as the screenshot it came from:
        # recognizing a position and then exploring it is one task, and
        # splitting it across tabs meant a round trip (and a button) between
        # reading a board and doing anything with it.
        tabPanel(
          "Analyze position",
          value = "analyze",
          mod_analyze_ui("analyze"),
          mod_board_ui("board"),
          tags$hr(),
          mod_live_ui("live")
        ),
        tabPanel("Piece sets & calibration", value = "calibrate", mod_calibrate_ui("calibrate"))
      )
    )
  )
}
