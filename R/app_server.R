#' Top-level Shiny server
#'
#' @param input,output,session Standard Shiny server arguments.
#' @return Called for its side effects; wires up the module servers.
app_server <- function(input, output, session) {
  # One chess.js (V8) context per session, reused across analyses/calibrations.
  chess_ctx <- new_chess_context()

  to_board <- mod_analyze_server("analyze", chess_ctx)
  mod_board_server("board", chess_ctx, incoming = to_board)
  mod_calibrate_server("calibrate", chess_ctx)

  # "Open in board" on the Analyze tab switches to the board tab.
  observeEvent(to_board(), {
    req(!is.null(to_board()))
    updateTabsetPanel(session, "main_tabs", selected = "board")
  })
}
