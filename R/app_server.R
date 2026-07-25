#' Top-level Shiny server
#'
#' @param input,output,session Standard Shiny server arguments.
#' @return Called for its side effects; wires up the module servers.
app_server <- function(input, output, session) {
  # One chess.js (V8) context per session, reused across analyses/calibrations.
  chess_ctx <- new_chess_context()

  mod_analyze_server("analyze", chess_ctx)
  mod_calibrate_server("calibrate", chess_ctx)
}
