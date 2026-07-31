# Analyze tab: drag-drop/paste a board screenshot -> best move.

#' Analyze tab UI
#'
#' @param id The module id.
#' @return A Shiny UI tag list for the analyze tab.
mod_analyze_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Drop or paste a board screenshot. The piece set and board orientation ",
      "are detected automatically - download more sets in the Piece sets tab ",
      "to cover every Lichess theme with zero calibration."
    ),
    image_input_ui("img", ns),
    fluidRow(
      column(4, radioButtons(ns("turn"), "Side to move", c("White" = "w", "Black" = "b"))),
      column(4, radioButtons(
        ns("orientation"), "Board orientation",
        c("Auto-detect" = "auto", "White on bottom" = "white", "Black on bottom" = "black")
      )),
      column(4, checkboxInput(ns("autocrop"), "Auto-crop to board", value = TRUE))
    ),
    actionButton(ns("analyze"), "Analyze", class = "btn-primary"),
    tags$hr(),
    fluidRow(
      column(5, div(class = "board-preview", h5("Input screenshot"), imageOutput(ns("input_preview"), height = "auto"))),
      column(7, uiOutput(ns("result")))
    ),
    tags$hr()
  )
}

#' Analyze tab server
#'
#' @param id The module id.
#' @param chess_ctx A chess.js V8 context from [new_chess_context()].
#' @return A reactive returning the FEN the user asked to open on the
#'   interactive board, or `NULL` before any request.
mod_analyze_server <- function(id, chess_ctx) {
  moduleServer(id, function(input, output, session) {
    img <- latest_image_input(input, "img")
    to_board <- reactiveVal(NULL)

    output$input_preview <- renderImage(
      {
        req(img())
        f <- tempfile(fileext = ".png")
        image_write(img(), f)
        list(src = f, contentType = "image/png", width = 320)
      },
      deleteFile = TRUE
    )
    # Switching tabs can suspend an output's computation; without this it may
    # never resume even after the tab (or this one) becomes visible again.
    outputOptions(output, "input_preview", suspendWhenHidden = FALSE)

    analysis <- eventReactive(input$analyze, {
      req(img())
      # Reload each run: cheap (a few RDS reads), and picks up any sets the
      # user downloaded or calibrated since the last click.
      libs <- load_all_template_libraries()
      recognize_position(img(), libs, chess_ctx,
        turn = input$turn, autocrop = input$autocrop,
        orientation = input$orientation
      )
    })

    # Hand each new recognition straight to the interactive board below. Only
    # a structurally legal FEN (one king per side, no back-rank pawns, ...)
    # can be loaded - chess.js throws on anything else - so an unreliable
    # guess that fails even that check is left off the board; the FEN text
    # beside it still shows what was read.
    #
    # The nonce matters: re-analysing the same screenshot yields an identical
    # FEN, and observeEvent on the receiving side would treat that as "no
    # change" and skip the reload - leaving whatever the user had dragged
    # around still on the board instead of resetting to what was just read.
    observeEvent(analysis(), {
      res <- analysis()
      req(res$valid)
      to_board(list(
        fen = res$fen,
        orientation = if (res$flip) "black" else "white",
        nonce = stats::runif(1)
      ))
    })

    # This panel reports what was *read* - piece set, orientation, FEN and any
    # doubts about them. What the engine makes of it (best move, evaluation,
    # lines, blunder radar) belongs to the board below, which now analyses the
    # same position live. Running a second one-shot search here as well would
    # duplicate the work and could disagree with the board, since the two use
    # different search times and MultiPV settings.
    output$result <- renderUI({
      req(analysis())
      res <- analysis()
      side <- if (res$flip) "black" else "white"
      how <- res$flip_source

      # Low match confidence => the screenshot's piece set almost certainly
      # isn't installed (typically Chess.com or a custom theme). Say so plainly
      # instead of presenting a guessed, probably-wrong FEN as fact.
      if (!res$set_confident) {
        return(tagList(
          tags$div(
            class = "alert alert-warning", role = "alert",
            tags$strong("Unrecognized piece set. "),
            "This board doesn't match any installed piece set well (closest: ",
            sprintf("%s), so the reading below is an unreliable guess. ", res$set),
            "This is usually a Chess.com or custom theme. To fix it: open the ",
            tags$strong("Piece sets & calibration"), " tab and either download ",
            "the Lichess sets, or calibrate this site once from its starting ",
            "position - or just drag the pieces on the board below into place ",
            "yourself."
          ),
          tags$p(
            class = "text-muted",
            sprintf(
              "(best-guess set %s, match confidence low: margin %.3f, weakest square %.2f)",
              res$set,
              ifelse(is.na(res$set_margin), 0, res$set_margin),
              res$set_min_occ
            )
          ),
          tags$p(tags$em("Best guess FEN (likely wrong):")),
          tags$code(class = "fen-code", res$fen)
        ))
      }

      pieces <- list(
        tags$p(
          sprintf(
            "Piece set: %s (auto-detected). Orientation: %s on the bottom (%s).",
            res$set, side, how
          )
        ),
        tags$code(class = "fen-code", res$fen)
      )

      if (!res$valid) {
        pieces <- c(pieces, list(tags$div(
          class = "alert alert-warning", role = "alert",
          "Recognized position is not legal, so it has not been loaded onto ",
          "the board. A square may have been misread, the side-to-move ",
          "setting may be wrong, or the templates may not match this site's ",
          "piece set (use the Calibrate tab)."
        )))
      }
      tagList(pieces)
    })
    outputOptions(output, "result", suspendWhenHidden = FALSE)

    to_board
  })
}
