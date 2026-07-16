#' Analyze tab: drag-drop/paste a board screenshot -> best move
#' @noRd

#' @noRd
mod_analyze_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Drop or paste a board screenshot. Works out of the box on Lichess ",
      "(cburnett pieces); orientation is detected automatically."),
    image_input_ui("img", ns),
    fluidRow(
      column(3, radioButtons(ns("turn"), "Side to move", c("White" = "w", "Black" = "b"))),
      column(3, checkboxInput(ns("autocrop"), "Auto-crop to board", value = TRUE)),
      column(6, sliderInput(ns("movetime"), "Engine time (s)", min = 0.2, max = 5, value = 1, step = 0.1))
    ),
    actionButton(ns("analyze"), "Analyze", class = "btn-primary"),
    tags$hr(),
    fluidRow(
      column(6, div(class = "board-preview", h5("Input screenshot"), imageOutput(ns("input_preview"), height = "auto"))),
      column(6, div(class = "board-preview", h5("Recognized position"), imageOutput(ns("recognized_preview"), height = "auto")))
    ),
    uiOutput(ns("result"))
  )
}

#' @noRd
mod_analyze_server <- function(id, chess_ctx) {
  moduleServer(id, function(input, output, session) {
    img <- latest_image_input(input, "img")

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
      lib <- load_template_library(active_templates_path())
      recognize_position(img(), lib, chess_ctx, turn = input$turn, autocrop = input$autocrop)
    })

    output$recognized_preview <- renderImage(
      {
        req(analysis())
        board_img <- render_position(analysis()$display_symbols, size = 400)
        f <- tempfile(fileext = ".png")
        image_write(board_img, f)
        list(src = f, contentType = "image/png", width = 320)
      },
      deleteFile = TRUE
    )
    outputOptions(output, "recognized_preview", suspendWhenHidden = FALSE)

    output$result <- renderUI({
      req(analysis())
      res <- analysis()
      side <- if (res$flip) "black" else "white"
      how <- if (is.na(res$flip_detected)) "assumed" else "auto-detected"

      pieces <- list(
        tags$p(sprintf("Orientation: %s on the bottom (%s).", side, how)),
        tags$code(class = "fen-code", res$fen)
      )

      if (!res$valid) {
        pieces <- c(pieces, list(tags$div(
          class = "alert alert-warning", role = "alert",
          "Recognized position is not legal. A square may have been misread, ",
          "the side-to-move setting may be wrong, or the templates may not ",
          "match this site's piece set (use the Calibrate tab)."
        )))
        return(tagList(pieces))
      }

      eng <- tryCatch(
        best_move_uci(res$fen, movetime_ms = round(input$movetime * 1000)),
        error = function(e) NULL
      )

      if (is.null(eng) || is.null(eng$move) || eng$move == "(none)") {
        pieces <- c(pieces, list(tags$div(class = "alert alert-info", "No legal moves (checkmate or stalemate).")))
        return(tagList(pieces))
      }

      san <- tryCatch(fen_move_to_san(chess_ctx, res$fen, eng$move), error = function(e) NULL)
      move_label <- if (!is.null(san)) sprintf("%s (%s)", san, eng$move) else eng$move

      eval_text <- if (!is.na(eng$score_mate)) {
        sprintf("Mate in %d", abs(eng$score_mate))
      } else if (!is.na(eng$score_cp)) {
        sprintf("%+.2f (pawns, side to move)", eng$score_cp / 100)
      } else {
        NA
      }

      pieces <- c(pieces, list(
        tags$h4(sprintf("Best move: %s", move_label)),
        if (!is.na(eval_text)) tags$p(sprintf("Evaluation: %s", eval_text)),
        if (!is.null(eng$ponder)) tags$p(class = "text-muted", sprintf("Likely reply: %s", eng$ponder))
      ))
      tagList(pieces)
    })
    outputOptions(output, "result", suspendWhenHidden = FALSE)
  })
}
