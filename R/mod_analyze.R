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
      column(3, radioButtons(ns("turn"), "Side to move", c("White" = "w", "Black" = "b"))),
      column(3, radioButtons(
        ns("orientation"), "Board orientation",
        c("Auto-detect" = "auto", "White on bottom" = "white", "Black on bottom" = "black")
      )),
      column(3, checkboxInput(ns("autocrop"), "Auto-crop to board", value = TRUE)),
      column(3, sliderInput(ns("movetime"), "Engine time (s)", min = 0.2, max = 5, value = 1, step = 0.1))
    ),
    actionButton(ns("analyze"), "Analyze", class = "btn-primary"),
    tags$hr(),
    fluidRow(
      column(6, div(class = "board-preview", h5("Input screenshot"), imageOutput(ns("input_preview"), height = "auto"))),
      column(6, div(
        class = "board-preview",
        h5("Recognized position"),
        p(class = "text-muted", "Drag pieces here to fix a misread square."),
        div(id = ns("preview_board"), class = "cv-board cv-board-preview")
      ))
    ),
    uiOutput(ns("result"))
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

    # A live, draggable board standing in for the old static preview image.
    # Recognition is often close but not perfect, so letting the user correct
    # a misread square by hand - right here, before deciding whether to open
    # it on the interactive board - beats forcing a recalibration for one bad
    # square. It is the same client-side board.js component the Board tab
    # uses, just without an attached engine.
    preview_id <- session$ns("preview_board")
    preview_state_input <- session$ns("preview_state")
    session$onFlushed(
      function() {
        session$sendCustomMessage("chessvision-board-create", list(
          container = preview_id,
          stateInput = preview_state_input,
          fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
          orientation = "white",
          pieceTheme = piece_theme_base("cburnett")
        ))
      },
      once = TRUE
    )
    preview_fen <- reactive({
      st <- input$preview_state
      if (is.null(st)) NULL else st$fen
    })

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

    # Push each new recognition onto the preview board. Only a structurally
    # legal FEN (one king per side, no back-rank pawns, ...) can be loaded -
    # chess.js throws on anything else - so an unreliable guess that fails
    # even that check is left off the board; the FEN text below still shows
    # it.
    observeEvent(analysis(), {
      res <- analysis()
      req(res$valid)
      side <- if (res$flip) "black" else "white"
      session$sendCustomMessage("chessvision-board-set", list(
        container = preview_id, fen = res$fen, orientation = side
      ))
    })

    output$result <- renderUI({
      req(analysis())
      res <- analysis()
      side <- if (res$flip) "black" else "white"
      how <- res$flip_source

      # "Open in board" only needs the FEN to be structurally loadable by
      # chess.js (one king per side, no back-rank pawns, ...), not for the
      # recognition to have been confident or the read to have found
      # something sensible. A low-confidence or checkmated position is still
      # worth fixing up by hand on the live board above, or exploring further
      # on the interactive board.
      open_board_btn <- if (res$valid) {
        list(actionButton(session$ns("open_board"), "Open in board", class = "btn-primary"))
      } else {
        NULL
      }

      # Low match confidence => the screenshot's piece set almost certainly
      # isn't installed (typically Chess.com or a custom theme). Say so plainly
      # instead of presenting a guessed, probably-wrong FEN as fact.
      if (!res$set_confident) {
        return(tagList(c(
          list(
            tags$div(
              class = "alert alert-warning", role = "alert",
              tags$strong("Unrecognized piece set. "),
              "This board doesn't match any installed piece set well (closest: ",
              sprintf("%s), so the reading below is an unreliable guess. ", res$set),
              "This is usually a Chess.com or custom theme. To fix it: open the ",
              tags$strong("Piece sets & calibration"), " tab and either download ",
              "the Lichess sets, or calibrate this site once from its starting ",
              "position - or just drag the pieces above into place yourself."
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
          ),
          open_board_btn
        )))
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
        pieces <- c(pieces, list(
          tags$div(class = "alert alert-info", "No legal moves (checkmate or stalemate).")
        ), open_board_btn)
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

      pieces <- c(
        pieces,
        list(
          tags$h4(sprintf("Best move: %s", move_label)),
          if (!is.na(eval_text)) tags$p(sprintf("Evaluation: %s", eval_text)),
          if (!is.null(eng$ponder)) tags$p(class = "text-muted", sprintf("Likely reply: %s", eng$ponder))
        ),
        open_board_btn
      )
      tagList(pieces)
    })
    outputOptions(output, "result", suspendWhenHidden = FALSE)

    # Hand the position to the interactive board tab. Prefer whatever is
    # currently on the preview board over the raw recognition, so any manual
    # correction the user just dragged into place travels with it.
    observeEvent(input$open_board, {
      res <- analysis()
      req(!is.null(res), res$valid)
      fen <- preview_fen()
      to_board(if (is.null(fen)) res$fen else fen)
    })

    to_board
  })
}
