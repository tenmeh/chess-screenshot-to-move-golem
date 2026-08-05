# Analyze tab: drag-drop/paste a board screenshot -> best move.

#' Analyze tab UI
#'
#' @param id The module id.
#' @return A Shiny UI tag list for the analyze tab.
mod_analyze_ui <- function(id) {
  ns <- NS(id)
  tags$section(
    class = "cv-card",
    cv_section_title(
      "Read a position",
      paste(
        "Drop or paste a screenshot of a board. The piece set and orientation",
        "are detected automatically - grab more sets from the Piece sets tab",
        "to cover every Lichess theme without calibrating anything."
      ),
      step = "1"
    ),
    div(
      class = "cv-intake",
      div(class = "cv-file", image_input_ui("img", ns)),
      div(
        class = "cv-options cv-choices",
        radioButtons(ns("turn"), "Side to move", c("White" = "w", "Black" = "b"), inline = TRUE),
        radioButtons(
          ns("orientation"), "Orientation",
          c("Auto" = "auto", "White at bottom" = "white", "Black at bottom" = "black"),
          inline = TRUE
        ),
        checkboxInput(ns("autocrop"), "Auto-crop to the board", value = TRUE)
      )
    ),
    div(
      class = "cv-actions",
      actionButton(ns("analyze"), "Analyse position", class = "btn-primary btn-lg")
    ),
    # Already have the position as text? Then recognition is a step to skip,
    # not a step to endure.
    tags$details(
      class = "cv-settings cv-fen-entry",
      tags$summary("...or paste a FEN"),
      div(
        class = "cv-settings-body",
        div(
          class = "cv-fen-row",
          textInput(
            ns("fen_text"), NULL,
            placeholder = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            width = "100%"
          ),
          actionButton(ns("load_fen"), "Load", class = "btn-primary")
        ),
        uiOutput(ns("fen_status"))
      )
    ),
    div(
      class = "cv-readout",
      div(
        class = "cv-readout-shot",
        div(class = "cv-panel-label", "Screenshot"),
        div(class = "board-preview", imageOutput(ns("input_preview"), height = "auto"))
      ),
      div(
        class = "cv-readout-result",
        div(class = "cv-panel-label", "What was read"),
        uiOutput(ns("result"))
      )
    )
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
              "(best-guess set %s, match confidence low: margin %.3f, typical square %.2f)",
              res$set,
              ifelse(is.na(res$set_margin), 0, res$set_margin),
              res$set_median_occ
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

    # ---- loading a position from FEN --------------------------------------

    fen_error <- reactiveVal(NULL)

    observeEvent(input$load_fen, {
      txt <- trimws(input$fen_text %||% "")
      if (!nzchar(txt)) {
        fen_error("Paste a FEN first.")
        return()
      }
      # Fill in the fields people habitually leave off - castling, en passant,
      # the clocks - so a four-field FEN is accepted rather than what every
      # website copies to the clipboard being rejected. chess.js validates
      # strictly and does *not* do this for us, which is why it is done here.
      txt <- fen_complete(txt)
      check <- tryCatch(validate_fen(chess_ctx, txt), error = function(e) NULL)
      if (is.null(check) || !isTRUE(check$ok)) {
        fen_error(check$error %||% "That is not a position chess.js can read.")
        return()
      }
      fen_error(NULL)
      to_board(list(
        fen = txt,
        orientation = if (identical(fen_turn(txt), "b")) "black" else "white",
        nonce = stats::runif(1)
      ))
    })

    output$fen_status <- renderUI({
      err <- fen_error()
      if (is.null(err)) {
        return(NULL)
      }
      # chess.js's own message says which field is wrong, which is more use
      # than a generic "invalid FEN".
      tags$div(class = "alert alert-warning cv-radar-summary", err)
    })
    outputOptions(output, "fen_status", suspendWhenHidden = FALSE)

    to_board
  })
}
