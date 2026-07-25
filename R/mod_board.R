# Interactive analysis board: explore positions with live engine evaluation.

#' Board tab UI
#'
#' @param id The module id.
#' @return A Shiny UI tag list for the interactive board tab.
mod_board_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Drag pieces to explore. The engine re-evaluates continuously and the ",
      "bar, score and top lines update as the search deepens. Recognized ",
      "screenshots can be sent here from the Analyze tab."
    ),
    fluidRow(
      column(
        7,
        div(
          class = "cv-board-wrap",
          div(
            class = "cv-evalbar",
            div(class = "cv-evalbar-black", id = ns("evalbar_black")),
            div(class = "cv-evalbar-white", id = ns("evalbar_white"))
          ),
          div(id = ns("board"), class = "cv-board")
        ),
        div(
          class = "cv-board-controls",
          actionButton(ns("flip"), "Flip"),
          actionButton(ns("undo"), "Undo"),
          actionButton(ns("reset"), "Start position"),
          actionButton(ns("play_best"), "Play best move", class = "btn-primary")
        )
      ),
      column(
        5,
        div(class = "cv-eval-headline", textOutput(ns("score"), inline = TRUE)),
        div(class = "text-muted", textOutput(ns("depth_line"), inline = TRUE)),
        tags$hr(),
        tags$strong("Engine lines"),
        uiOutput(ns("lines")),
        tags$hr(),
        tags$strong("Moves"),
        div(class = "cv-movelist", textOutput(ns("movelist"))),
        tags$hr(),
        tags$code(class = "fen-code", textOutput(ns("fen"), inline = TRUE))
      )
    )
  )
}

#' Board tab server
#'
#' Owns a persistent Stockfish process for the session and keeps the eval bar,
#' score, depth and principal variations in sync with the board position.
#'
#' @param id The module id.
#' @param chess_ctx A chess.js V8 context from [new_chess_context()], used to
#'   convert engine moves to SAN.
#' @param incoming A reactive returning a FEN to load into the board (e.g. a
#'   position recognized on the Analyze tab), or `NULL`.
#' @return Called for its side effects; wires up the board reactives.
mod_board_server <- function(id, chess_ctx, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    START_FEN <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    engine <- engine_session_start(multipv = 3L)
    session$onSessionEnded(function() engine_session_stop(engine))

    # Latest analysis snapshot drained from the engine.
    snap <- reactiveVal(NULL)

    # Ids are resolved once here and passed explicitly to the browser, so the
    # JS never has to guess at Shiny's namespacing.
    board_id <- ns("board")
    state_input <- ns("state")

    # Create the client-side board once the session is up.
    session$onFlushed(
      function() {
        session$sendCustomMessage("chessvision-board-create", list(
          container = board_id,
          stateInput = state_input,
          fen = START_FEN,
          orientation = "white",
          pieceTheme = piece_theme_base("cburnett")
        ))
      },
      once = TRUE
    )

    # Board state reported by the browser after every move.
    board_state <- reactive(input$state)

    current_fen <- reactive({
      st <- board_state()
      if (is.null(st) || is.null(st$fen)) START_FEN else st$fen
    })

    # Restart the search whenever the position changes.
    observeEvent(current_fen(), {
      req(engine)
      engine_session_analyse(engine, current_fen())
      snap(NULL)
    })

    # Load an externally supplied position (from the Analyze tab).
    observeEvent(incoming(), {
      fen <- incoming()
      req(!is.null(fen))
      session$sendCustomMessage("chessvision-board-set", list(
        container = board_id,
        fen = fen,
        orientation = "white"
      ))
    })

    observeEvent(input$flip, {
      session$sendCustomMessage("chessvision-board-flip", list(container = board_id))
    })
    observeEvent(input$undo, {
      session$sendCustomMessage("chessvision-board-undo", list(container = board_id))
    })
    observeEvent(input$reset, {
      session$sendCustomMessage("chessvision-board-set", list(
        container = board_id, fen = START_FEN, orientation = "white"
      ))
    })
    observeEvent(input$play_best, {
      s <- snap()
      req(!is.null(s), length(s$lines) > 0)
      session$sendCustomMessage("chessvision-board-move", list(
        container = board_id, uci = s$lines[[1]]$first
      ))
    })

    # ---- live evaluation loop -------------------------------------------
    # Non-blocking: drain whatever the engine has emitted, ~4x/second. Shiny is
    # single-threaded, so this must never wait on the engine.
    observe({
      invalidateLater(250, session)
      req(engine)
      s <- engine_session_poll(engine)
      if (!is.null(s) && length(s$lines)) snap(s)
    })

    turn <- reactive({
      st <- board_state()
      if (is.null(st) || is.null(st$turn)) "w" else st$turn
    })

    output$score <- renderText({
      s <- snap()
      if (is.null(s) || !length(s$lines)) {
        return("evaluating...")
      }
      top <- s$lines[[1]]
      format_score(top$cp, top$mate, turn())
    })

    output$depth_line <- renderText({
      s <- snap()
      if (is.null(s) || !length(s$lines)) {
        return("")
      }
      sprintf("depth %d - %d lines", s$depth, length(s$lines))
    })

    # Eval bar: white's share of the height, from White's point of view.
    observe({
      s <- snap()
      pct <- if (is.null(s) || !length(s$lines)) {
        50
      } else {
        eval_bar_pct(s$lines[[1]]$cp, s$lines[[1]]$mate, turn())
      }
      session$sendCustomMessage("chessvision-evalbar", list(
        white_id = ns("evalbar_white"),
        black_id = ns("evalbar_black"),
        pct = pct
      ))
    })

    # Engine's best move as an arrow on the board.
    observe({
      s <- snap()
      arrows <- list()
      if (!is.null(s) && length(s$lines)) {
        best <- s$lines[[1]]$first
        arrows <- list(list(
          from = substr(best, 1, 2),
          to = substr(best, 3, 4),
          color = "#2b6cb0",
          opacity = 0.7
        ))
      }
      session$sendCustomMessage("chessvision-board-arrows", list(
        container = board_id, arrows = arrows
      ))
    })

    output$lines <- renderUI({
      s <- snap()
      if (is.null(s) || !length(s$lines)) {
        return(tags$p(class = "text-muted", "Waiting for the engine..."))
      }
      fen <- current_fen()
      rows <- lapply(s$lines, function(ln) {
        san <- tryCatch(fen_move_to_san(chess_ctx, fen, ln$first),
          error = function(e) NULL
        )
        label <- if (is.null(san)) ln$first else san
        tags$div(
          class = "cv-line",
          tags$span(class = "cv-line-score", format_score(ln$cp, ln$mate, turn())),
          tags$span(class = "cv-line-move", label),
          tags$span(class = "cv-line-pv", paste(utils::head(ln$pv, 6), collapse = " "))
        )
      })
      do.call(tagList, rows)
    })

    output$movelist <- renderText({
      st <- board_state()
      if (is.null(st) || !length(st$history)) {
        return("(no moves yet)")
      }
      hist <- st$history
      nums <- seq_along(hist)
      paste(ifelse(nums %% 2 == 1, paste0((nums + 1) %/% 2, ". "), ""), hist,
        sep = "", collapse = " "
      )
    })

    output$fen <- renderText(current_fen())

    outputOptions(output, "score", suspendWhenHidden = FALSE)
    outputOptions(output, "depth_line", suspendWhenHidden = FALSE)
    outputOptions(output, "lines", suspendWhenHidden = FALSE)
    outputOptions(output, "movelist", suspendWhenHidden = FALSE)
    outputOptions(output, "fen", suspendWhenHidden = FALSE)
  })
}
