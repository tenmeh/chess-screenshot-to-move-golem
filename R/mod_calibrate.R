#' Calibrate tab: teach the recognizer a different site's piece set
#' @noRd

#' @noRd
mod_calibrate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("The app already ships with ", tags$strong("Lichess (cburnett)"), " templates, ",
      "so you only need this for a different piece set (Chess.com, a custom theme, etc). ",
      "Upload or paste a screenshot of that site's ", tags$strong("standard starting position"),
      " to teach the recognizer its pieces."),
    image_input_ui("cal", ns),
    checkboxInput(ns("autocrop"), "Auto-crop to the board", value = TRUE),
    actionButton(ns("calibrate"), "Calibrate", class = "btn-primary"),
    actionButton(ns("reset"), "Reset to Lichess defaults"),
    tags$hr(),
    uiOutput(ns("status")),
    div(class = "board-preview", imageOutput(ns("preview"), height = "auto"))
  )
}

#' @noRd
mod_calibrate_server <- function(id, chess_ctx) {
  moduleServer(id, function(input, output, session) {
    img <- latest_image_input(input, "cal")
    status <- reactiveVal(NULL)
    preview_img <- reactiveVal(NULL)

    observeEvent(input$calibrate, {
      req(img())
      board_img <- prepare_board(img(), autocrop = input$autocrop)
      squares <- split_board(board_img)
      flip <- detect_flip(board_img)
      if (isTRUE(flip)) squares <- rev(squares)

      lib <- build_from_start_position(squares)
      n <- length(lib$pieces)
      save_template_library(lib, custom_templates_path())

      full_lib <- load_template_library(custom_templates_path())
      recognized <- recognize_symbols(squares, full_lib)
      preview_img(render_position(recognized, size = 320))

      if (n == 12) {
        status(tags$div(class = "alert alert-success",
          "Calibrated all 12 piece templates. You're ready to analyze positions."))
      } else {
        missing <- setdiff(PIECE_SYMBOLS, names(lib$pieces))
        status(tags$div(class = "alert alert-warning", sprintf(
          "Only %d/12 templates learned (missing %s). Make sure this is the standard starting position.",
          n, paste(missing, collapse = ", ")
        )))
      }
    })

    observeEvent(input$reset, {
      custom <- custom_templates_path()
      if (file.exists(custom)) file.remove(custom)
      status(tags$div(class = "alert alert-info", "Reset to the bundled Lichess (cburnett) templates."))
      preview_img(NULL)
    })

    output$status <- renderUI(status())
    # Tabs start hidden (only "Analyze" is shown initially); without this,
    # Shiny suspends computation for outputs in a not-yet-visible tab and
    # never triggers their first render, even after the tab is opened.
    outputOptions(output, "status", suspendWhenHidden = FALSE)

    output$preview <- renderImage(
      {
        req(preview_img())
        f <- tempfile(fileext = ".png")
        image_write(preview_img(), f)
        list(src = f, contentType = "image/png", width = 320)
      },
      deleteFile = TRUE
    )
    outputOptions(output, "preview", suspendWhenHidden = FALSE)
  })
}
