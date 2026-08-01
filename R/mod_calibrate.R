# Piece sets & calibration tab: download Lichess sets, or teach a new one.

#' Calibrate tab UI
#'
#' @param id The module id.
#' @return A Shiny UI tag list for the piece-sets and calibration tab.
mod_calibrate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$section(
      class = "cv-card",
      cv_section_title(
        "Lichess piece sets",
        paste(
          "About 1 MB in total, fetched from lichess.org's public repository",
          "for personal use. Screenshots using any downloaded set are then",
          "recognised automatically, with nothing to calibrate."
        )
      ),
      div(
        class = "cv-actions",
        actionButton(ns("download_sets"), "Download piece sets", class = "btn-primary")
      ),
      uiOutput(ns("sets_status"))
    ),
    tags$section(
      class = "cv-card",
      cv_section_title(
        "Teach an unknown set",
        tagList(
          "For art the app does not know - Chess.com themes, custom pieces - ",
          "upload or paste a screenshot of that site's ",
          tags$strong("standard starting position"),
          ". The result joins auto-detection as the \"custom\" set."
        )
      ),
      div(
        class = "cv-intake",
        div(class = "cv-file", image_input_ui("cal", ns)),
        div(
          class = "cv-options cv-choices",
          radioButtons(
            ns("orientation"), "Board orientation",
            c(
              "Auto" = "auto",
              "White at bottom" = "white",
              "Black at bottom" = "black"
            ),
            inline = TRUE
          ),
          checkboxInput(ns("autocrop"), "Auto-crop to the board", value = TRUE)
        )
      ),
      div(
        class = "cv-actions",
        actionButton(ns("calibrate"), "Calibrate", class = "btn-primary"),
        actionButton(ns("reset"), "Reset to Lichess defaults")
      ),
      uiOutput(ns("status")),
      div(class = "board-preview cv-cal-preview", imageOutput(ns("preview"), height = "auto"))
    )
  )
}

#' Calibrate tab server
#'
#' @param id The module id.
#' @param chess_ctx A chess.js V8 context from [new_chess_context()] (unused
#'   here; accepted for a uniform module signature).
#' @return Called for its side effects; wires up the calibration reactives.
mod_calibrate_server <- function(id, chess_ctx) {
  moduleServer(id, function(input, output, session) {
    img <- latest_image_input(input, "cal")
    status <- reactiveVal(NULL)
    preview_img <- reactiveVal(NULL)
    sets_refresh <- reactiveVal(0)

    observeEvent(input$download_sets, {
      manifest <- piece_set_manifest()
      withProgress(message = "Downloading piece sets", value = 0, {
        ready <- setup_piece_sets(progress = function(name, i, n) {
          setProgress(value = (i - 1) / n, detail = name)
        })
      })
      sets_refresh(sets_refresh() + 1)
      n_ok <- sum(ready)
      status(tags$div(
        class = if (n_ok > 0) "alert alert-success" else "alert alert-warning",
        sprintf("%d/%d piece sets ready for auto-detection.", n_ok, length(ready)),
        if (any(!ready)) sprintf(" Failed: %s.", paste(names(ready)[!ready], collapse = ", "))
      ))
    })

    output$sets_status <- renderUI({
      sets_refresh()
      installed <- available_piece_sets()
      manifest <- piece_set_manifest()
      tags$p(
        class = "text-muted",
        sprintf(
          "Installed: %s (%d of %d supported).",
          paste(installed, collapse = ", "),
          length(installed), nrow(manifest) + 1
        )
      )
    })
    outputOptions(output, "sets_status", suspendWhenHidden = FALSE)

    observeEvent(input$calibrate, {
      req(img())
      board_img <- prepare_board(img(), autocrop = input$autocrop)
      squares <- split_board(board_img)

      # Orientation decides which end of the board is white's. Get this wrong
      # and every template is learned from the opposing colour, which poisons
      # all later recognition - so it is resolved deterministically here.
      ori <- resolve_calibration_flip(squares, board_img, input$orientation)
      if (isTRUE(ori$flip)) squares <- rev(squares)

      lib <- build_from_start_position(squares)
      # Last line of defence: white pieces must be brighter than black ones.
      guard <- fix_template_colour_swap(lib)
      lib <- guard$lib
      n <- length(lib$pieces)
      save_template_library(lib, custom_templates_path())

      full_lib <- load_template_library(custom_templates_path())
      recognized <- recognize_symbols(squares, full_lib)
      preview_img(render_position(recognized, size = 320))

      detail <- sprintf(
        " Orientation: %s on the bottom (%s).%s",
        if (ori$flip) "black" else "white", ori$source,
        if (guard$swapped) " Piece colours were inverted and have been corrected." else ""
      )

      if (n == 12) {
        status(tags$div(
          class = "alert alert-success",
          "Calibrated all 12 piece templates. You're ready to analyze positions.",
          detail
        ))
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
