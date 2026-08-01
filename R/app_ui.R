#' Top-level Shiny UI
#'
#' The board is the centre of this app, so it gets the top of the page and the
#' width to be read comfortably. Everything that feeds it - the screenshot, the
#' capture controls, the calibration - is arranged around that, rather than the
#' board being the third thing down a stack of forms.
#'
#' @param request The Shiny request object (unused; present for the golem
#'   bookmarking signature).
#' @return A Shiny UI tag list.
app_ui <- function(request) {
  bslib::page_navbar(
    title = cv_brand(),
    id = "main_tabs",
    theme = cv_theme(),
    window_title = "Tanmai Chess - read a board, find the move",
    fillable = FALSE,
    # Stylesheets, the chessboard/chess.js bundles and the resource paths they
    # are served from. This has to hang off the page itself: `page_navbar()`
    # does not take a bare tag list the way `fluidPage()` did, so dropping it
    # leaves every asset unloaded and the app renders as unstyled HTML.
    header = golem_add_external_resources(),

    bslib::nav_panel(
      "Analyse",
      value = "analyze",
      div(
        class = "cv-page",
        mod_analyze_ui("analyze"),
        mod_board_ui("board"),
        mod_live_ui("live")
      )
    ),
    bslib::nav_panel(
      "Piece sets",
      value = "calibrate",
      div(class = "cv-page", mod_calibrate_ui("calibrate"))
    ),

    bslib::nav_spacer(),
    bslib::nav_item(
      tags$a(
        class = "cv-navlink",
        href = "https://github.com/tenmeh/chess-screenshot-to-move-golem",
        target = "_blank", rel = "noopener",
        "Source"
      )
    ),
    bslib::nav_item(bslib::input_dark_mode(id = "colour_mode"))
  )
}

#' Wordmark for the navbar
#'
#' "Tanmai" is Tanmay and Maia run together - the author, and the human-move
#' network the Blunder Radar is built on. The R package stays `chessvision`:
#' that name is an implementation detail nobody using the app ever sees, and
#' renaming it would touch the NAMESPACE, the Dockerfile's build-time checks
#' and every internal call for no user-visible gain.
#'
#' @return A Shiny tag.
cv_brand <- function() {
  tags$span(
    class = "cv-brand",
    tags$span(class = "cv-brand-mark", HTML("&#9822;")), # knight
    tags$span(
      class = "cv-brand-name",
      "Tanmai", tags$span(class = "cv-brand-sub", "chess")
    )
  )
}

#' A section heading with an optional lead paragraph
#'
#' Used instead of bare `h4()` + `p()` so every section on the page shares one
#' rhythm rather than each module inventing its own spacing.
#'
#' @param title Heading text.
#' @param lead Optional supporting sentence.
#' @param step Optional short marker shown before the title.
#' @return A Shiny tag.
cv_section_title <- function(title, lead = NULL, step = NULL) {
  tags$div(
    class = "cv-section-head",
    tags$div(
      class = "cv-section-titleline",
      if (!is.null(step)) tags$span(class = "cv-step", step),
      tags$h2(class = "cv-section-title", title)
    ),
    if (!is.null(lead)) tags$p(class = "cv-section-lead", lead)
  )
}
