# Shared UI and reactive helpers.

#' Locate a file installed under inst/
#'
#' @param ... Path components relative to inst/.
#' @return The full path returned by [system.file()] for this package.
app_sys <- function(...) {
  system.file(..., package = "chessvision")
}

#' Register static resource paths and add css/js includes
#'
#' Serves the package's own `www/` assets, plus the bundled cburnett SVGs and
#' any downloaded piece sets so the interactive board can use them as its
#' piece theme.
#'
#' @return A `tags$head` include with the stylesheets and scripts.
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  # Piece art for the interactive board: bundled cburnett, plus the cache dir
  # holding any sets the user downloaded.
  add_resource_path("pieces", app_sys("svg"))
  add_resource_path("piecesets", piece_sets_dir())
  # The same chess.js bundle the server uses via V8, served to the browser so
  # both sides agree on the rules.
  add_resource_path("chessjs", app_sys("js"))
  # Cache-bust our own assets on every package version so an upgrade can't
  # leave a browser running last version's CSS/JS against new markup.
  v <- paste0("?v=", utils::packageVersion("chessvision"))
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/chessboard-1.0.0.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("www/styles.css", v)),
    tags$script(src = "www/chessboard-1.0.0.min.js"),
    # chess.js is a CommonJS build; give it a module/exports shim before it
    # loads, then hoist the constructor onto window for board.js.
    tags$script(HTML("var module = {exports: {}}; var exports = module.exports;")),
    tags$script(src = "chessjs/chess.js"),
    tags$script(HTML("window.ChessCtor = module.exports.Chess;")),
    tags$script(src = paste0("www/paste.js", v)),
    tags$script(src = paste0("www/board.js", v))
  )
}

#' Base URL for a piece set's SVGs as served to the browser
#'
#' @param set A piece-set name, or "custom"/unknown for the bundled default.
#' @return A URL prefix that [mod_board_ui()]'s piece theme appends
#'   `{piece}.svg` to.
piece_theme_base <- function(set = "cburnett") {
  if (identical(set, "cburnett") || is.null(set) || is.na(set)) {
    return("pieces")
  }
  if (is.null(piece_set_path(set))) {
    return("pieces")
  }
  file.path("piecesets", set)
}

#' Drag-drop file input plus a clipboard-paste zone
#'
#' Produces two Shiny inputs: `{id}_upload` (a fileInput) and `{id}_paste` (a
#' data-URL string set by paste.js). Combine them server-side with
#' [latest_image_input()].
#'
#' @param id An id prefix, namespaced within the module.
#' @param ns The module namespace function.
#' @return A Shiny UI tag list.
image_input_ui <- function(id, ns) {
  tagList(
    fileInput(ns(paste0(id, "_upload")), "Drag & drop or browse",
      accept = c("image/png", "image/jpeg", "image/webp")
    ),
    tags$div(
      class = "paste-zone", tabindex = "0",
      `data-target` = ns(paste0(id, "_paste")),
      "Click here, then Ctrl+V to paste a screenshot"
    )
  )
}

#' Combine upload and paste events into one image reactive
#'
#' Keeps whichever of the two inputs fired most recently. Must be called from
#' within a module server function (it registers observers on that scope).
#'
#' @param input The module's `input` object.
#' @param id The same id prefix passed to [image_input_ui()].
#' @return A reactive returning a magick image, or `NULL` before any input.
latest_image_input <- function(input, id) {
  upload_id <- paste0(id, "_upload")
  paste_id <- paste0(id, "_paste")
  state <- reactiveValues(image = NULL)

  observeEvent(input[[upload_id]], {
    state$image <- image_read(input[[upload_id]]$datapath)
  })
  observeEvent(input[[paste_id]], {
    state$image <- decode_data_url_image(input[[paste_id]])
  })

  reactive(state$image)
}

#' Decode a data-URL image string into a magick image
#'
#' @param data_url A "data:image/...;base64,...." string.
#' @return A magick image.
decode_data_url_image <- function(data_url) {
  b64 <- sub("^data:[^,]+,", "", data_url)
  raw_bytes <- jsonlite::base64_dec(b64)
  image_read(raw_bytes)
}

#' Path for user-calibrated ("custom") templates
#'
#' @return The RDS path where a manual calibration is persisted across
#'   restarts.
custom_templates_path <- function() {
  dir <- tools::R_user_dir("chessvision", "config")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "templates_custom.rds")
}

#' Path to the currently active template library
#'
#' @return The user calibration path if one exists, otherwise the bundled
#'   cburnett (Lichess) default.
active_templates_path <- function() {
  custom <- custom_templates_path()
  if (file.exists(custom)) custom else app_sys("app", "templates.rds")
}
