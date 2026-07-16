#' Locate a file installed under inst/
#' @param ... path components relative to inst/
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "chessvision")
}

#' Register www/ as a resource path and set favicon/js/css includes
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(src = "www/paste.js")
  )
}

#' Drag-drop file input + clipboard-paste zone, sharing one image.
#'
#' Two Shiny inputs are produced: `{id}_upload` (fileInput) and `{id}_paste`
#' (data-URL string set by paste.js). The server side should combine both with
#' `latest_image_input()`.
#' @noRd
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

#' Combine upload + paste events into one reactive, keeping whichever fired
#' most recently. Returns a magick image or NULL. Must be called from within
#' a module server function (uses observeEvent/reactiveValues on that scope).
#' @noRd
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

#' Decode a "data:image/png;base64,...." string into a magick image.
#' @noRd
decode_data_url_image <- function(data_url) {
  b64 <- sub("^data:[^,]+,", "", data_url)
  raw_bytes <- jsonlite::base64_dec(b64)
  image_read(raw_bytes)
}

#' Path for user-calibrated templates (persists across app restarts).
#' @noRd
custom_templates_path <- function() {
  dir <- tools::R_user_dir("chessvision", "config")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "templates_custom.rds")
}

#' Path to whichever template library is currently active: a user calibration
#' if one exists, otherwise the bundled cburnett (Lichess) default.
#' @noRd
active_templates_path <- function() {
  custom <- custom_templates_path()
  if (file.exists(custom)) custom else app_sys("app", "templates.rds")
}
