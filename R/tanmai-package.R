# Package-level documentation and imports.
#
# Note: each @importFrom tag must fit on a single line - roxygen2 rejects a
# wrapped one - so magick's imports are split across several tags.

#' tanmai: Chess Board Screenshot to Best Move
#'
#' A golem Shiny app that reads a chess board screenshot (drag-drop or clipboard
#' paste), recognizes the position with template matching against multiple piece
#' sets, and suggests the best move via a local Stockfish engine.
#'
#' @keywords internal
#' @import shiny
#' @importFrom magick image_read image_info image_data image_resize
#' @importFrom magick image_crop image_convert image_blank image_composite
#' @importFrom magick image_write geometry_area geometry_point
#' @importFrom rsvg rsvg_png
#' @importFrom processx process
#' @importFrom jsonlite fromJSON base64_dec
#' @importFrom V8 v8
#' @importFrom stats median var setNames
#' @importFrom tools R_user_dir
#' @importFrom utils download.file unzip untar packageVersion head
#' @importFrom golem add_resource_path with_golem_options
"_PACKAGE"
