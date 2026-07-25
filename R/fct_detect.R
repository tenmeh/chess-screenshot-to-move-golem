# Board detection: screenshot -> 64 square images, plus orientation.
#
# Port of chessvision/detect.py. Note the local convention: gray matrices here
# are indexed mat[x, y] (x = width/column, y = height/row, both 1-based, y = 1
# is the TOP of the image) - this is what magick::image_data() returns, the
# transpose of numpy's row/column convention used in the Python original.

SQUARE_PX <- 64L
BOARD_PX <- SQUARE_PX * 8L

#' Convert a magick image to a grayscale integer matrix
#'
#' @param img A magick image.
#' @return An integer matrix indexed `[x, y]` (width by height), values 0-255.
to_gray_matrix <- function(img) {
  g <- image_convert(img, colorspace = "gray")
  arr <- image_data(g, channels = "gray")
  w <- dim(arr)[2]
  h <- dim(arr)[3]
  matrix(as.integer(arr[1, , ]), nrow = w, ncol = h)
}

#' Best-effort crop to the board by trimming near-uniform margins
#'
#' @param img A magick image of a board, possibly with surrounding margins.
#' @return A magick image cropped to the detected square board region.
autocrop_board <- function(img) {
  info <- image_info(img)
  w <- info$width
  h <- info$height
  mat <- to_gray_matrix(img)
  col_var <- apply(mat, 1, stats::var) # length w: variance over y, per x
  row_var <- apply(mat, 2, stats::var) # length h: variance over x, per y

  bounds <- function(activity) {
    thresh <- max(activity) * 0.05
    idx <- which(activity > thresh)
    if (length(idx) == 0) {
      return(c(1L, length(activity)))
    }
    c(min(idx), max(idx))
  }

  xb <- bounds(col_var)
  yb <- bounds(row_var)
  # These are inclusive 1-based index bounds, so a span of columns 1..400 is
  # 400 pixels wide, not 399. Getting this wrong crops a pixel short and the
  # 8x8 grid drifts by up to a pixel across the board - invisible on a large
  # screenshot, but enough to misread squares below ~512px.
  bw <- xb[2] - xb[1] + 1L
  bh <- yb[2] - yb[1] + 1L
  side <- min(max(bw, bh), w, h)
  # Anchor on the detected box and grow evenly to a square, rather than
  # rebuilding the origin from an integer-rounded centre (which reintroduced
  # an off-by-one whenever the board sat inside a margin).
  x0 <- max(1L, min(xb[1] - (side - bw) %/% 2L, w - side + 1L))
  y0 <- max(1L, min(yb[1] - (side - bh) %/% 2L, h - side + 1L))

  image_crop(img, geometry_area(side, side, x0 - 1L, y0 - 1L))
}

#' Normalize a screenshot to an exact board-sized RGB image
#'
#' @param image A magick image of a board screenshot.
#' @param autocrop Whether to trim near-uniform margins first.
#' @return A magick image resized to `BOARD_PX` by `BOARD_PX`.
prepare_board <- function(image, autocrop = TRUE) {
  img <- image_convert(image, colorspace = "sRGB")
  if (autocrop) img <- autocrop_board(img)
  image_resize(img, paste0(BOARD_PX, "x", BOARD_PX, "!"))
}

#' Split a normalized board into 64 square images
#'
#' @param board A magick image sized `BOARD_PX` by `BOARD_PX`.
#' @return A list of 64 magick images, row-major, top-left square first.
split_board <- function(board) {
  squares <- vector("list", 64)
  idx <- 1L
  for (r in 0:7) {
    for (c in 0:7) {
      squares[[idx]] <- image_crop(
        board, geometry_area(SQUARE_PX, SQUARE_PX, c * SQUARE_PX, r * SQUARE_PX)
      )
      idx <- idx + 1L
    }
  }
  squares
}

#' Prepare and split a screenshot into 64 square images in one step
#'
#' @param image A magick image of a board screenshot.
#' @param autocrop Whether to trim near-uniform margins first.
#' @return A list of 64 magick images, row-major, top-left square first.
split_squares <- function(image, autocrop = TRUE) {
  split_board(prepare_board(image, autocrop))
}

#' Foreground ink in the rank-label strip of a left-edge square
#'
#' @param mat A grayscale board matrix from [to_gray_matrix()].
#' @param row The 0-indexed board row whose left-edge label strip to measure.
#' @return The fraction of strip pixels that differ strongly from the local
#'   background (higher means more ink, i.e. a wider digit such as 8).
label_ink <- function(mat, row) {
  y0 <- row * SQUARE_PX
  top_corner <- mat[1:6, (y0 + 1):(y0 + 6)]
  bot_corner <- mat[1:6, (y0 + 59):(y0 + 64)]
  bg <- stats::median(c(as.vector(top_corner), as.vector(bot_corner)))
  strip <- mat[1:8, (y0 + 1):(y0 + 32)]
  # Higher contrast threshold than the Python original: magick's autocrop/
  # resize leaves slightly more edge antialiasing than PIL's, which at the
  # original threshold (45) could register a sliver of a corner piece as
  # label ink. 65 still catches solid digit strokes.
  mean(abs(strip - bg) > 65)
}

#' Detect board orientation from rank labels along the left edge
#'
#' Rank labels read 8..1 top-to-bottom for white's view and 1..8 for black's;
#' the digit 1 carries much less ink than 8, so comparing the top and bottom
#' label strips decides orientation.
#'
#' @param board_img A magick image sized `BOARD_PX` by `BOARD_PX`.
#' @return `TRUE` if black is on the bottom, `FALSE` if white is on the bottom,
#'   or `NA` if it cannot tell confidently (no coordinate labels visible).
detect_flip <- function(board_img) {
  mat <- to_gray_matrix(board_img)
  top <- label_ink(mat, 0)
  bottom <- label_ink(mat, 7)
  hi <- max(top, bottom)
  lo <- min(top, bottom)
  if (hi < 0.03 || hi > 0.6 || (hi - lo) < 0.4 * hi) {
    return(NA)
  }
  top < bottom
}
