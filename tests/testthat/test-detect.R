# Regression tests for board cropping and splitting.

test_that("a board splits into 64 squares of the expected size", {
  squares <- squares_of(fen_symbols(MID_FEN))
  expect_length(squares, 64)
  info <- magick::image_info(squares[[1]])
  expect_equal(info$width, SQUARE_PX)
  expect_equal(info$height, SQUARE_PX)
})

test_that("autocrop is a no-op on a board that has no margins", {
  # It used to treat an inclusive index difference as a length, cropping one
  # pixel short; the 8x8 grid then drifted and misread squares below ~512px.
  for (size in c(256L, 400L, 512L)) {
    img <- board_of(fen_symbols(MID_FEN), size = size)
    cropped <- magick::image_info(autocrop_board(img))
    expect_equal(cropped$width, size, info = paste("size", size))
    expect_equal(cropped$height, size, info = paste("size", size))
  }
})

test_that("small boards still recognize exactly once cropped", {
  truth <- fen_symbols(MID_FEN)
  libs <- cburnett_libs()
  for (size in c(256L, 320L, 400L, 512L)) {
    img <- board_of(truth, size = size)
    rec <- recognize_symbols_auto(
      split_board(prepare_board(img, autocrop = TRUE)), libs
    )
    expect_equal(rec$symbols, truth, info = paste("size", size))
    expect_true(rec$confident, info = paste("size", size))
  }
})

test_that("autocrop trims a surrounding margin", {
  truth <- fen_symbols(MID_FEN)
  padded <- magick::image_border(
    board_of(truth, size = 480), color = "#202020", geometry = "40x40"
  )
  rec <- recognize_symbols_auto(
    split_board(prepare_board(padded, autocrop = TRUE)), cburnett_libs()
  )
  expect_equal(rec$symbols, truth)
  expect_true(rec$confident)
})
