# Recognition and the confidence gate that keeps a wrong reading from being
# presented as fact.

test_that("the bundled set reads its own boards exactly", {
  libs <- cburnett_libs()
  for (placement in c(MID_FEN, "8/5k2/6p1/8/8/2K5/5R2/8", "1n2k3/PP6/8/8/8/8/6pp/4K1N1")) {
    truth <- fen_symbols(placement)
    rec <- recognize_symbols_auto(squares_of(truth), libs)
    expect_equal(rec$symbols, truth, info = placement)
    expect_equal(rec$set, "cburnett", info = placement)
    expect_true(rec$confident, info = placement)
  }
})

test_that("empty squares are reported as empty", {
  rec <- recognize_symbols_auto(squares_of(rep(".", 64)), cburnett_libs())
  expect_equal(rec$symbols, rep(".", 64))
})

test_that("calibrating on a rendered start position yields 12 templates", {
  lib <- build_from_start_position(squares_of(START_SYMBOLS))
  expect_length(lib$pieces, 12)
  expect_setequal(names(lib$pieces), PIECE_SYMBOLS)
  expect_gt(lib$empty_threshold, 0)
})

test_that("a template library survives a save/load round trip", {
  lib <- build_from_start_position(squares_of(START_SYMBOLS))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  save_template_library(lib, path)

  loaded <- load_template_library(path)
  expect_equal(loaded$empty_threshold, lib$empty_threshold)
  expect_setequal(names(loaded$pieces), names(lib$pieces))
  # And it must still read a board correctly.
  expect_equal(recognize_symbols(squares_of(START_SYMBOLS), loaded), START_SYMBOLS)
})

test_that("recognition needs at least one template library", {
  expect_error(recognize_symbols_auto(squares_of(START_SYMBOLS), list()), "no template")
})

test_that("confidence gates reject art the installed sets cannot explain", {
  # Scribble over every square: nothing in the library can explain it, so the
  # weakest-square score must collapse and confidence must be withheld.
  noise <- magick::image_blank(512, 512, color = "#888888")
  noise <- magick::image_noise(noise, noisetype = "gaussian")
  rec <- recognize_symbols_auto(
    split_board(prepare_board(noise, autocrop = FALSE)), cburnett_libs()
  )
  expect_false(rec$confident)
})
