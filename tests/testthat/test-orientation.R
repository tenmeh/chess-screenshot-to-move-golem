# Regression tests for the orientation failure that produced colour-swapped,
# 180-degree-wrong FENs on black-perspective screenshots.

test_that("piece brightness identifies which end holds the white army", {
  white_bottom <- squares_of(START_SYMBOLS)
  black_bottom <- squares_of(rev(START_SYMBOLS))

  expect_false(detect_flip_start_brightness(white_bottom))
  expect_true(detect_flip_start_brightness(black_bottom))
})

test_that("army positions identify orientation for a played position", {
  symbols <- fen_symbols(MID_FEN)

  expect_false(detect_flip_piece_mass(symbols))
  expect_true(detect_flip_piece_mass(rev(symbols)))
})

test_that("army positions abstain rather than guess when a side is absent", {
  only_white <- c(rep(".", 56), rep("P", 8))
  expect_true(is.na(detect_flip_piece_mass(only_white)))
  expect_true(is.na(detect_flip_piece_mass(character(0))))
})

test_that("calibrating a black-perspective board does not invert the colours", {
  # The exact failure: without a reliable flip signal the templates were built
  # from the opposing colour, silently poisoning every later reading.
  squares <- squares_of(rev(START_SYMBOLS))
  ori <- resolve_calibration_flip(squares, board_of(rev(START_SYMBOLS)), "auto")
  expect_true(ori$flip)

  lib <- build_from_start_position(rev(squares))
  guard <- fix_template_colour_swap(lib)
  expect_false(guard$swapped) # already the right way round

  # White templates must be brighter than black ones.
  kinds <- c("P", "N", "B", "R", "Q", "K")
  white <- mean(vapply(guard$lib$pieces[kinds], mean, numeric(1)))
  black <- mean(vapply(guard$lib$pieces[tolower(kinds)], mean, numeric(1)))
  expect_gt(white, black)
})

test_that("the colour-swap guard detects and repairs an inverted library", {
  # Build deliberately wrong: black-perspective squares, not reversed.
  inverted <- build_from_start_position(squares_of(rev(START_SYMBOLS)))
  guard <- fix_template_colour_swap(inverted)
  expect_true(guard$swapped)

  # Repair must be stable, not oscillate.
  expect_false(fix_template_colour_swap(guard$lib)$swapped)

  # And a correct library must be left alone.
  correct <- build_from_start_position(squares_of(START_SYMBOLS))
  expect_false(fix_template_colour_swap(correct)$swapped)
})

test_that("an explicit orientation choice overrides detection", {
  squares <- squares_of(START_SYMBOLS)
  board <- board_of(START_SYMBOLS)

  expect_false(resolve_calibration_flip(squares, board, "white")$flip)
  expect_true(resolve_calibration_flip(squares, board, "black")$flip)
})
