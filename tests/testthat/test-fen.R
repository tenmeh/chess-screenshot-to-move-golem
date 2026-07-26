# FEN assembly from a recognized grid.

test_that("a symbol grid round-trips through the placement field", {
  expect_equal(grid_to_placement(fen_symbols(MID_FEN)), MID_FEN)
  expect_equal(
    grid_to_placement(START_SYMBOLS),
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  )
})

test_that("an empty board is all eights", {
  expect_equal(grid_to_placement(rep(".", 64)), "8/8/8/8/8/8/8/8")
})

test_that("a wrong-sized grid is rejected rather than silently truncated", {
  expect_error(grid_to_placement(rep(".", 63)), "64")
})

test_that("castling rights need both king and rook at home", {
  expect_equal(infer_castling(START_SYMBOLS), "KQkq")

  # Move only white's king: white loses both rights, black keeps its own.
  moved_king <- START_SYMBOLS
  moved_king[sq_idx(7, 4)] <- "."
  expect_equal(infer_castling(moved_king), "kq")

  # Remove black's a8 rook: black loses the queenside right only.
  no_ra8 <- START_SYMBOLS
  no_ra8[sq_idx(0, 0)] <- "."
  expect_equal(infer_castling(no_ra8), "KQk")

  expect_equal(infer_castling(rep(".", 64)), "-")
})

test_that("flip assembles the FEN from black's perspective", {
  symbols <- fen_symbols(MID_FEN)
  # A board seen from black is the reversed grid; flipping must undo that.
  expect_equal(build_fen(rev(symbols), flip = TRUE)$placement, MID_FEN)
  expect_equal(build_fen(symbols, flip = FALSE)$placement, MID_FEN)
})

test_that("the side to move appears in the FEN", {
  expect_match(build_fen(START_SYMBOLS, turn = "w")$fen, " w ")
  expect_match(build_fen(START_SYMBOLS, turn = "b")$fen, " b ")
})
