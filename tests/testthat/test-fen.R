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

test_that("a FEN with the trailing fields trimmed is completed, not rejected", {
  # The bug: the FEN box claimed since 1.3.0 that chess.js "fills in the fields
  # people habitually leave off". It does the opposite - it validates strictly
  # and rejects anything short of six fields - so pasting what Lichess and
  # Chess.com actually put on the clipboard failed with "must contain six
  # space-delimited fields".
  ctx <- new_chess_context()
  four <- "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -"

  expect_false(isTRUE(validate_fen(ctx, four)$ok)) # chess.js on its own
  expect_true(isTRUE(validate_fen(ctx, fen_complete(four))$ok)) # after completion
  expect_equal(fen_complete(four), paste(four, "0 1"))

  # Placement alone means White to move and no rights claimed. Castling is
  # never invented: a FEN that omits the field is not claiming those rights.
  bare <- fen_complete("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
  expect_equal(bare, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
  expect_true(isTRUE(validate_fen(ctx, bare)$ok))

  # A complete FEN is passed through untouched.
  full <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq e3 5 12"
  expect_equal(fen_complete(full), full)
})
