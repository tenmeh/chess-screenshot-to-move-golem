# Engine-backed analysis.
#
# Most of what can go wrong here does not need an engine to catch: sign
# conventions, thresholds, and refusing to answer when there is nothing to
# answer from. Those tests run everywhere. The ones that genuinely need
# Stockfish are guarded - and note that a skip is not a pass, so the test
# *count* is what tells you whether they ran.

scholars_mate <- "1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6 4. Qxf7#"

test_that("analysis refuses a game it has not been given evaluations for", {
  # Better to say why than to return a table of NA or an empty plot.
  g <- read_pgn("1. e4 e5 2. Nf3 Nc6")
  expect_error(accuracy(g), "not been evaluated")
  expect_error(turning_points(g), "not been evaluated")
  expect_error(plot(g), "evaluate")
})

test_that("analysis refuses things that are not games", {
  expect_error(accuracy(data.frame(x = 1)), "tanmai_game")
  expect_error(turning_points(data.frame(x = 1)), "tanmai_game")
})

test_that("move classification follows the thresholds players know", {
  played <- rep("e2e4", 6)
  best <- c("e2e4", "d2d4", "d2d4", "d2d4", "d2d4", "d2d4")
  cls <- classify_move(c(0, 10, 60, 120, 250, NA), played, best)

  # Matching the engine is "best" even though it lost nothing, because
  # "found the only move" and "did not lose anything measurable" are
  # different facts about a move.
  expect_equal(as.character(cls), c("best", "good", "inaccuracy", "mistake", "blunder", NA))
  expect_equal(levels(cls), c("best", "good", "inaccuracy", "mistake", "blunder"))
})

test_that("a finished position has a definite evaluation, not a missing one", {
  # The engine reports no principal variation for a position with no legal
  # move, which would leave the ply that *ended the game* blank - the single
  # most important row in the table.
  ctx <- new_chess_context()
  mate <- "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
  expect_equal(position_status(ctx, mate), "checkmate")
  # Black king boxed in but not in check: a draw, and worth exactly 0, which
  # is a different answer from "mated" and from "unknown".
  expect_equal(position_status(ctx, "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"), "stalemate")
  expect_equal(
    position_status(ctx, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
    "ongoing"
  )
})

test_that("evaluate fills the columns read_pgn leaves empty", {
  skip_if_not(has_engine(), "no Stockfish available")

  g <- evaluate(read_pgn(scholars_mate), movetime = 50L)

  expect_false(anyNA(g$cp))
  expect_false(anyNA(g$cp_loss))
  expect_false(anyNA(g$class))
  # cp_loss is a loss: it can be zero but never negative, whichever colour moved.
  expect_true(all(g$cp_loss >= 0))
})

test_that("cp is from White's point of view throughout", {
  skip_if_not(has_engine(), "no Stockfish available")

  # Scholar's mate ends with White mating, so the last evaluation must be
  # large and *positive* regardless of whose move it was. Getting this
  # backwards is the classic UCI bug: the engine answers from the side to
  # move's point of view, which flips every ply.
  g <- evaluate(read_pgn(scholars_mate), movetime = 50L)
  expect_gt(g$cp[nrow(g)], 5000)
  expect_equal(g$side[nrow(g)], "w")

  # And the mating move cost White nothing.
  expect_equal(g$cp_loss[nrow(g)], 0)
})

test_that("walking into mate is charged to the player who did it", {
  skip_if_not(has_engine(), "no Stockfish available")

  g <- evaluate(read_pgn(scholars_mate), movetime = 50L)
  # 3...Nf6 is the losing move: Black's, and by far the most expensive.
  worst <- turning_points(g, min_loss = 100, n = 1L)
  expect_equal(worst$side, "b")
  expect_equal(worst$san, "Nf6")
})

test_that("accuracy reports one row per side, on a 0-100 scale", {
  skip_if_not(has_engine(), "no Stockfish available")

  acc <- accuracy(evaluate(read_pgn(scholars_mate), movetime = 50L))

  expect_equal(nrow(acc), 2L)
  expect_equal(acc$side, c("w", "b"))
  expect_true(all(acc$accuracy >= 0 & acc$accuracy <= 100))
  expect_true(all(acc$acpl >= 0))
  # White played a forced mate; Black got mated. The ordering is not a matter
  # of opinion.
  expect_gt(acc$accuracy[acc$side == "w"], acc$accuracy[acc$side == "b"])
  expect_equal(sum(acc$moves), 7L)
})

test_that("evaluating a single position gives an evaluation and a best move", {
  skip_if_not(has_engine(), "no Stockfish available")

  # White to move with mate in one.
  p <- evaluate(
    read_fen("r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 4 4"),
    movetime = 200L
  )
  expect_gt(p$cp, 5000)
  expect_equal(p$best_uci, "f3f7")
  expect_equal(p$best_san, "Qxf7#")
})

test_that("an evaluated game can be plotted", {
  skip_if_not(has_engine(), "no Stockfish available")

  g <- evaluate(read_pgn(scholars_mate), movetime = 50L)
  f <- withr::local_tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(g))
})
