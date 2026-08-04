# Importing a finished game from PGN.
#
# The parsing itself is chess.js's job. What is tested here is the contract the
# rest of the app depends on: that an imported game is shaped exactly like one
# built move by move, because every downstream feature - the evaluation graph,
# the turning points, the radar review, the rating estimator - reads that shape
# and none of them know where the game came from.

immortal <- paste(
  '[Event "Immortal Game"]',
  '[White "Anderssen, Adolf"]',
  '[Black "Kieseritzky, Lionel"]',
  '[Result "1-0"]',
  "",
  "1.e4 e5 2.f4 exf4 3.Bc4 Qh4+ 4.Kf1 b5 5.Bxb5 Nf6 6.Nf3 Qh6 7.d3 Nh5 8.Nh4 Qg5",
  "9.Nf5 c6 10.g4 Nf6 11.Rg1 cxb5 12.h4 Qg6 13.h5 Qg5 14.Qf3 Ng8 15.Bxf4 Qf6",
  "16.Nc3 Bc5 17.Nd5 Qxb2 18.Bd6 Bxg1 19.e5 Qxa1+ 20.Ke2 Na6 21.Nxg7+ Kd8",
  "22.Qf6+ Nxf6 23.Be7# 1-0",
  sep = "\n"
)

test_that("a game imports with one position per ply, plus the start", {
  ctx <- new_chess_context()
  res <- game_from_pgn(ctx, immortal)

  expect_null(res$error)
  expect_equal(res$n_moves, 45)

  g <- res$game
  # This invariant is the whole contract: fens is always moves + 1. Every
  # consumer indexes g$fens[i] as "the position before move i", so an
  # off-by-one here would misattribute every evaluation in the game.
  expect_equal(length(g$fens), length(g$ucis) + 1L)
  expect_equal(length(g$sans), length(g$ucis))
  expect_equal(length(g$cp), length(g$fens))
  expect_equal(length(g$best), length(g$fens))

  expect_equal(g$fens[1], "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  expect_equal(g$sans[1:4], c("e4", "e5", "f4", "exf4"))
  expect_equal(g$ucis[1], "e2e4")
  expect_true(all(is.na(g$cp))) # nothing scored yet
  expect_true(g$turn_pinned) # a PGN says whose move it is
})

test_that("the recorded positions really are the game", {
  ctx <- new_chess_context()
  g <- game_from_pgn(ctx, immortal)$game

  # Replay independently: every stored FEN must be what playing the stored
  # moves actually produces. Storing a plausible-looking but wrong sequence
  # would still pass the length checks above.
  fen <- g$fens[1]
  for (i in seq_along(g$ucis)) {
    expect_equal(fen, g$fens[i])
    fen <- fen_after_move(ctx, fen, g$ucis[i])
    expect_false(is.na(fen))
  }
  expect_equal(fen_placement(fen), fen_placement(g$fens[length(g$fens)]))
})

test_that("a game starting from a set-up position is not replayed from the start", {
  ctx <- new_chess_context()
  # A [FEN] header means the game does not begin from the initial position.
  # Ignoring it would silently produce a completely different game that still
  # parses, which is the worst kind of wrong.
  setup <- paste(
    '[SetUp "1"]',
    '[FEN "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1"]',
    "",
    "1. e4 Kd7 2. e5 Ke6 *",
    sep = "\n"
  )
  res <- game_from_pgn(ctx, setup)

  expect_null(res$error)
  expect_equal(res$game$fens[1], "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1")
  expect_equal(res$n_moves, 4)
  expect_equal(res$game$sans[1], "e4")
})

test_that("bare movetext with no headers works", {
  ctx <- new_chess_context()
  res <- game_from_pgn(ctx, "1. d4 d5 2. c4 e6 3. Nc3 Nf6")
  expect_null(res$error)
  expect_equal(res$n_moves, 6)
  expect_equal(res$game$sans[6], "Nf6")
})

test_that("comments, annotations and variations are tolerated", {
  ctx <- new_chess_context()
  res <- game_from_pgn(
    ctx,
    "1. e4 {best by test} e5 2. Nf3! Nc6?! (2... d6 3. d4) 3. Bb5 a6 *"
  )
  expect_null(res$error)
  # The main line only - a variation is not part of the game that was played.
  expect_equal(res$game$sans, c("e4", "e5", "Nf3", "Nc6", "Bb5", "a6"))
})

test_that("unusable input is reported rather than half-imported", {
  ctx <- new_chess_context()

  expect_match(game_from_pgn(ctx, "")$error, "No PGN", ignore.case = TRUE)
  expect_match(game_from_pgn(ctx, "   ")$error, "No PGN", ignore.case = TRUE)

  # Headers but no moves: parses fine, and is still not a game.
  headers_only <- '[Event "Nothing"]\n[White "A"]\n\n*'
  expect_false(is.null(game_from_pgn(ctx, headers_only)$error))

  # An illegal move must not yield a truncated game presented as complete.
  bad <- game_from_pgn(ctx, "1. e4 e5 2. Qxq9 Nc6")
  expect_false(is.null(bad$error))
  expect_null(bad$game)
})

test_that("the summary line survives missing headers", {
  expect_match(pgn_summary(c(White = "Carlsen", Black = "Nakamura"), 60), "Carlsen vs Nakamura")
  expect_match(pgn_summary(c(White = "Carlsen", Black = "Nakamura"), 60), "60 plies")
  # "?" is PGN's own placeholder for unknown and must not be shown as a name.
  expect_match(pgn_summary(c(White = "?", Black = "?", Event = "Casual"), 20), "Casual")
  expect_type(pgn_summary(character(0), 20), "character")
})
