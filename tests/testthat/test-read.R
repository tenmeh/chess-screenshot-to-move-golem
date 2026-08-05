# The public API.
#
# What matters here is not that each adapter works in isolation but that they
# agree: a position read from a screenshot and the same position read from its
# FEN must be the same row shape, because everything downstream is written once
# against that shape and never asks which adapter produced it.

test_that("read_fen returns one position row", {
  p <- read_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  expect_s3_class(p, "tanmai_position")
  expect_s3_class(p, "data.frame") # must stay an ordinary data frame
  expect_equal(nrow(p), 1L)
  expect_equal(p$turn, "w")
  expect_true(p$valid)
  expect_equal(p$source, "fen")
})

test_that("a FEN with the trailing fields trimmed is accepted", {
  # This is the bug the API surfaced: chess.js validates strictly and rejects
  # anything short of six fields, but sites copy four to the clipboard. The
  # app's FEN box claimed to accept them and did not.
  four <- "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq -"
  expect_no_error(read_fen(four))
  expect_equal(read_fen(four)$fen, paste(four, "0 1"))

  # Placement alone means White to move, no rights claimed.
  bare <- read_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
  expect_equal(bare$fen, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")

  # Castling must never be invented: a FEN that omits it is not claiming it.
  expect_match(bare$fen, " - - ")
})

test_that("fen_complete leaves a full FEN alone", {
  full <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq e3 5 12"
  expect_equal(fen_complete(full), full)
})

test_that("an unreadable FEN is refused, not quietly accepted", {
  expect_error(read_fen("rubbish"), "Not a readable FEN")
  expect_error(read_fen(""), "non-empty")
  expect_error(read_fen(c("a", "b")), "single")
})

test_that("read_board recovers a position it was given", {
  placement <- "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R"
  board <- render_position(fen_to_symbols(placement), size = 512)

  p <- read_board(board, piece_sets = cburnett_libs())

  expect_s3_class(p, "tanmai_position")
  expect_equal(sub(" .*$", "", p$fen), placement)
  expect_equal(p$source, "screenshot")
  # The confidence fields are the reason this adapter differs from read_fen,
  # and they must be populated rather than left NA.
  expect_true(p$confident)
  expect_false(is.na(p$median_occ))
  expect_equal(p$piece_set, "cburnett")
})

test_that("both position adapters produce the same columns", {
  placement <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  from_fen <- read_fen(placement)
  from_img <- read_board(
    render_position(fen_to_symbols(placement), size = 512),
    piece_sets = cburnett_libs()
  )
  expect_equal(names(from_fen), names(from_img))
  expect_equal(
    sub(" .*$", "", from_fen$fen),
    sub(" .*$", "", from_img$fen)
  )
})

test_that("castling is inferred from a picture but never from a short FEN", {
  # A real asymmetry between the adapters, and the right one.
  #
  # A picture has no castling field at all, so inferring it from where the
  # kings and rooks stand is the only information available - and it is what a
  # human reading the diagram would assume.
  #
  # A FEN that omits the field is a different thing: it is a statement that
  # said nothing about castling, and filling in KQkq would silently grant
  # rights the position never claimed. So the two disagree here on purpose.
  placement <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

  expect_match(read_fen(placement)$fen, " w - - ")
  expect_match(
    read_board(
      render_position(fen_to_symbols(placement), size = 512),
      piece_sets = cburnett_libs()
    )$fen,
    " w KQkq "
  )
})

test_that("read_pgn gives one row per ply, in the documented shape", {
  g <- read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")

  expect_s3_class(g, "tanmai_game")
  expect_equal(nrow(g), 6L)
  expect_equal(
    names(g),
    c(
      "ply", "move_no", "side", "san", "uci", "fen_before", "fen_after",
      "cp", "cp_loss", "best_uci", "best_san", "class", "human_p"
    )
  )
  expect_equal(g$san, c("e4", "e5", "Nf3", "Nc6", "Bb5", "a6"))
  expect_equal(g$move_no, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(g$side, c("w", "b", "w", "b", "w", "b"))

  # Reading a game must never need an engine, so these stay empty until
  # evaluate() is called.
  expect_true(all(is.na(g$cp)))
  expect_true(all(is.na(g$class)))
})

test_that("fen_before and fen_after really chain", {
  # The off-by-one that would misattribute every evaluation in the game: each
  # row's fen_after must be the next row's fen_before.
  g <- read_pgn("1. d4 d5 2. c4 e6 3. Nc3 Nf6")
  expect_equal(g$fen_after[-nrow(g)], g$fen_before[-1])
})

test_that("a game starting from a set-up position is not replayed from the start", {
  setup <- paste(
    '[SetUp "1"]', '[FEN "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1"]', "",
    "1. e4 Kd7 2. e5 Ke6 *",
    sep = "\n"
  )
  g <- read_pgn(setup)
  expect_equal(g$fen_before[1], "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1")
  expect_equal(nrow(g), 4L)
})

test_that("read_pgn accepts a file as readily as a string", {
  f <- withr::local_tempfile(fileext = ".pgn")
  writeLines("1. e4 e5 2. Nf3 Nc6", f)
  expect_equal(read_pgn(f)$san, c("e4", "e5", "Nf3", "Nc6"))
})

test_that("unusable PGN is reported rather than half-read", {
  expect_error(read_pgn(""), "Could not read")
  expect_error(read_pgn("1. e4 e5 2. Qxq9 Nc6"), "Could not read")
})

test_that("fen_to_symbols expands a placement field", {
  syms <- fen_to_symbols("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
  expect_length(syms, 64L)
  expect_equal(syms[1:8], c("r", "n", "b", "q", "k", "b", "n", "r"))
  expect_equal(syms[17:24], rep(".", 8))
  expect_error(fen_to_symbols("8/8/8"), "8 ranks")
})

test_that("plotting a game refuses before it has been evaluated", {
  # Better to say why than to draw an empty panel.
  g <- read_pgn("1. e4 e5")
  expect_error(plot(g), "evaluate")
})
