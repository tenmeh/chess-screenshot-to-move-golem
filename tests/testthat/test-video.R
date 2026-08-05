# Reading a game out of a recording.
#
# The recordings here are synthesised, which is the only way to test this
# honestly: a checked-in video would be a black box, whereas a rendered one has
# a known answer, and the frames that must be *rejected* can be constructed
# deliberately rather than hoped for.

render_frames <- function(dir, game, ghost_at = NULL, dup = 1L, size = 320) {
  k <- 0L
  put <- function(fen) {
    k <<- k + 1L
    magick::image_write(
      render_position(fen_to_symbols(fen), size = size),
      file.path(dir, sprintf("f%03d.png", k))
    )
  }
  for (i in seq_len(nrow(game))) {
    if (identical(i, ghost_at)) {
      # A frame caught mid-animation: a piece has left its square and has not
      # arrived. No legal sequence explains it.
      syms <- fen_to_symbols(game$fen_before[i])
      syms[syms != "."][1] <- "."
      k <- k + 1L
      magick::image_write(
        render_position(syms, size = size), file.path(dir, sprintf("f%03d.png", k))
      )
    }
    put(game$fen_after[i])
    # Nothing happens between moves, so the same picture repeats.
    for (d in seq_len(dup)) put(game$fen_after[i])
  }
  invisible(k)
}

test_that("a game is recovered from a folder of frames", {
  truth <- read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")
  dir <- withr::local_tempdir()
  render_frames(dir, truth)

  got <- read_video(dir, piece_sets = cburnett_libs())

  expect_s3_class(got, "tanmai_game")
  expect_equal(got$san, truth$san)
  expect_equal(
    fen_placement(got$fen_after[nrow(got)]),
    fen_placement(truth$fen_after[nrow(truth)])
  )
})

test_that("a recording produces the same object a PGN does", {
  # The point of the whole design: evaluate(), accuracy() and the rest are
  # written once and cannot tell where the game came from.
  truth <- read_pgn("1. e4 e5 2. Nf3 Nc6")
  dir <- withr::local_tempdir()
  render_frames(dir, truth)

  got <- read_video(dir, piece_sets = cburnett_libs())
  expect_equal(names(got), names(truth))
  expect_equal(class(got), class(truth))
})

test_that("castling survives the round trip", {
  # Castling moves two pieces at once, so a tracker that reasons one piece at
  # a time gets it wrong, and it is the move most likely to be silently lost.
  truth <- read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. O-O Nf6")
  dir <- withr::local_tempdir()
  render_frames(dir, truth)

  got <- read_video(dir, piece_sets = cburnett_libs())
  expect_true("O-O" %in% got$san)
  expect_equal(got$san, truth$san)
})

test_that("frames caught mid-animation are thrown away, not believed", {
  truth <- read_pgn("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6")
  dir <- withr::local_tempdir()
  render_frames(dir, truth, ghost_at = 3L)

  got <- read_video(dir, piece_sets = cburnett_libs())

  # The game must come out intact despite the corrupt frame...
  expect_equal(got$san, truth$san)
  # ...and the frame must be accounted for rather than silently dropped.
  ledger <- attr(got, "ledger")
  expect_true("unexplained" %in% ledger$outcome)
})

test_that("unchanged frames are skipped rather than recognised again", {
  # Recognition is orders of magnitude more expensive than comparing two
  # thumbnails, and in a real recording most frames are the previous frame.
  # If this stops working the function still returns the right answer, just
  # many times slower - so it needs a test of its own.
  truth <- read_pgn("1. e4 e5 2. Nf3 Nc6")
  dir <- withr::local_tempdir()
  render_frames(dir, truth, dup = 3L)

  got <- read_video(dir, piece_sets = cburnett_libs())
  ledger <- attr(got, "ledger")
  skipped <- ledger$frames[ledger$outcome == "skipped"]

  expect_equal(got$san, truth$san)
  expect_gt(skipped, nrow(truth)) # most of the recording, not a handful
})

test_that("the frame comparison separates a real move from compression noise", {
  # The threshold these depend on was calibrated, not guessed: identical
  # renderings and a jpeg round trip both differ by 0, the smallest real
  # change (a pawn advancing) by about 0.12.
  a <- render_position(fen_to_symbols("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"), size = 320)
  same <- render_position(fen_to_symbols("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"), size = 320)
  pawn <- render_position(fen_to_symbols("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"), size = 320)

  sig_a <- frame_signature(a)
  expect_lt(max(abs(sig_a - frame_signature(same))), FRAME_CHANGE_GATE)
  expect_gt(max(abs(sig_a - frame_signature(pawn))), FRAME_CHANGE_GATE * 3)

  # A jpeg round trip must not read as a change.
  f <- withr::local_tempfile(fileext = ".jpg")
  magick::image_write(a, f, quality = 70)
  expect_lt(max(abs(sig_a - frame_signature(magick::image_read(f)))), FRAME_CHANGE_GATE)
})

test_that("a recording with nothing readable says so", {
  dir <- withr::local_tempdir()
  magick::image_write(
    magick::image_blank(320, 320, color = "grey40"),
    file.path(dir, "f001.png")
  )
  expect_error(read_video(dir, piece_sets = cburnett_libs()), "readable board")
})

test_that("missing and empty inputs are reported", {
  expect_error(read_video(file.path(tempdir(), "definitely-absent.gif")), "No such file")

  empty <- withr::local_tempdir()
  expect_true(dir.exists(empty))
  expect_error(read_video(empty), "No image files")
})

test_that("a video format needs ffmpeg, and says so when it is missing", {
  skip_if(nzchar(Sys.which("ffmpeg")), "ffmpeg is installed, so this path is not taken")
  f <- withr::local_tempfile(fileext = ".mp4")
  file.create(f)
  expect_error(read_video(f), "ffmpeg")
})
