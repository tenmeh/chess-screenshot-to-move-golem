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

test_that("an explicitly configured engine path takes precedence", {
  fake <- tempfile(fileext = ".bin")
  file.create(fake)
  on.exit(unlink(fake), add = TRUE)

  withr_env <- Sys.getenv("CHESSVISION_STOCKFISH", unset = NA)
  Sys.setenv(CHESSVISION_STOCKFISH = fake)
  on.exit(
    {
      if (is.na(withr_env)) {
        Sys.unsetenv("CHESSVISION_STOCKFISH")
      } else {
        Sys.setenv(CHESSVISION_STOCKFISH = withr_env)
      }
    },
    add = TRUE
  )

  expect_equal(find_local_engine(), fake)

  # A configured path that does not exist must be ignored, not returned.
  Sys.setenv(CHESSVISION_STOCKFISH = file.path(tempdir(), "definitely-absent"))
  expect_false(identical(
    find_local_engine(),
    file.path(tempdir(), "definitely-absent")
  ))
})

test_that("confidence gates reject art the installed sets cannot explain", {
  # Scribble over every square: nothing in the library can explain it, so the
  # typical-square score must collapse and confidence must be withheld.
  noise <- magick::image_blank(512, 512, color = "#888888")
  noise <- magick::image_noise(noise, noisetype = "gaussian")
  rec <- recognize_symbols_auto(
    split_board(prepare_board(noise, autocrop = FALSE)), cburnett_libs()
  )
  expect_false(rec$confident)
})

test_that("a board cropped a pixel short is still read confidently", {
  # The regression this pins: the gate used to be the *worst* square's score,
  # which measures how well the crop lines up rather than whether the piece set
  # is known. A 280px board cropped one pixel short reads all 64 squares
  # correctly and yet scored 0.44 on that statistic - under the old 0.60 gate -
  # so the app warned "unreliable guess" on essentially every real screenshot,
  # since the board edge is only ever found to within a pixel or two.
  syms <- fen_symbols(MID_FEN)

  for (px in c(512, 280)) {
    board <- board_of(syms, size = px)
    clipped <- magick::image_crop(
      board, magick::geometry_area(px - 1, px - 1, 1, 1)
    )
    rec <- recognize_symbols_auto(
      split_board(prepare_board(clipped, autocrop = FALSE)), cburnett_libs()
    )
    info <- sprintf("%dpx board, 1px crop offset", px)
    # Recognition genuinely survives the offset...
    expect_equal(rec$symbols, syms, info = info)
    # ...so the app must not tell the user it is an unreliable guess.
    expect_true(rec$confident, info = info)
  }
})

test_that("a piece set that is not installed is still refused", {
  # The other half of the contract. Loosening the gate is only safe if art we
  # have never seen still fails it - otherwise the app would confidently report
  # a wrong FEN, which is worse than warning too often. Rendered with a real
  # set's art and scored against a library that does not contain it.
  other <- Filter(
    function(s) !is.null(piece_set_path(s)),
    c("merida", "alpha", "staunty", "horsey", "pixel", "xkcd")
  )
  skip_if(length(other) == 0, "no downloaded piece sets to hold out")

  syms <- fen_symbols(MID_FEN)
  for (set in other) {
    board <- render_position(syms, size = 512, set_dir = piece_set_path(set))
    rec <- recognize_symbols_auto(
      split_board(prepare_board(board, autocrop = FALSE)), cburnett_libs()
    )
    expect_false(rec$confident, info = paste(set, "held out"))
  }
})

test_that("the confidence signal survives a board that is not 512px", {
  # Screenshots come at whatever size the site rendered at, scaled by the
  # display's DPR. Every one of these reads correctly, so every one must be
  # reported as confident.
  syms <- fen_symbols(MID_FEN)
  for (px in c(720, 400, 320, 256, 200)) {
    rec <- recognize_symbols_auto(squares_of(syms, size = px), cburnett_libs())
    expect_equal(rec$symbols, syms, info = paste0(px, "px"))
    expect_true(rec$confident, info = paste0(px, "px"))
  }
})
