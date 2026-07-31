# Live game tracking. Everything here is pure move logic run through chess.js,
# so the suite needs no engine, no network and no piece art.

START <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#' Play a sequence of UCI moves and return the resulting FEN
play <- function(ctx, fen, moves) {
  for (m in moves) fen <- fen_after_move(ctx, fen, m)
  fen
}

test_that("a FEN's placement field is extracted, and the turn can be rewritten", {
  expect_equal(fen_placement(START), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
  expect_true(is.na(fen_placement(NA_character_)))
  expect_true(is.na(fen_placement("")))

  flipped <- fen_with_turn(START, "b")
  expect_equal(strsplit(flipped, " ")[[1]][2], "b")
  # The en-passant square belongs to whoever was on turn, so it is cleared.
  ep <- fen_with_turn("rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2", "b")
  expect_equal(strsplit(ep, " ")[[1]][4], "-")
})

test_that("a single legal move is recovered from the placement alone", {
  ctx <- new_chess_context()
  after <- fen_after_move(ctx, START, "e2e4")
  paths <- find_move_paths(ctx, START, fen_placement(after), max_plies = 2L)

  expect_length(paths, 1)
  expect_equal(paths[[1]], "e2e4")
})

test_that("castling, en passant and promotion are recovered as one move each", {
  ctx <- new_chess_context()

  # Castling moves two pieces at once; a naive square-diff would see two moves.
  castle_pos <- "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 0 1"
  target <- fen_placement(fen_after_move(ctx, castle_pos, "e1g1"))
  paths <- find_move_paths(ctx, castle_pos, target, max_plies = 1L)
  expect_length(paths, 1)
  expect_equal(paths[[1]], "e1g1")

  # En passant removes a pawn from a square the moving piece never touches.
  # The right to it exists only because the previous move was a double push,
  # which the tracker knows because it carries the game forward by playing
  # moves - a recognized screenshot could never show it.
  ep_pos <- play(ctx, START, c("e2e4", "a7a6", "e4e5", "d7d5"))
  expect_match(ep_pos, " d6 ") # the en-passant square really is set
  target <- fen_placement(fen_after_move(ctx, ep_pos, "e5d6"))
  paths <- find_move_paths(ctx, ep_pos, target, max_plies = 1L)
  expect_length(paths, 1)
  expect_equal(paths[[1]], "e5d6")

  # Promotion: the four promotion pieces give four different placements, so
  # the choice is readable off the board rather than guessed.
  promo_pos <- "8/P7/8/8/8/8/8/K6k w - - 0 1"
  q <- fen_placement(fen_after_move(ctx, promo_pos, "a7a8q"))
  n <- fen_placement(fen_after_move(ctx, promo_pos, "a7a8n"))
  expect_false(identical(q, n))
  expect_equal(find_move_paths(ctx, promo_pos, q, 1L)[[1]], "a7a8q")
  expect_equal(find_move_paths(ctx, promo_pos, n, 1L)[[1]], "a7a8n")
})

test_that("every move of a real game is uniquely recoverable one ply at a time", {
  # The gate's usefulness rests on single moves almost never being ambiguous.
  # Rather than assert that, play a game and check it holds at every ply.
  ctx <- new_chess_context()
  moves <- c(
    "e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "f8c5", "b2b4", "c5b4",
    "c2c3", "b4a5", "d2d4", "e5d4", "e1g1", "d4c3", "d1b3", "d8f6",
    "e4e5", "f6g6", "b1c3", "g8e7", "c1a3", "b7b5"
  )
  fen <- START
  for (m in moves) {
    target <- fen_placement(fen_after_move(ctx, fen, m))
    paths <- find_move_paths(ctx, fen, target, max_plies = 1L)
    expect_length(paths, 1)
    expect_equal(paths[[1]], m)
    fen <- fen_after_move(ctx, fen, m)
  }
})

test_that("a two-ply jump is inferred when a frame is missed", {
  ctx <- new_chess_context()
  after <- play(ctx, START, c("d2d4", "g8f6"))
  res <- track_observation(ctx, game_new(START, turn_pinned = TRUE), after, max_plies = 2L)

  expect_equal(res$status, "move")
  expect_equal(res$moves, c("d2d4", "g8f6"))

  # ...but not when only one ply is allowed.
  strict <- track_observation(ctx, game_new(START, turn_pinned = TRUE), after, max_plies = 1L)
  expect_equal(strict$status, "unexplained")
})

test_that("noise is rejected rather than accepted as a position", {
  ctx <- new_chess_context()
  g <- game_new(START, turn_pinned = TRUE)

  # A misread square: one black pawn read as a bishop. No legal move does that.
  misread <- "rnbqkbnr/pppppbpp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  res <- track_observation(ctx, g, misread, max_plies = 2L)
  expect_equal(res$status, "unexplained")
  expect_match(res$reason, "misread")

  # A position from another game entirely - reachable, but not from here.
  far <- "8/8/4k3/8/8/4K3/8/8 w - - 0 1"
  expect_equal(track_observation(ctx, g, far, max_plies = 2L)$status, "unexplained")

  # Structural garbage from a failed recognition.
  expect_equal(track_observation(ctx, g, "", max_plies = 2L)$status, "invalid")
  expect_equal(track_observation(ctx, g, NA_character_, max_plies = 2L)$status, "invalid")
})

test_that("an unchanged board and a rewound one are distinguished from a move", {
  ctx <- new_chess_context()
  g <- game_new(START, turn_pinned = TRUE)
  g <- game_accept(ctx, g, track_observation(ctx, g, play(ctx, START, "e2e4")))
  g <- game_accept(ctx, g, track_observation(ctx, g, play(ctx, START, c("e2e4", "e7e5"))))
  expect_equal(game_ply(g), 2)

  # The same frame captured twice.
  expect_equal(
    track_observation(ctx, g, game_current_fen(g))$status, "unchanged"
  )
  # A frame caught mid-animation still showing the previous position. Without
  # this the game would rewind, or the position would be reported as noise.
  expect_equal(
    track_observation(ctx, g, play(ctx, START, "e2e4"))$status, "stale"
  )
  expect_equal(track_observation(ctx, g, START)$status, "stale")
})

test_that("competing explanations are refused, not guessed between", {
  ctx <- new_chess_context()
  # Ambiguity needs an odd-length window, where the same two moves by one side
  # can be played in either order. A sparse endgame keeps the search small.
  pos <- "8/8/8/8/8/8/1P3P2/4K2k w - - 0 1"
  target <- fen_placement(play(ctx, pos, c("b2b3", "h1h2", "f2f3")))
  paths <- find_move_paths(ctx, pos, target, max_plies = 3L)

  expect_gt(length(paths), 1) # both orderings reach it
  expect_true(all(lengths(paths) == 3))

  res <- track_observation(ctx, game_new(pos, turn_pinned = TRUE), target, max_plies = 3L)
  expect_equal(res$status, "ambiguous")
  expect_equal(res$moves, character(0))
})

test_that("a shorter explanation wins over a longer one", {
  ctx <- new_chess_context()
  # Reaching the position after 1.e4 also has three-ply accounts (a knight out
  # and back around it). The one-move reading is the right one.
  target <- fen_placement(play(ctx, START, "e2e4"))
  res <- track_observation(ctx, game_new(START, turn_pinned = TRUE), target, max_plies = 3L)
  expect_equal(res$status, "move")
  expect_equal(res$moves, "e2e4")
})

test_that("a wrongly guessed side to move is corrected by the first move", {
  ctx <- new_chess_context()
  # A screenshot cannot show whose turn it is, so the anchor's turn field is a
  # guess from the UI. Here it says White, but Black is actually to move.
  anchor <- "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1"
  g <- game_new(anchor, turn_pinned = FALSE)

  observed <- play(ctx, fen_with_turn(anchor, "b"), "e7e5")
  res <- track_observation(ctx, g, observed, max_plies = 2L)

  expect_equal(res$status, "move")
  expect_true(res$turn_flipped)
  expect_equal(res$moves, "e7e5")

  g <- game_accept(ctx, g, res)
  expect_equal(game_ply(g), 1)
  expect_equal(g$sans, "e5") # attributed to Black, not White
  expect_true(g$turn_pinned)
  # The corrected anchor replaced the guess, so the record is self-consistent.
  expect_equal(strsplit(g$fens[1], " ")[[1]][2], "b")
  expect_equal(fen_placement(game_current_fen(g)), fen_placement(observed))
})

test_that("the side to move is not re-flipped once a move has confirmed it", {
  ctx <- new_chess_context()
  g <- game_new(START, turn_pinned = TRUE)
  # Black cannot move first; with the turn pinned this must be rejected rather
  # than silently reinterpreted.
  observed <- play(ctx, fen_with_turn(START, "b"), "e7e5")
  expect_equal(track_observation(ctx, g, observed, max_plies = 1L)$status, "unexplained")
})

test_that("accepted moves build a game record with SAN", {
  ctx <- new_chess_context()
  g <- game_new(START, turn_pinned = TRUE)
  for (mv in list("e2e4", c("e7e5"), "g1f3")) {
    fen <- play(ctx, game_current_fen(g), mv)
    g <- game_accept(ctx, g, track_observation(ctx, g, fen))
  }
  expect_equal(g$sans, c("e4", "e5", "Nf3"))
  expect_equal(g$ucis, c("e2e4", "e7e5", "g1f3"))
  expect_length(g$fens, 4)
  expect_length(g$cp, 4)
})

test_that("a rendered game is followed move by move through real recognition", {
  # The gate is only worth anything if it survives the actual recognizer
  # rather than hand-written FENs. Render each position as a board image, read
  # it back the way a screenshot would be read, and check the game is tracked.
  ctx <- new_chess_context()
  libs <- cburnett_libs()
  moves <- c("e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "g8f6", "f3g5", "d7d5")

  read_board <- function(fen) {
    img <- board_of(fen_symbols(fen_placement(fen)))
    recognize_position(img, libs, ctx, turn = "w", autocrop = TRUE, orientation = "white")$fen
  }

  g <- game_new(START, turn_pinned = TRUE)
  truth <- START
  for (m in moves) {
    truth <- fen_after_move(ctx, truth, m)
    res <- track_observation(ctx, g, read_board(truth), max_plies = 2L)
    expect_equal(res$status, "move")
    g <- game_accept(ctx, g, res)
  }

  expect_equal(game_ply(g), length(moves))
  expect_equal(g$ucis, moves)
  expect_equal(g$sans, c("e4", "e5", "Nf3", "Nc6", "Bc4", "Nf6", "Ng5", "d5"))
  expect_equal(fen_placement(game_current_fen(g)), fen_placement(truth))
  # The tracked FEN is the real one, not a reconstruction: state a screenshot
  # cannot show is carried forward by playing the moves. Castling rights here
  # survive because they were never lost, rather than because a heuristic saw
  # the kings and rooks at home.
  expect_equal(game_current_fen(g), truth)
})

test_that("a frame that skips a ply is still followed, and a corrupted one is not", {
  ctx <- new_chess_context()
  libs <- cburnett_libs()

  read_board <- function(fen) {
    img <- board_of(fen_symbols(fen_placement(fen)))
    recognize_position(img, libs, ctx, turn = "w", autocrop = TRUE, orientation = "white")$fen
  }

  g <- game_new(START, turn_pinned = TRUE)
  two_on <- play(ctx, START, c("d2d4", "d7d5"))
  res <- track_observation(ctx, g, read_board(two_on), max_plies = 2L)
  expect_equal(res$status, "move")
  expect_equal(res$moves, c("d2d4", "d7d5"))

  # Now a frame where one square came back wrong - the single most common
  # recognition failure. It must be refused, not appended.
  syms <- fen_symbols(fen_placement(two_on))
  syms[sq_idx(4, 4)] <- "N" # a white knight materialises in mid-board
  bogus <- recognize_position(board_of(syms), libs, ctx,
    turn = "w", autocrop = TRUE, orientation = "white"
  )$fen
  expect_equal(track_observation(ctx, g, bogus, max_plies = 2L)$status, "unexplained")
})

test_that("positions are compared without the move clocks getting in the way", {
  # The same board arrives one way from chess.js and another from a
  # screenshot; only the counters differ, and they must not count.
  expect_true(same_position(START, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 9 40"))
  expect_false(same_position(START, fen_with_turn(START, "b")))
  expect_false(same_position(START, "8/8/8/8/8/8/8/8 w - - 0 1"))
  expect_false(same_position(NULL, START))
  expect_false(same_position(NA_character_, START))
})

test_that("the eval graph plots one point per position and can be clicked", {
  svg <- as.character(eval_graph_svg(c(0, 50, -300, -280), click_input = "live-goto"))

  expect_match(svg, "<svg")
  expect_match(svg, "polyline")
  # One click target per position, each reporting its own ply index.
  expect_equal(lengths(regmatches(svg, gregexpr("cv-graph-hit", svg))), 4L)
  expect_match(svg, "\\{ply: 0,")
  expect_match(svg, "\\{ply: 3,")
  expect_match(svg, "live-goto")

  # Losing badly must sit lower on the graph than being level. Read the curve
  # itself, not the shaded band, which carries two extra baseline corners.
  line <- regmatches(svg, regexpr("<polyline points=\"[^\"]+\"", svg))
  xy <- as.numeric(strsplit(sub(".*points=\"", "", sub("\"$", "", line)), "[ ,]")[[1]])
  ys <- xy[seq(2, length(xy), by = 2)]

  expect_length(ys, 4)
  expect_lt(ys[2], ys[1]) # +0.50 sits above level
  expect_gt(ys[3], ys[1]) # -3.00 sits below it
  expect_lt(ys[4], ys[3]) # and -2.80 recovers slightly

  # Nothing measured yet, nothing drawn - rather than a flat line implying
  # the game is level.
  expect_null(eval_graph_svg(numeric(0)))
  expect_null(eval_graph_svg(c(NA_real_, NA_real_)))
  # A gap mid-game holds the last known value instead of breaking the line.
  expect_match(as.character(eval_graph_svg(c(0, NA, 100))), "polyline")
})

test_that("move quality follows the usual thresholds", {
  expect_equal(move_quality(c(0, 60, 140, 400)), c("ok", "inaccuracy", "mistake", "blunder"))
  expect_equal(move_quality(NA_real_), "ok")
})

test_that("walking into mate is described as mate, not as a hundred pawns", {
  # Mate scores are folded onto the centipawn scale so they outrank material.
  # Read back literally that makes a mate "lose 100.3 pawns", which is true and
  # useless.
  expect_equal(format_loss(140), "1.4 pawns")
  expect_equal(format_loss(0), "0.0 pawns")
  expect_equal(format_loss(10029), "gets mated")
  expect_equal(format_loss(2500), "loses outright")
  expect_equal(format_loss(NA_real_), "not scored")
  expect_length(format_loss(c(10, 10029, NA)), 3)
})

test_that("what each move cost is measured from the mover's point of view", {
  ctx <- new_chess_context()
  g <- game_new(START, turn_pinned = TRUE)
  for (mv in c("e2e4", "e7e5", "g1f3", "b8c6")) {
    g <- game_accept(ctx, g, track_observation(ctx, g, play(ctx, game_current_fen(g), mv)))
  }
  # Evaluations are stored from White's point of view throughout.
  #   ply 1 (White):  +20 -> +10   =>  White lost 10
  #   ply 2 (Black):  +10 -> +310  =>  Black lost 300
  #   ply 3 (White): +310 -> +300  =>  White lost 10
  #   ply 4 (Black): +300 -> +290  =>  Black gained 10, so lost nothing
  g$cp <- c(20, 10, 310, 300, 290)
  expect_equal(game_move_losses(g), c(10, 300, 10, 0))

  tp <- game_turning_points(g, min_loss = 100)
  expect_equal(nrow(tp), 1)
  expect_equal(tp$ply, 2)
  expect_equal(tp$mover, "b")
  expect_equal(tp$san, "e5")
  expect_equal(tp$quality, "blunder")
  expect_equal(tp$move_no, 1)

  # Nothing evaluated yet: no losses, and no turning points invented.
  g$cp <- rep(NA_real_, 5)
  expect_true(all(is.na(game_move_losses(g))))
  expect_equal(nrow(game_turning_points(g)), 0)
})
