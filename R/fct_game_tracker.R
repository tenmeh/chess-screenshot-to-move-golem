# Live game tracking: turn a stream of imperfect screenshots into a game.
#
# Recognition is imperfect and a screen capture is noisy: it delivers
# re-renders, half-finished piece animations, hover highlights and the
# occasional misread square. Accepting whatever FEN comes back would produce a
# "game" that is mostly garbage.
#
# So nothing is accepted on the strength of recognition alone. A new
# observation is only believed when it can be *explained*: when there is
# exactly one sequence of legal moves, of at most a couple of plies, that turns
# the position we are already sure about into the one we just saw. That single
# rule does the work of a dozen ad-hoc filters - it rejects noise, tolerates a
# frame arriving mid-animation, survives a dropped frame, and gets castling,
# en passant and promotion right for free because chess.js generates the moves.

#' The standard starting position
#'
#' @return The FEN of the initial position.
#' @noRd
CV_START_FEN <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#' The piece-placement field of a FEN
#'
#' @param fen A FEN string.
#' @return The placement field (everything before the first space), or `NA` for
#'   an empty/missing input.
fen_placement <- function(fen) {
  if (is.null(fen) || !length(fen) || is.na(fen) || !nzchar(fen)) {
    return(NA_character_)
  }
  strsplit(fen, " ", fixed = TRUE)[[1]][1]
}

#' Rewrite a FEN's side to move
#'
#' The en-passant square is cleared at the same time: it belongs to the side
#' that was about to move, and is meaningless once the other side is on turn.
#'
#' @param fen A FEN string.
#' @param turn "w" or "b".
#' @return The FEN with its turn field replaced.
fen_with_turn <- function(fen, turn) {
  f <- strsplit(fen, " ", fixed = TRUE)[[1]]
  if (length(f) < 4) {
    return(NA_character_)
  }
  f[2] <- turn
  f[4] <- "-"
  paste(f, collapse = " ")
}

#' Test whether two FENs describe the same position to play from
#'
#' Compares the placement and the side to move, and ignores the move clocks.
#' Two FENs for the same board can differ in the halfmove counter alone - one
#' arrived at by playing moves, the other assembled from a screenshot - and
#' treating those as different positions would break every comparison.
#'
#' @param a,b FEN strings.
#' @return `TRUE` if both describe the same position and side to move.
same_position <- function(a, b) {
  if (is.null(a) || is.null(b) || !length(a) || !length(b) || is.na(a) || is.na(b)) {
    return(FALSE)
  }
  fa <- strsplit(a, " ", fixed = TRUE)[[1]]
  fb <- strsplit(b, " ", fixed = TRUE)[[1]]
  length(fa) >= 2 && length(fb) >= 2 && identical(fa[1:2], fb[1:2])
}

#' Define the path-search helper inside a chess.js context
#'
#' The search runs entirely in V8 rather than looping over [fen_after_move()]
#' from R. At two plies that is over a thousand positions, and one V8 round trip
#' each would turn a 0.1s check into a visible stall in a live session.
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @return Invisibly `ctx`.
#' @noRd
ensure_tracker_js <- function(ctx) {
  if (isTRUE(ctx$get("typeof cvFindPaths === 'function'"))) {
    return(invisible(ctx))
  }
  ctx$eval("
  function cvFindPaths(fen, target, maxPlies, cap) {
    var out = [];
    var g;
    try { g = new ChessCtor(fen); } catch (e) { return out; }
    function rec(path) {
      // A match ends this branch: we want the shortest explanation, and
      // continuing would only find longer ways to say the same thing.
      if (path.length > 0 && g.fen().split(' ')[0] === target) {
        out.push(path.slice());
        return;
      }
      if (path.length >= maxPlies || out.length >= cap) return;
      var ms = g.moves({verbose: true});
      for (var i = 0; i < ms.length; i++) {
        var m = ms[i];
        var uci = m.from + m.to + (m.promotion ? m.promotion : '');
        try {
          g.move({from: m.from, to: m.to, promotion: m.promotion});
        } catch (e) { continue; }
        path.push(uci);
        rec(path);
        path.pop();
        g.undo();
      }
    }
    rec([]);
    return out;
  }")
  invisible(ctx)
}

#' Every short legal path from one position to a target placement
#'
#' Searches all legal move sequences up to `max_plies` long and returns those
#' that reach `target_placement`. Only the placement is compared, because that
#' is all a screenshot can show - side to move, castling rights, en passant and
#' the move clocks are not visible on a board and are instead carried forward
#' by playing the moves.
#'
#' Cost grows steeply with depth: two plies is around 1200 positions (~0.1s),
#' three is around 40000 (~5s). Two is the useful setting - it covers a capture
#' frame arriving only after the reply - and the default reflects that.
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param from_fen The position we are confident about.
#' @param target_placement The placement field just observed.
#' @param max_plies Longest sequence to consider.
#' @param cap Stop once this many distinct paths are found; the caller only
#'   needs to distinguish none, one and more-than-one.
#' @return A list of character vectors of UCI moves, shortest first.
find_move_paths <- function(ctx, from_fen, target_placement, max_plies = 2L,
                            cap = 4L) {
  if (is.na(from_fen) || is.na(target_placement)) {
    return(list())
  }
  ensure_tracker_js(ctx)
  ctx$assign("cvFrom", from_fen)
  ctx$assign("cvTarget", target_placement)
  ctx$assign("cvMaxPlies", as.integer(max_plies))
  ctx$assign("cvCap", as.integer(cap))
  res <- ctx$eval("JSON.stringify(cvFindPaths(cvFrom, cvTarget, cvMaxPlies, cvCap))")
  paths <- jsonlite::fromJSON(res, simplifyVector = FALSE)
  paths <- lapply(paths, function(p) as.character(unlist(p)))
  paths[order(lengths(paths))]
}

# ---- the running game ---------------------------------------------------

#' Start a new tracked game
#'
#' @param fen The anchor position. Its side-to-move is a guess when it came
#'   from a screenshot (nothing on a board says whose turn it is), so it stays
#'   unpinned until a move confirms it.
#' @param turn_pinned `TRUE` if the side to move is already known to be right,
#'   e.g. because the game started from the initial position.
#' @return A game record: `fens` (n+1 positions), `ucis`/`sans` (n moves),
#'   `cp` (n+1 evaluations in centipawns from White's point of view, `NA` until
#'   measured) and `turn_pinned`.
game_new <- function(fen = CV_START_FEN, turn_pinned = FALSE) {
  list(
    fens = fen,
    ucis = character(0),
    sans = character(0),
    cp = NA_real_,
    best = NA_character_,
    turn_pinned = isTRUE(turn_pinned)
  )
}

#' The position a tracked game currently stands at
#'
#' @param game A game record from [game_new()].
#' @return The latest FEN.
game_current_fen <- function(game) game$fens[length(game$fens)]

#' Number of plies played in a tracked game
#'
#' @param game A game record from [game_new()].
#' @return The move count.
game_ply <- function(game) length(game$ucis)

#' Play a move onto a tracked game
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param game A game record from [game_new()].
#' @param uci The move to play.
#' @return The updated game record, or the original unchanged if the move was
#'   illegal.
game_apply_move <- function(ctx, game, uci) {
  from <- game_current_fen(game)
  san <- tryCatch(fen_move_to_san(ctx, from, uci), error = function(e) NULL)
  after <- fen_after_move(ctx, from, uci)
  if (is.na(after)) {
    return(game)
  }
  game$fens <- c(game$fens, after)
  game$ucis <- c(game$ucis, uci)
  game$sans <- c(game$sans, san %||% uci)
  game$cp <- c(game$cp, NA_real_)
  game$best <- c(game$best, NA_character_)
  game$turn_pinned <- TRUE
  game
}

# ---- the gate -----------------------------------------------------------

#' Decide what a freshly recognized position means for a tracked game
#'
#' The whole point of the feature lives here. An observation is classified, not
#' trusted:
#'
#' * `invalid` - the recognizer produced nothing usable.
#' * `unchanged` - the same position we already hold; the common case, and not
#'   an error.
#' * `stale` - a position we were in earlier. A capture frame caught mid
#'   animation, or a re-render, shows the board as it was; treating that as a
#'   fresh event would rewind the game.
#' * `move` - exactly one legal sequence explains it. Accepted.
#' * `ambiguous` - several sequences explain it, so we cannot say which was
#'   played. Rare in practice but never guessed at.
#' * `unexplained` - no legal sequence reaches it. Almost always a misread
#'   square; occasionally a jump longer than `max_plies`. Rejected either way.
#'
#' While the side to move is still unpinned (the anchor came from a screenshot,
#' which cannot show whose turn it is) the search is retried from the same
#' position with the turn flipped. A first move that only makes sense for the
#' other side is the evidence that settles it.
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param game A game record from [game_new()].
#' @param observed_fen The FEN just recognized; only its placement is used.
#' @param max_plies Longest move sequence that may be inferred at once.
#' @return A list with `status`, `moves` (UCI moves for status `move`),
#'   `anchor_fen` (the position the moves are played from - the current one,
#'   unless the turn had to be flipped), `turn_flipped` and a human-readable
#'   `reason`.
track_observation <- function(ctx, game, observed_fen, max_plies = 2L) {
  out <- function(status, reason, moves = character(0),
                  anchor_fen = game_current_fen(game), turn_flipped = FALSE) {
    list(
      status = status, reason = reason, moves = moves,
      anchor_fen = anchor_fen, turn_flipped = turn_flipped
    )
  }

  target <- fen_placement(observed_fen)
  if (is.na(target) || !grepl("^[1-8pnbrqkPNBRQK/]+$", target)) {
    return(out("invalid", "the screenshot did not produce a readable position"))
  }

  current <- game_current_fen(game)
  if (identical(target, fen_placement(current))) {
    return(out("unchanged", "same position as before"))
  }
  if (target %in% vapply(game$fens, fen_placement, character(1))) {
    return(out("stale", "a position this game has already been in (re-render or mid-animation frame)"))
  }

  paths <- find_move_paths(ctx, current, target, max_plies)
  anchor <- current
  flipped <- FALSE

  # Nothing fits, and we were never sure whose turn it was: the anchor came
  # from a screenshot. Try the other side before blaming recognition.
  if (!length(paths) && !isTRUE(game$turn_pinned)) {
    alt_turn <- if (grepl(" b ", current, fixed = TRUE)) "w" else "b"
    alt <- fen_with_turn(current, alt_turn)
    if (!is.na(alt) && is_valid_fen(ctx, alt)) {
      alt_paths <- find_move_paths(ctx, alt, target, max_plies)
      if (length(alt_paths)) {
        paths <- alt_paths
        anchor <- alt
        flipped <- TRUE
      }
    }
  }

  if (!length(paths)) {
    return(out(
      "unexplained",
      sprintf(
        "no legal sequence of %d move(s) or fewer leads here - most likely a misread square",
        max_plies
      )
    ))
  }
  # Keep only the shortest explanations; a longer path reaching the same
  # placement is not a competing account of the same event.
  shortest <- lengths(paths)
  paths <- paths[shortest == min(shortest)]
  if (length(paths) > 1) {
    return(out(
      "ambiguous",
      sprintf("%d different move sequences would produce this position", length(paths))
    ))
  }

  moves <- paths[[1]]
  out(
    "move",
    if (length(moves) > 1) {
      sprintf("%d plies inferred at once (a frame was missed)", length(moves))
    } else {
      "single legal move"
    },
    moves = moves, anchor_fen = anchor, turn_flipped = flipped
  )
}

#' Fold an accepted observation into the game
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param game A game record from [game_new()].
#' @param result A `move` result from [track_observation()].
#' @return The updated game record.
game_accept <- function(ctx, game, result) {
  if (!identical(result$status, "move")) {
    return(game)
  }
  if (isTRUE(result$turn_flipped)) {
    # The anchor's side to move was a guess and the first real move disproved
    # it. Correct the record before playing on, so the move numbers, the SAN
    # and every evaluation from here are attributed to the right player.
    game$fens[length(game$fens)] <- result$anchor_fen
  }
  for (uci in result$moves) game <- game_apply_move(ctx, game, uci)
  game
}

# ---- reading the game back ----------------------------------------------

#' Classify how bad a move was
#'
#' Thresholds follow the convention players already know from Lichess and
#' Chess.com, so "blunder" here means what it means everywhere else.
#'
#' @param loss_cp Centipawns lost by the move.
#' @return One of "blunder", "mistake", "inaccuracy" or "ok".
move_quality <- function(loss_cp) {
  ifelse(is.na(loss_cp), "ok",
    ifelse(loss_cp >= 200, "blunder",
      ifelse(loss_cp >= 100, "mistake",
        ifelse(loss_cp >= 50, "inaccuracy", "ok")
      )
    )
  )
}

#' What each move cost the player who made it
#'
#' Evaluations are stored from White's point of view so the graph has a fixed
#' meaning; a loss is measured from the mover's, which is the only reading
#' under which "you lost 3 pawns" makes sense for both colours.
#'
#' @param game A game record with `cp` filled in.
#' @return A numeric vector, one entry per move, of centipawns lost. `NA` where
#'   either surrounding position is not yet evaluated.
game_move_losses <- function(game) {
  n <- length(game$ucis)
  if (!n) {
    return(numeric(0))
  }
  movers <- vapply(game$fens[seq_len(n)], function(f) {
    strsplit(f, " ", fixed = TRUE)[[1]][2]
  }, character(1))
  sign <- ifelse(movers == "b", -1, 1)
  before <- game$cp[seq_len(n)] * sign
  after <- game$cp[seq_len(n) + 1] * sign
  pmax(0, before - after)
}

#' Describe what a move cost, in words a player would use
#'
#' Mate is scored in the thousands of centipawns so that it outranks any amount
#' of material, which makes "lost 100.3 pawns" the literal but useless reading
#' of walking into mate. Past the point where material stops being the story,
#' say what actually happened instead.
#'
#' @param loss_cp Centipawns lost, from [game_move_losses()].
#' @return A short label such as "1.4 pawns" or "gets mated".
format_loss <- function(loss_cp) {
  vapply(loss_cp, function(x) {
    if (is.na(x)) {
      return("not scored")
    }
    if (x >= 5000) {
      return("gets mated")
    }
    if (x >= 1000) {
      return("loses outright")
    }
    sprintf("%.1f pawns", x / 100)
  }, character(1))
}

#' The moves where the game turned
#'
#' @param game A game record with `cp` filled in.
#' @param min_loss Smallest centipawn loss worth reporting.
#' @param n How many to return.
#' @return A data frame of `ply`, `move_no`, `mover`, `san`, `loss` and
#'   `quality`, worst first.
game_turning_points <- function(game, min_loss = 100, n = 5L) {
  losses <- game_move_losses(game)
  empty <- data.frame(
    ply = integer(0), move_no = integer(0), mover = character(0),
    san = character(0), loss = numeric(0), quality = character(0),
    stringsAsFactors = FALSE
  )
  if (!length(losses) || all(is.na(losses))) {
    return(empty)
  }
  keep <- which(!is.na(losses) & losses >= min_loss)
  if (!length(keep)) {
    return(empty)
  }
  df <- data.frame(
    ply = keep,
    move_no = (keep + 1L) %/% 2L,
    mover = vapply(game$fens[keep], function(f) {
      strsplit(f, " ", fixed = TRUE)[[1]][2]
    }, character(1)),
    san = game$sans[keep],
    loss = losses[keep],
    quality = move_quality(losses[keep]),
    stringsAsFactors = FALSE
  )
  utils::head(df[order(-df$loss), ], n)
}

#' Geometry for the evaluation graph
#'
#' The arithmetic behind the graph, with no drawing: where each point sits and
#' what shapes join them. [eval_graph_svg()] turns this into an SVG. They are
#' split so this layer stays free of any UI toolkit - see the note in
#' `test-core-boundary.R`.
#'
#' Evaluations are squashed with the same logistic the eval bar uses, so a
#' three-pawn edge and a nine-pawn one stay visibly different without the graph
#' being dominated by a single decisive moment.
#'
#' Positions that have not been evaluated yet carry the last known value
#' forward rather than breaking the line, so the graph does not flicker while
#' the engine catches up with a fast game.
#'
#' @param cp Centipawn evaluations from White's point of view, one per position
#'   (so one more than the number of moves). `NA` where not yet measured.
#' @param width,height Pixel size of the graph.
#' @return A list with `n` points, `xs`/`ys` coordinates, `points` and `area`
#'   as SVG coordinate strings, `mid` (the halfway line) and `hit_width` (the
#'   width of one clickable band); or `NULL` if there is nothing to draw.
eval_graph_geometry <- function(cp, width = 420, height = 110) {
  if (!length(cp) || all(is.na(cp))) {
    return(NULL)
  }
  # Carry the last measured value forward; before the first one, sit at level.
  filled <- cp
  last <- 0
  for (i in seq_along(filled)) {
    if (is.na(filled[i])) filled[i] <- last else last <- filled[i]
  }

  n <- length(filled)
  xs <- if (n == 1) width / 2 else seq(0, width, length.out = n)
  ys <- vapply(filled, function(v) {
    height * (1 - eval_bar_pct(v, NA_integer_, "w") / 100)
  }, numeric(1))

  pts <- paste(sprintf("%.1f,%.1f", xs, ys), collapse = " ")
  mid <- height / 2

  list(
    n = n,
    xs = xs,
    ys = ys,
    points = pts,
    # A filled band between the curve and the halfway line reads as "who is
    # better" at a glance, which a bare line does not.
    area = sprintf("0,%.1f %s %.1f,%.1f", mid, pts, width, mid),
    mid = mid,
    hit_width = if (n == 1) width else width / (n - 1)
  )
}

#' Build a tracked game from PGN
#'
#' Parsing PGN properly is a real job - disambiguated SAN, comments, variations,
#' numeric annotation glyphs, games that start from a set-up position - and
#' chess.js already does it. It is embedded here anyway, for legality and SAN,
#' so this hands the text to `loadPgn()` and then walks the resulting history to
#' record the position before every move.
#'
#' The result is the same shape [game_new()] produces, which is the point: an
#' imported game is indistinguishable from one watched move by move, so the
#' evaluation graph, the turning points, the Blunder Radar review and the rating
#' estimator all work on it with no code of their own.
#'
#' A `[FEN]` header is honoured, so a study or an endgame that does not begin
#' from the initial position imports correctly rather than being silently
#' replayed from the standard start.
#'
#' @param ctx A chess.js V8 context from [new_chess_context()].
#' @param pgn PGN text, with or without headers.
#' @return A list with `game` (a record as from [game_new()]), `headers` (named
#'   character, possibly empty) and `n_moves`; or a list with `error` when the
#'   text could not be parsed.
game_from_pgn <- function(ctx, pgn) {
  if (!length(pgn) || !nzchar(trimws(paste(pgn, collapse = "\n")))) {
    return(list(error = "No PGN supplied."))
  }
  ctx$assign("pgnText", paste(pgn, collapse = "\n"))

  raw <- ctx$eval("
    (function () {
      var g = new ChessCtor();
      try {
        g.loadPgn(pgnText);
      } catch (e) {
        return JSON.stringify({error: String(e && e.message ? e.message : e)});
      }
      var verbose = g.history({verbose: true});
      if (!verbose.length) {
        return JSON.stringify({error: 'The PGN parsed but contains no moves.'});
      }
      // Replay from the start of *this* game, which a [FEN] header may have
      // moved: rewinding the loaded game with undo() would be equivalent but
      // loses the header handling.
      var headers = g.header ? g.header() : {};
      var start = headers.FEN || headers.Fen || null;
      var walk = start ? new ChessCtor(start) : new ChessCtor();
      var fens = [walk.fen()];
      var ucis = [];
      var sans = [];
      for (var i = 0; i < verbose.length; i++) {
        var m = verbose[i];
        sans.push(m.san);
        ucis.push(m.from + m.to + (m.promotion ? m.promotion : ''));
        walk.move(m.san);
        fens.push(walk.fen());
      }
      return JSON.stringify({
        fens: fens, ucis: ucis, sans: sans, headers: headers
      });
    })()
  ")

  parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
  if (is.null(parsed)) {
    return(list(error = "The PGN could not be read."))
  }
  if (!is.null(parsed$error)) {
    return(list(error = parsed$error))
  }

  n <- length(parsed$ucis)
  game <- list(
    fens = as.character(parsed$fens),
    ucis = as.character(parsed$ucis),
    sans = as.character(parsed$sans),
    # Evaluations are filled in afterwards, a few plies at a time, so importing
    # a long game does not stall the app while an engine works through it.
    cp = rep(NA_real_, n + 1L),
    best = rep(NA_character_, n + 1L),
    turn_pinned = TRUE
  )

  headers <- parsed$headers
  if (is.null(headers) || !length(headers)) headers <- character(0)

  list(game = game, headers = unlist(headers), n_moves = n)
}

#' A one-line description of an imported game
#'
#' @param headers Headers from [game_from_pgn()].
#' @param n_moves Number of plies.
#' @return A single string, or `NULL` when the headers say nothing useful.
pgn_summary <- function(headers, n_moves) {
  # `headers` is a named character vector, where `[[` on an absent name is an
  # error rather than NULL - unlike a list. Most real PGNs omit at least one of
  # these, so the membership test is what stops an ordinary game crashing the
  # import. "?" is PGN's own placeholder for unknown and is not a name.
  get <- function(k) {
    if (!length(headers) || !(k %in% names(headers))) {
      return(NULL)
    }
    v <- unname(headers[[k]])
    if (is.na(v) || !nzchar(v) || identical(v, "?")) NULL else v
  }
  players <- if (!is.null(get("White")) || !is.null(get("Black"))) {
    sprintf("%s vs %s", get("White") %||% "?", get("Black") %||% "?")
  } else {
    NULL
  }
  bits <- c(players, get("Event"), get("Date"), get("Result"))
  bits <- bits[!vapply(bits, is.null, logical(1))]
  if (!length(bits)) {
    return(sprintf("%d moves", ceiling(n_moves / 2)))
  }
  paste0(paste(bits, collapse = " - "), sprintf(" (%d plies)", n_moves))
}
