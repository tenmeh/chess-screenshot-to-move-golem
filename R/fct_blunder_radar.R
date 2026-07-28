# Blunder Radar: what a human is likely to get wrong here.
#
# An engine answers "what is best". This answers a different question: given a
# player of a particular strength, which mistakes are they actually likely to
# make, and how expensive are those mistakes? It is the product of two things
# the app already has - Maia's probability that a human plays each move, and
# Stockfish's evaluation of what each move costs.
#
#   risk = sum over moves of  P_human(move) x centipawns_lost(move)
#
# A move that loses a queen but no human would play carries no risk; a natural
# move that quietly loses a pawn carries a lot. That difference is invisible to
# an engine alone, and it is the whole point of this module.

# Mate scores are folded into a centipawn scale so they can be compared with
# ordinary evaluations. The exact number only needs to dominate real material.
MATE_CP <- 10000

#' Convert a UCI score into a single centipawn number
#'
#' @param cp Centipawn score, or `NA`.
#' @param mate Moves-to-mate score, or `NA`.
#' @return A centipawn-scaled value from the side-to-move's point of view.
score_to_cp <- function(cp, mate) {
  if (!is.na(mate)) {
    return(sign(mate) * (MATE_CP - abs(mate)))
  }
  if (!is.na(cp)) {
    return(cp)
  }
  NA_real_
}

#' Evaluate a specific set of moves in one search
#'
#' Uses UCI `searchmoves` so the engine scores exactly the moves asked for.
#' This matters: the moves a human is likely to blunder into are by definition
#' ones the engine ranks poorly, so they would fall outside an ordinary MultiPV
#' list.
#'
#' @param fen Position to search from.
#' @param moves UCI moves to evaluate.
#' @param movetime_ms Thinking time for the whole search.
#' @param engine_path Optional explicit engine path.
#' @return A data frame of `move` and `cp` (side-to-move's point of view).
evaluate_moves <- function(fen, moves, movetime_ms = 1200L, engine_path = NULL) {
  empty <- data.frame(move = character(0), cp = numeric(0), stringsAsFactors = FALSE)
  if (!length(moves)) {
    return(empty)
  }
  if (is.null(engine_path)) engine_path <- tryCatch(ensure_stockfish(), error = function(e) NULL)
  if (is.null(engine_path)) {
    return(empty)
  }

  p <- processx::process$new(engine_path, stdin = "|", stdout = "|", stderr = "|")
  on.exit(try(p$kill(), silent = TRUE), add = TRUE)
  p$write_input("uci\n")
  wait_for_line(p, "uciok", timeout = 20)
  p$write_input(sprintf("setoption name MultiPV value %d\n", length(moves)))
  p$write_input("isready\n")
  wait_for_line(p, "readyok", timeout = 20)

  p$write_input(sprintf("position fen %s\n", fen))
  p$write_input(sprintf(
    "go movetime %d searchmoves %s\n",
    as.integer(movetime_ms), paste(moves, collapse = " ")
  ))

  best <- list()
  deadline <- Sys.time() + (movetime_ms / 1000 + 20)
  repeat {
    if (Sys.time() > deadline) break
    p$poll_io(100)
    lines <- p$read_output_lines()
    for (line in lines) {
      if (startsWith(line, "info")) {
        rec <- parse_info_line(line)
        if (is.null(rec) || is.na(rec$depth)) next
        # Keyed by first move, so deeper iterations replace shallower ones.
        best[[rec$first]] <- rec
      }
    }
    if (any(startsWith(lines, "bestmove"))) break
  }

  if (!length(best)) {
    return(empty)
  }
  data.frame(
    move = names(best),
    cp = vapply(best, function(r) score_to_cp(r$cp, r$mate), numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Assess how likely a human is to go wrong in a position
#'
#' @param fen Position to assess.
#' @param maia A session from [maia_session_start()].
#' @param min_prob Ignore moves a human is very unlikely to play; keeps the
#'   engine work proportional to what actually matters.
#' @param movetime_ms Engine time for the evaluation search.
#' @param engine_path Optional explicit engine path.
#' @param max_loss_cp Ceiling on how bad a single move is allowed to count as.
#'   Past roughly ten pawns a move is simply losing, and without a ceiling a
#'   forced mate (scored in the thousands) would swamp the average and make
#'   `risk` unreadable.
#' @return A list with `risk` (probability-weighted centipawn loss),
#'   `blunder_prob` (chance of losing at least a pawn), `sharpness` (how much
#'   the plausible moves disagree in value), `best` (the engine's move) and
#'   `moves`, a data frame of candidates with `prob`, `cp`, `loss` and
#'   `contribution`.
blunder_risk <- function(fen, maia, min_prob = 0.01,
                         movetime_ms = 1200L, engine_path = NULL,
                         max_loss_cp = 1000) {
  none <- list(
    risk = NA_real_, blunder_prob = NA_real_, sharpness = NA_real_,
    best = NA_character_,
    moves = data.frame(
      move = character(0), prob = numeric(0), cp = numeric(0),
      loss = numeric(0), contribution = numeric(0), stringsAsFactors = FALSE
    )
  )
  policy <- human_move_probabilities(maia, fen)
  if (!nrow(policy)) {
    return(none)
  }

  candidates <- policy[policy$prob >= min_prob, , drop = FALSE]
  # Always keep a few, even in a position with one obvious move.
  if (nrow(candidates) < 3) candidates <- utils::head(policy, 3)

  evals <- evaluate_moves(fen, candidates$move, movetime_ms, engine_path)
  if (!nrow(evals)) {
    return(none)
  }

  merged <- merge(candidates, evals, by = "move")
  if (!nrow(merged)) {
    return(none)
  }

  # Loss is measured against the best of the moves actually considered, which
  # is the engine's preference among plausible human choices.
  best_cp <- max(merged$cp)
  merged$loss <- pmin(max_loss_cp, pmax(0, best_cp - merged$cp))
  # Renormalize: the discarded long-tail moves should not shrink the estimate.
  weight <- merged$prob / sum(merged$prob)
  merged$contribution <- weight * merged$loss
  merged <- merged[order(-merged$contribution), ]

  list(
    risk = sum(merged$contribution),
    blunder_prob = sum(weight[merged$loss >= 100]),
    # Spread of outcomes across the moves a human might realistically pick.
    sharpness = sqrt(sum(weight * (merged$loss - sum(merged$contribution))^2)),
    best = merged$move[which.max(merged$cp)],
    moves = merged[, c("move", "prob", "cp", "loss", "contribution")]
  )
}

#' Rank your moves by how likely they are to induce an opponent error
#'
#' Choose which human moves to draw on the board
#'
#' Selection is by risk contribution rather than raw probability. In a trap the
#' dangerous move is by construction not the most likely one - it is merely
#' plausible - so picking the most probable moves would leave the single move
#' worth warning about undrawn, which defeats the purpose of the overlay. The
#' likeliest move is appended for context.
#'
#' @param moves The `moves` data frame from [blunder_risk()].
#' @param n How many risk-ranked moves to take before adding the likeliest.
#' @return The selected rows, most dangerous first, with no duplicates.
radar_arrow_moves <- function(moves, n = 3L) {
  if (is.null(moves) || !nrow(moves)) {
    return(moves)
  }
  by_risk <- utils::head(moves[order(-moves$contribution), ], n)
  shown <- rbind(by_risk, moves[which.max(moves$prob), ])
  shown[!duplicated(shown$move), ]
}

#' Rate moves by how likely they are to induce an opponent mistake
#'
#' For each candidate move, plays it and measures how badly a human opponent is
#' likely to go wrong in the resulting position. Moves are then scored on
#' practical value: what the move costs you objectively, set against what it is
#' likely to win you from the opponent's mistakes.
#'
#' @param fen Position to move from.
#' @param maia A session from [maia_session_start()] representing the opponent.
#' @param ctx A chess.js V8 context, used to apply moves.
#' @param top_n How many of your own candidate moves to examine.
#' @param movetime_ms Engine time per evaluation search.
#' @return A data frame of `move`, `cp` (its objective value to you),
#'   `cost` (centipawns given up versus your best move), `trap` (the
#'   opponent's expected loss afterwards) and `practical` (trap minus cost),
#'   ordered by `practical`.
trappiness <- function(fen, maia, ctx, top_n = 5L, movetime_ms = 800L) {
  empty <- data.frame(
    move = character(0), cp = numeric(0), cost = numeric(0),
    trap = numeric(0), practical = numeric(0), stringsAsFactors = FALSE
  )
  own <- evaluate_moves(fen, legal_moves(ctx, fen), movetime_ms)
  if (!nrow(own)) {
    return(empty)
  }
  own <- own[order(-own$cp), , drop = FALSE]
  own <- utils::head(own, top_n)

  traps <- vapply(own$move, function(mv) {
    after <- fen_after_move(ctx, fen, mv)
    if (is.na(after)) {
      return(NA_real_)
    }
    res <- blunder_risk(after, maia, movetime_ms = movetime_ms)
    if (is.na(res$risk)) NA_real_ else res$risk
  }, numeric(1))

  out <- data.frame(
    move = own$move,
    cp = own$cp,
    cost = max(own$cp) - own$cp,
    trap = unname(traps),
    stringsAsFactors = FALSE
  )
  out$practical <- out$trap - out$cost
  out[order(-out$practical), ]
}
