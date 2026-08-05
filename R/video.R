# Reading a whole game out of a recording.
#
# This is the same tracker the live app uses, pointed at a file instead of a
# screen. That is the entire trick, and it works because of the gate in
# fct_game_tracker.R: a frame is only believed when exactly one legal sequence
# of moves explains it. A recording is *full* of frames that must not be
# believed - pieces halfway through a slide animation, the board re-rendering,
# a mouse dragging a piece that is put back - and every one of them either
# fails to produce a legal sequence or reproduces a position the game has
# already been in. They are discarded on those grounds rather than by any
# video-specific cleverness.
#
# The expensive step is recognition, not decoding. Almost every frame in a
# recording is pixel-identical to the one before it, so frames are compared
# first and only recognised when the picture actually changed. On a real game
# recording that skips the large majority of them.

#' Cheap signature of a frame, for spotting frames that did not change
#'
#' A 32x32 grey thumbnail, four cells to a square. Enough to notice a piece
#' moving, cheap enough to compute on every frame of a long recording, and -
#' because downscaling averages compression artefacts away - blind to the noise
#' that makes exact pixel comparison useless on video.
#'
#' Values come back on a 0-1 scale, not 0-255.
#'
#' @param img A magick image.
#' @return A numeric vector of length 1024.
#' @noRd
frame_signature <- function(img) {
  small <- magick::image_convert(
    magick::image_resize(img, "32x32!"),
    colorspace = "gray"
  )
  as.numeric(magick::image_data(small, channels = "gray"))
}

# How different two thumbnails must be before the frame is worth recognising.
#
# Measured, not guessed. At 32x32 on a 400px board: two renderings of the same
# position differ by 0.0000, and so does a jpeg round trip at quality 70 -
# the downscale averages the artefacts out. The smallest real change there is,
# a pawn advancing one square, differs by 0.1176; a knight move by 0.3098.
# 0.02 sits an order of magnitude above the noise and six times below the
# faintest signal.
#
# The statistic is deliberately the *maximum* cell difference rather than the
# mean: a moved piece changes a few cells a great deal, while noise changes
# many cells slightly, and the maximum is what tells those apart.
FRAME_CHANGE_GATE <- 0.02

#' Pull frames out of a recording
#'
#' GIFs and directories of stills need nothing beyond magick. Real video
#' formats need ffmpeg, which is not an R package and cannot be depended on, so
#' its absence is reported rather than assumed away.
#'
#' @param path A GIF, a directory of images, or a video file.
#' @param fps Frames to sample per second, for video only.
#' @return A magick image vector.
#' @noRd
video_frames <- function(path, fps = 1) {
  if (dir.exists(path)) {
    files <- sort(list.files(path,
      pattern = "\\.(png|jpe?g|gif|bmp|tiff?)$",
      ignore.case = TRUE, full.names = TRUE
    ))
    if (!length(files)) stop("No image files in ", path, call. = FALSE)
    return(magick::image_read(files))
  }
  if (!file.exists(path)) stop("No such file: ", path, call. = FALSE)

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("gif", "png", "tif", "tiff")) {
    # magick reads every frame of an animation in one go.
    return(magick::image_read(path))
  }

  ffmpeg <- Sys.which("ffmpeg")
  if (!nzchar(ffmpeg)) {
    stop(
      "Reading ", ext, " needs ffmpeg, which was not found on your PATH. ",
      "Either install it, or export the recording as an animated GIF or a ",
      "folder of images, both of which need nothing extra.",
      call. = FALSE
    )
  }
  dir <- file.path(tempdir(), paste0("tanmai-frames-", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(NULL)
  status <- system2(
    ffmpeg,
    c(
      "-nostdin", "-loglevel", "error", "-i", shQuote(path),
      "-vf", shQuote(sprintf("fps=%s", format(fps, scientific = FALSE))),
      shQuote(file.path(dir, "f%06d.png"))
    ),
    stdout = FALSE, stderr = FALSE
  )
  files <- sort(list.files(dir, pattern = "\\.png$", full.names = TRUE))
  if (status != 0 || !length(files)) {
    stop("ffmpeg could not read any frames from ", path, call. = FALSE)
  }
  magick::image_read(files)
}

#' Read a game from a recording of it being played
#'
#' Takes a screen recording, an animated GIF, or a folder of screenshots, and
#' returns the game that was played - the same [tanmai_game] that [read_pgn()]
#' returns, so everything else in the package works on it unchanged.
#'
#' Nothing seen is trusted on its own. A frame joins the game only when exactly
#' one legal sequence of moves explains it, which is what makes this work on
#' real footage: a recording is full of frames showing a piece halfway through
#' a slide, or the board mid-redraw, and those either explain nothing legal or
#' reproduce a position the game has already been in. Both are discarded. The
#' count of what was accepted and what was thrown away, and why, comes back as
#' a `ledger` attribute rather than being hidden.
#'
#' @param path A video file, an animated GIF, or a directory of stills.
#' @param fps Frames to sample per second. One is usually plenty - it only has
#'   to be faster than the players are moving, not faster than the animation.
#' @param region Optional `c(x, y, width, height)` in pixels, if the board is
#'   part of a larger screen. By default each frame is auto-cropped.
#' @param start Where the game begins: `"start"` for the standard position, or
#'   `"first"` to anchor on whatever the first readable frame shows.
#' @param turn Side to move at the anchor. Ignored when `start = "start"`.
#' @param max_plies Longest move sequence that may be inferred between two
#'   frames. Two allows a frame to be missed entirely; more invites guessing.
#' @param piece_sets Template libraries to match against. Defaults to every set
#'   installed locally.
#' @param verbose Report progress while working through the frames.
#' @return A [tanmai_game], with a `ledger` attribute recording what happened
#'   to every frame that was looked at.
#' @examples
#' \donttest{
#' # A folder of screenshots, one per move, works as well as video and needs
#' # nothing installed:
#' # game <- read_video("~/screenshots/my-game/")
#' # accuracy(evaluate(game))
#' }
#' @export
read_video <- function(path, fps = 1, region = NULL,
                       start = c("start", "first"), turn = c("w", "b"),
                       max_plies = 2L, piece_sets = NULL, verbose = FALSE) {
  start <- match.arg(start)
  turn <- match.arg(turn)

  frames <- video_frames(path, fps = fps)
  n_frames <- length(frames)
  if (!n_frames) stop("That recording has no frames.", call. = FALSE)

  ctx <- chess_ctx()
  libs <- piece_sets %||% load_all_template_libraries()
  game <- if (identical(start, "start")) game_new(CV_START_FEN, turn_pinned = TRUE) else NULL

  ledger <- character(0)
  note <- function(what) ledger[[length(ledger) + 1L]] <<- what

  # Counted separately from the ledger because "no move was ever played" and
  # "no board was ever seen" are completely different failures, and only the
  # second one means the caller pointed this at the wrong thing. With the
  # default anchor the game object exists from the start, so its emptiness
  # cannot be used to tell them apart.
  n_readable <- 0L
  last_sig <- NULL
  for (i in seq_len(n_frames)) {
    img <- frames[i]
    if (!is.null(region)) {
      img <- magick::image_crop(
        img, magick::geometry_area(region[3], region[4], region[1], region[2])
      )
    }

    # The cheap check first. Recognition costs orders of magnitude more than a
    # 16x16 thumbnail, and on a recording most frames are the previous frame.
    sig <- frame_signature(img)
    if (!is.null(last_sig) && max(abs(sig - last_sig)) < FRAME_CHANGE_GATE) {
      note("skipped (identical to the previous frame)")
      next
    }
    last_sig <- sig

    rec <- tryCatch(
      recognize_position(img, libs, ctx,
        turn = turn, autocrop = is.null(region), orientation = "auto"
      ),
      error = function(e) NULL
    )
    if (is.null(rec) || !isTRUE(rec$set_confident)) {
      note("rejected (could not read the board)")
      next
    }
    n_readable <- n_readable + 1L

    if (is.null(game)) {
      # Anchoring on the first readable frame: the turn is a guess, so it is
      # left unpinned and the first move that only makes sense for one side
      # settles it.
      game <- game_new(rec$fen, turn_pinned = FALSE)
      note("anchored on this frame")
      next
    }

    res <- track_observation(ctx, game, rec$fen, max_plies = max_plies)
    if (identical(res$status, "move")) {
      game <- game_accept(ctx, game, res)
      note(sprintf("accepted %s", paste(res$moves, collapse = " ")))
      if (verbose) {
        message(
          "  frame ", i, "/", n_frames, ": ",
          paste(res$moves, collapse = " ")
        )
      }
    } else {
      note(sprintf("%s (%s)", res$status, res$reason))
    }
  }

  if (!n_readable) {
    stop(
      "No frame in that recording produced a readable board. If the board is ",
      "part of a larger screen, pass `region`; if the site uses art the ",
      "package does not know, calibrate it first.",
      call. = FALSE
    )
  }

  out <- game_to_frame(game)
  attr(out, "ledger") <- summarise_ledger(ledger, n_frames)
  out
}

#' Count what happened to the frames
#'
#' @param ledger Character vector of per-frame outcomes.
#' @param n_frames Total frames looked at.
#' @return A data frame of outcome counts, commonest first.
#' @noRd
summarise_ledger <- function(ledger, n_frames) {
  kind <- sub(" \\(.*$", "", ledger)
  tab <- sort(table(kind), decreasing = TRUE)
  data.frame(
    outcome = names(tab),
    frames = as.integer(tab),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
