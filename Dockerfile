# chessvision: chess screenshot -> best move.
#
# Everything the app needs at runtime is baked in here, because container
# filesystems are ephemeral and often read-only: the chess engine comes from
# apt rather than a runtime download, and every piece-set template library is
# built during the image build so a cold start is immediately ready.

FROM rocker/r-ver:4.6.1

# System libraries:
#   libmagick++    - the magick package
#   librsvg2       - rasterizes the piece SVGs (magick's own SVG delegate is
#                    unreliable, hence the rsvg package)
#   libnode        - the V8 package, which runs chess.js for rules/SAN
#   stockfish      - the engine; installing it here avoids fetching a binary
#                    onto a filesystem that may be read-only
RUN apt-get update && apt-get install -y --no-install-recommends \
        libmagick++-dev \
        librsvg2-dev \
        libnode-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        curl \
        stockfish \
    && rm -rf /var/lib/apt/lists/*

# Debian installs the engine into /usr/games, which is not on the default PATH
# of a non-interactive session.
ENV PATH="/usr/games:${PATH}"

WORKDIR /srv/chessvision

# renv.lock is the single source of truth for R package versions - the same
# file `renv::snapshot()` maintains for local dev - copied in on its own so
# this layer only rebuilds when a dependency actually changes, not on every
# source edit.
COPY renv.lock renv.lock

# restore() installs the exact locked versions directly into R's normal
# site-library (not renv's usual project-private renv/library folder, via
# the explicit `library` argument below). This is deliberate: renv isolates
# an active project's library and - critically - hides the plain
# site-library once a project is "activated" but not yet in sync, which is
# exactly what broke `library(chessvision)` here previously (chessvision
# itself is installed by a plain R CMD INSTALL below, into that same plain
# site-library, bypassing renv entirely). Using renv purely as a one-shot,
# build-time installer sidesteps that: nothing here ever activates renv as a
# project, so .Rprofile and renv/activate.R are never needed in the image
# (see .dockerignore) and there is no isolation to fight with at runtime.
RUN Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
    && Rscript -e "renv::restore(project = '.', library = '/usr/local/lib/R/site-library', prompt = FALSE)"

COPY . .

RUN R CMD INSTALL --no-multiarch --with-keep.source . \
    && rm -rf /tmp/downloaded_packages

# Pre-build every piece-set template library. Without this the app would fetch
# ~38 sets from lichess.org on first use and lose them again on the next cold
# start. Failing softly keeps the image buildable if lichess is unreachable -
# the bundled cburnett set always works.
RUN Rscript -e "library(chessvision); \
      ready <- tryCatch(chessvision:::setup_piece_sets(), error = function(e) { \
        message('piece-set prefetch skipped: ', conditionMessage(e)); logical(0) }); \
      message(sprintf('piece sets baked in: %d', sum(ready)))"

# Fail the build rather than ship an image whose engine or art is missing.
RUN Rscript -e "library(chessvision); \
      stopifnot(!is.null(chessvision:::find_local_engine())); \
      sets <- chessvision:::available_piece_sets(); \
      message(sprintf('engine: %s', chessvision:::find_local_engine())); \
      message(sprintf('piece sets available: %d', length(sets))); \
      stopifnot(length(sets) >= 1)"

# Cloud Run (and most hosts) inject the port to listen on.
ENV PORT=8080
EXPOSE 8080

CMD ["Rscript", "/srv/chessvision/app.R"]
