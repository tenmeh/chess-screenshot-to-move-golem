# Configuration read from the environment.
#
# The package was called `chessvision` until 1.2.0, and its environment
# variables were named to match. Renaming them outright would silently break
# any deployment, shell profile or container that already sets the old ones -
# and silently, because every one of these has a fallback that looks like
# working behaviour: the engine is searched for on PATH instead, the weights
# are looked for in the user cache instead. Nothing errors; the human model
# just quietly reports itself unavailable.
#
# So both spellings are accepted, with the current one winning.

#' Read a configuration setting from the environment
#'
#' Checks `TANMAI_<name>` first, then the `CHESSVISION_<name>` spelling used
#' before 1.2.0.
#'
#' @param name The setting, without prefix - for example `"STOCKFISH"`.
#' @param default Returned when neither variable is set.
#' @return The configured value, or `default`.
cfg_env <- function(name, default = "") {
  current <- Sys.getenv(paste0("TANMAI_", name), "")
  if (nzchar(current)) {
    return(current)
  }
  Sys.getenv(paste0("CHESSVISION_", name), default)
}
