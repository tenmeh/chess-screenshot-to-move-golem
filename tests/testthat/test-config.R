# Environment configuration, including the names used before the 1.2.0 rename.

test_that("the current spelling is read", {
  withr::with_envvar(c(TANMAI_STOCKFISH = "/new/sf"), {
    expect_equal(cfg_env("STOCKFISH"), "/new/sf")
  })
})

test_that("the pre-rename spelling still works", {
  # The package was `chessvision` until 1.2.0. Anyone whose container or shell
  # profile still sets CHESSVISION_* must not silently lose their engine - and
  # it would be silent, because the fallback path (search PATH, look in the
  # user cache) looks like ordinary behaviour rather than an error.
  withr::with_envvar(c(TANMAI_LC0 = NA, CHESSVISION_LC0 = "/old/lc0"), {
    expect_equal(cfg_env("LC0"), "/old/lc0")
  })
})

test_that("the current spelling wins when both are set", {
  withr::with_envvar(
    c(TANMAI_MAIA_DIR = "/new/maia", CHESSVISION_MAIA_DIR = "/old/maia"),
    expect_equal(cfg_env("MAIA_DIR"), "/new/maia")
  )
})

test_that("an unset setting falls back to the default", {
  withr::with_envvar(c(TANMAI_STOCKFISH = NA, CHESSVISION_STOCKFISH = NA), {
    expect_equal(cfg_env("STOCKFISH"), "")
    expect_equal(cfg_env("STOCKFISH", "fallback"), "fallback")
  })
})
