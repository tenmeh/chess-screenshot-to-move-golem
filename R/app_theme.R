# The app's visual language, in one place.
#
# Bootstrap variables are set here and the same palette is mirrored as CSS
# custom properties in styles.css, so Shiny's own widgets and this app's
# hand-built pieces (the board, the eval bar, the radar rows) cannot drift
# apart.
#
# Colours are keyed off `data-bs-theme`, which is the attribute bslib's dark
# mode switch toggles - not `prefers-color-scheme`. Keying off the media query
# instead would look right on load and then ignore the switch entirely.

# Green because the subject is chess, but a muted one: the board, the eval bar
# and the radar arrows all carry meaning through colour, and a loud accent
# competes with them for attention.
CV_ACCENT <- "#1f9d61"
CV_ACCENT_DARK <- "#34c583"

#' The application's Bootstrap theme
#'
#' @return A [bslib::bs_theme()] object.
cv_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = CV_ACCENT,
    success = CV_ACCENT,
    # A system stack rather than a web font: it costs no round trip, cannot
    # fail on a cold Cloud Run start, and renders natively on every platform.
    base_font = bslib::font_face(
      family = "cv-sans",
      src = "local('Inter'), local('Segoe UI'), local('SF Pro Text')"
    ),
    "font-family-base" = paste(
      "Inter", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'",
      "Roboto", "'Helvetica Neue'", "Arial", "sans-serif",
      sep = ", "
    ),
    "font-family-monospace" = paste(
      "'JetBrains Mono'", "'SF Mono'", "'Cascadia Mono'", "Consolas",
      "'Liberation Mono'", "monospace",
      sep = ", "
    ),
    "body-bg" = "#f6f7f9",
    "body-color" = "#12151a",
    "border-radius" = "0.65rem",
    "border-radius-sm" = "0.45rem",
    "border-radius-lg" = "0.9rem",
    "card-border-color" = "rgba(16, 22, 32, 0.08)",
    "card-cap-bg" = "transparent",
    "navbar-padding-y" = "0.6rem",
    "nav-link-font-weight" = "500",
    "headings-font-weight" = "650",
    "btn-font-weight" = "550",
    "input-btn-focus-width" = "0.2rem"
  )
}
