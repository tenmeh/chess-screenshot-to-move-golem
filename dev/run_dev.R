## Run the app in development mode (hot-reloads R/ on save if using RStudio's
## Run App button; otherwise re-run this script after editing).
##
## Requires: install.packages(c(
##   "golem", "shiny", "magick", "processx", "jsonlite", "bslib", "V8", "shinyjs"
## ))

pkgload::load_all(here::here())
shiny::runApp(shiny::shinyApp(ui = app_ui, server = app_server))
