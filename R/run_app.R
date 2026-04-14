#' Run the Shiny Application
#'
#' @title maceR
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @description add description
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Ensure Pandoc is available for rmarkdown report generation
  # Check common Homebrew installation path
  homebrew_pandoc <- "/opt/homebrew/bin/pandoc"
  if (file.exists(homebrew_pandoc)) {
    # Add Homebrew bin to PATH if Pandoc is there
    current_path <- Sys.getenv("PATH")
    if (!grepl("/opt/homebrew/bin", current_path)) {
      Sys.setenv(PATH = paste("/opt/homebrew/bin", current_path, sep = ":"))
    }
  }
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}
