#' @export
#' 
runShiny <- function() {
  appDir <- system.file("shinyApp", package = "LtAtStructuR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `LtAtStructuR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}