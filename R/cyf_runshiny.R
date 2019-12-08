#' @name cy_RunShiny
#' @title Run Shiny App
#' @description Launch Shiny App that displays pay information for subsets of Iowa State University Employees
#' @return a Shiny App
#' @examples
#' \dontrun{
#' cy_RunShiny()
#' }
#' @export

cy_RunShiny <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "CyChecks2")
  if (appDir == ""){
    stop("Couldn't locate example directory. Try re-installing `CyChecks`", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}