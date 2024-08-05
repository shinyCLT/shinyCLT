#' Launch the Central Limit Theorem Shiny application
#'
#' This function starts a Shiny application that demonstrates examples of
#'  the Central Limit Theorem.
#' The app is stored internally within the package and showcases various
#'  statistical principles through interactive visualizations.
#' It automatically checks for, installs (if missing), and loads necessary
#'  packages.
#'
#' @export
#' @return Runs shinyApp
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#'   CLT()
#' }
CLT <- function() {
    # Start the Shiny app
  shiny::runApp(system.file("shinyApp", package = "shinyCLT"))
}
