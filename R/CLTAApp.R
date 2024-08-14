#' Launch the Central Limit Theorem Shiny application
#'
#' This function starts a Shiny application that demonstrates examples of
#'  the Central Limit Theorem.
#' The app is stored internally within the package and showcases various
#'  statistical principles through interactive visualizations.
#' Check more details at vignette("shinyCLT")
#'
#' @param n.cores Number of cores to use for calculations. Default value
#' is NULL, which means that half of the cores will be used with the ceiling
#'  rounding rule.
#' @param mode Change application behaviour when web browser tab or IDE preview
#'  is closed.
#' By default this will end the running function and stop the local shinyApp.
#' Switching to "server" mode will keep the shiny application running in a
#' background even if all with application is closed.
#' @export
#' @return Runs shinyApp
#' @importFrom shiny runApp shinyOptions
#' @examples
#' \dontrun{
#' CLT()
#' }
CLT <- function(n.cores = NULL, mode = "app") {
  shiny::shinyOptions(n.cores = n.cores, mode = mode)

  shiny::runApp(system.file("shinyApp", package = "shinyCLT"))
}
