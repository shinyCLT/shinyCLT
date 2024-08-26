globalVariables(c("ui", "server"))

#' Launch the Central Limit Theorem Shiny application
#'
#' This function starts a Shiny application that demonstrates examples of
#'  the Central Limit Theorem.
#' The app is stored internally within the package and showcases various
#'  statistical principles through interactive visualizations.
#' Check more details at vignette("shinyCLT")
#' @param n.cores Number of cores to use for calculations. Default value
#' is NULL, which means that half of the cores will be used with the ceiling
#'  rounding rule.
#' @param mode Change application behaviour when web browser tab or IDE preview
#'  is closed.
#' By default this will end the running function and stop the local shinyApp.
#' Switching to "server" mode will keep the shiny application running in a
#' background even if all with application is closed.
#' @param user_plan Specifies the parallelization strategy to use.
#'  Acceptable values are "cluster" (default), "multicore", or "multisession".
#' @return Runs shinyApp
#' @export
#' @import shiny gamlss plotly future shinycssloaders waiter shinythemes
#' @import cachem
#' @importFrom shiny shinyApp
#' @import shinyWidgets purrr
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis curve hist legend lines par points segments
#' @importFrom stats dnorm dunif qqline qqnorm qt qunif runif sd t.test var
#' @importFrom stats wilcox.test
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport
#' @importFrom parallel parLapply
#' @importFrom dplyr mutate rename slice
#' @importFrom utils globalVariables
#' @examples
#' \dontrun{
#'   CLT()  # Launch the CLT demonstration app
#' }
CLT <- function(n.cores = NULL, mode = "app", user_plan = "cluster") {

  shiny::shinyOptions(n.cores = n.cores, mode = mode, user_plan = user_plan)

  source(system.file("app", "ui.R", package = "shinyCLT"),
                                        local = TRUE)
  source(system.file("app", "server.R", package = "shinyCLT"),
                                        local = TRUE)
  source(system.file("app", "global.R", package = "shinyCLT"),
                                        local = TRUE)
  source(system.file("app", "calc_func.R", package = "shinyCLT"),
                                        local = TRUE)
  source(system.file("app", "plot_func.R", package = "shinyCLT"),
                                        local = TRUE)

  shiny::shinyApp(ui = ui, server = server)
}
