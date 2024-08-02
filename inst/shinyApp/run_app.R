packages = c("shiny", "gamlss", "plotly", "future","shinycssloaders", "waiter", 
             "shinythemes", "shinyWidgets")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library(shiny)

appDir <- dirname(normalizePath(sys.frames()[[1]]$ofile))
runApp(appDir)
