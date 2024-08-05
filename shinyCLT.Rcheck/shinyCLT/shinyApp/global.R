require("shiny")
require("gamlss")
require("plotly")
require("future")
require("shinycssloaders")
require("waiter")
require("shinythemes")
require("shinyWidgets")

plan(multisession)

distribution = data.frame(pos = 1:9,
    id          = c("NO","GA","BE","BI","WEI3","ZIP2","PO","EXP", "UNIF"),
    fullname    = c("Gaussian","Gamma","Beta","Bernoulli","Weibull",
                    "Zero-inflated Poisson","Poisson","Exponential", "Uniform"),
    mu.low      = c(-100,1e-2,1e-2,1e-2,1e-2,1e-2,.25,1e-2, -100),
    mu.high     = c(100,100,1 - 1e-2,0.99,100,100,100,100, 100),
    mu.value    = c(0,10,.5,.5,10,10,10,10,0),
    sigma.low   = c(.01,1e-2,0.05,NA,1e-2,1e-2,NA,NA,.01),
    sigma.high  = c(100,2,.95,NA,5,.99,NA,NA, 100),
    sigma.value = c(1,1,.1,NA,1.01,.1,NA,NA, 1),
    x.low       = c(-Inf,1e-20,1e-20,0,0,0,0,0,0),
    x.high      = c(Inf,Inf,1 - 1e-2,1,Inf,Inf,Inf,Inf,Inf),
    row.names   = c("Gaussian","Gamma","Beta","Bernoulli","Weibull",
                    "Zero-inflated Poisson","Poisson","Exponential","Uniform"),
    mu.name     = c("Theoretical mean value",
              "Theoretical probability of success value")[c(1,1,1,2,1,1,1,1,1)],
    sigma.name  = c("Theoretical standard deviation value",
                    "Theoretical shape parameter value",
                    "Theoretical probability to belong to the clump-at-zero")[
                    c(1,2,2,NA,2,3,NA,NA,1)],
    discrete    = c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE),
    stringsAsFactors = FALSE
    )[c(1,2,3,4,5,6,7,8,9),]

source("calc_func.R")
source("plot_func.R")

check_input_gr1 <- function(input) {
    invalid_input <- switch(distribution[input$distr, "id"],
                            "NO"     = is.null(input$sigma),
                            "BE"     = input$mu <= 0 | input$mu >= 1 |
                                        input$sigma <= 0 | input$sigma >= 1,
                            "GA"     = input$mu <= 0,
                            "BI"     = input$mu <= 0 | input$mu >= 1,
                            "WEI3"   = input$mu <= 0,
                            "ZIP2"   = input$mu <= 0 | input$sigma < 0 |
                                        input$sigma >= 1,
                            "PO"     = input$mu < .25,
                            "EXP"    = input$mu <= 0,
                            "IG"     = input$mu <= 0,
                            FALSE
    )
  return(invalid_input)
}

check_input_gr2 <- function(input) {

  invalid_input <- switch(distribution[input$distr, "id"],
                          "NO"     = any(is.null(input$group2.sigma),
                          is.null(input$sigma), is.null(input$group2.mu)),
                          "BE"     = any(input$mu <= 0,
                                        input$mu >= 1,
                                        input$sigma <= 0,
                                        input$sigma >= 1,
                                        input$group2.mu <= 0,
                                        input$group2.mu >= 1,
                                        input$group2.sigma <= 0,
                                        input$group2.sigma >= 1,
                                        is.null(input$group2.sigma),
                                        is.null(input$sigma),
                                        is.null(input$group2.mu)),
                          "GA"     = any(input$mu <= 0,
                                        input$group2.mu <= 0,
                                        is.null(input$group2.sigma),
                                        is.null(input$sigma),
                                        is.null(input$group2.mu)),
                          "BI"     = any(input$mu <= 0,
                                        input$mu >= 1,
                                        input$group2.mu <= 0,
                                        input$group2.mu >= 1,
                                        is.null(input$group2.mu)),
                          "WEI3"   = any(input$mu <= 0,
                                        input$group2.mu <= 0,
                                        is.null(input$group2.sigma),
                                        is.null(input$sigma),
                                        is.null(input$group2.mu)),
                          "ZIP2"   = any(input$mu <= 0,
                                        input$sigma < 0,
                                        input$sigma >= 1,
                                        input$group2.mu <= 0,
                                        input$group2.sigma < 0,
                                        input$group2.sigma >= 1,
                                        is.null(input$group2.sigma),
                                        is.null(input$sigma),
                                        is.null(input$group2.mu)),
                          "PO"     = any(input$mu < .25,
                                        input$group2.mu < .25,
                                        is.null(input$group2.mu)),
                          "EXP"    = any(input$mu <= 0,
                                        input$group2.mu <= 0,
                                        is.null(input$group2.mu)),
                          "IG"     = any(input$mu <= 0,
                                        input$group2.mu <= 0,
                                        is.null(input$group2.sigma),
                                        is.null(input$sigma),
                                        is.null(input$group2.mu)),
                          "UNIF"   = any(is.null(input$group2.sigma),
                                        is.null(input$sigma),
                                        is.null(input$group2.mu)),
                          FALSE
  )
return(invalid_input)
}

generate_uniform_sample <- function(n, mean, sigma) {
  delta = sqrt(12 * sigma^2) / 2
  a <- mean - delta
  b <- mean + delta
  runif(n, min = a, max = b)
}

pval <- function(pval, digit = 3) {
  out <- round(pval, digits = digit)
  out[is.na(pval)] <- ""
  out[out < 0.001 & !is.na(pval)] <- "<0.001"
  out
}

qUNIF <- function(x, mu, sigma = 0) {
  return(mu)
}

isin <- function(ci, theta) {
  if (!any(is.na(ci) | is.nan(ci))) {
    if (theta >= ci[1] & theta <= ci[2]) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(NA)
  }
}
