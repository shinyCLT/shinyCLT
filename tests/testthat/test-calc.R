library(testthat)

user_plan <- "cluster"

global_path <- system.file("app", "global.R", package = "shinyCLT")
calc_func_path <- system.file("app", "calc_func.R", package = "shinyCLT")
plot_func_path <- system.file("app", "plot_func.R", package = "shinyCLT")

source(global_path)
source(calc_func_path)
source(plot_func_path)


test_that("calculate_statistics works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, n = 10, R = 5)
  result <- calculate_statistics(input, distribution)
  expect_type(result, "list")
  expect_length(result$y_r, 5)
})

test_that("calculate_statistics_group2 works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1,
                group2_n = 10, R = 5)
  result <- calculate_statistics_group2(input, distribution)
  expect_type(result, "list")
  expect_length(result$y_r, 5)
})
