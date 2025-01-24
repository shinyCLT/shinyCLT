library(testthat)

user_plan <- "cluster"

global_path <- testthat::test_path("..", "..", "inst", "app", "global.R")
calc_func_path <- testthat::test_path("..", "..", "inst", "app", "calc_func.R")
plot_func_path <- testthat::test_path("..", "..", "inst", "app", "plot_func.R")

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

test_that("calculate_wilcoxon works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1,
                group2_n = 10, n = 10, R = 5)
  group1 <- calculate_statistics(input, distribution)
  group2 <- calculate_statistics_group2(input, distribution)
  result <- calculate_wilcoxon(group1, group2, input, distribution)
  expect_type(result, "list")
})
