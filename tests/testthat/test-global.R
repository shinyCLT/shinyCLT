library(testthat)

global_path <- testthat::test_path("..", "..", "inst", "app", "global.R")
source(global_path)

test_that("check_input_gr1 works correctly", {
    input <- list(distr = 1, sigma = NULL, mu = 0.5)
    result <- check_input_gr1(input)
    expect_true(result)
})

test_that("check_input_gr2 works correctly", {
    input <- list(distr = 1, sigma = NULL, mu = 0.5, group2.sigma = NULL,
                    group2.mu = 0.5)
    result <- check_input_gr2(input)
    expect_true(result)
})

test_that("generate_uniform_sample works correctly", {
    result <- generate_uniform_sample(10, 0, 1)
    expect_length(result, 10)
    expect_true(all(result >= -sqrt(3) & result <= sqrt(3)))
})

test_that("pval works correctly", {
    result <- pval(0.0001)
    expect_equal(result, "<0.001")
})

test_that("qUNIF works correctly", {
    result <- qUNIF(0.5, 0, 1)
    expect_equal(result, 0)
})

test_that("isin works correctly", {
    result <- isin(c(0, 1), 0.5)
    expect_true(result)
})