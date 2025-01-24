library(testthat)
library(plotly)

global_path <- testthat::test_path("..", "..", "inst", "app", "global.R")
calc_func_path <- testthat::test_path("..", "..", "inst", "app", "calc_func.R")
plot_func_path <- testthat::test_path("..", "..", "inst", "app", "plot_func.R")

source(global_path)
source(calc_func_path)
source(plot_func_path)


test_that("generate_distribution_plot works correctly", {
    input <- list(distr = 1, mu = 0, sigma = 1)
    data <- data.frame(x = rnorm(100), y = rnorm(100))
    plot <- generate_distribution_plot(input, distribution, data)
    expect_s3_class(plot, "plotly")
})

test_that("generate_compare_distributions_plot works correctly", {
    input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1)
    group1 <- list(y_r = list(rnorm(100), rnorm(100), rnorm(100), rnorm(100),
                    rnorm(100)))
    group2 <- list(y_r = list(rnorm(100), rnorm(100), rnorm(100), rnorm(100),
                    rnorm(100)))
    plot <- generate_compare_distributions_plot(input, distribution, group1,
                                                group2)
    expect_s3_class(plot, "plotly")
})

test_that("group1_5samples_plot works correctly", {
    input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1)
    group1 <- list(y_r = list(rnorm(100), rnorm(100), rnorm(100), rnorm(100),
                    rnorm(100)))
    group2 <- list(y_r = list(rnorm(100), rnorm(100), rnorm(100), rnorm(100),
                    rnorm(100)))
    plot <- group1_5samples_plot(input, distribution, group1, group2)
    expect_s3_class(plot, "plotly")
})

test_that("generate_5samples_plot works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, n = 10, R = 5)
  data <- list(y_r = list(rnorm(10), rnorm(10), rnorm(10), rnorm(10),
              rnorm(10)))
  plot <- generate_5samples_plot(input, distribution, data)
  expect_s3_class(plot, "plotly")
})

test_that("generate_uniform_plot works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, n = 10, R = 5)
  data <- list(y_r = list(rnorm(10), rnorm(10), rnorm(10), rnorm(10),
              rnorm(10)))
  plot <- generate_uniform_plot(input, distribution, data)
  expect_s3_class(plot, "plotly")
})

test_that("generate_compare_uniform_plot works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1,
              n = 10, R = 5)
  group1 <- list(y_r = list(rnorm(10), rnorm(10), rnorm(10), rnorm(10),
                rnorm(10)))
  group2 <- list(y_r = list(rnorm(10), rnorm(10), rnorm(10), rnorm(10),
                rnorm(10)))
  plot <- generate_compare_uniform_plot(input, distribution, group1, group2)
  expect_s3_class(plot, "plotly")
})

test_that("group2_5samples_plot works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1,
                n = 10, R = 5)
  group1 <- list(y_r = list(rnorm(10), rnorm(10), rnorm(10), rnorm(10),
                rnorm(10)))
  group2 <- list(y_r = list(rnorm(10), rnorm(10), rnorm(10), rnorm(10),
                rnorm(10)))
  plot <- group2_5samples_plot(input, distribution, group1, group2)
  expect_s3_class(plot, "plotly")
})

test_that("plot_density_qq works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, n = 10, R = 5)
  data <- list(mean_r = rnorm(100), y_r = list(rnorm(10), rnorm(10), rnorm(10),
              rnorm(10), rnorm(10)))

  pdf(NULL)
  plot_density_qq(data, input)
  dev.off()

  expect_true(TRUE)
})

test_that("plot_compare_density_qq works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1,
                n = 10, R = 5)
  group1 <- list(mean_r = rnorm(100), y_r = list(rnorm(10), rnorm(10),
                rnorm(10), rnorm(10), rnorm(10)))
  group2 <- list(mean_r = rnorm(100), y_r = list(rnorm(10), rnorm(10),
                rnorm(10), rnorm(10), rnorm(10)))
  pdf(NULL)
  plot_compare_density_qq(group1, group2, input)
  dev.off()
  expect_true(TRUE)
})

test_that("plot_compare_means_qq works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1,
                n = 10, R = 5)
  group1 <- list(mean_r = rnorm(100), y_r = list(rnorm(10), rnorm(10),
                  rnorm(10), rnorm(10), rnorm(10)))
  group2 <- list(mean_r = rnorm(100), y_r = list(rnorm(10), rnorm(10),
                  rnorm(10), rnorm(10), rnorm(10)))
  pdf(NULL)
  plot_compare_means_qq(group1, group2, input)
  dev.off()
  expect_true(TRUE)
})

test_that("plot_CI works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, n = 10, R = 5)
  data <- list(ci_r = matrix(rnorm(200), ncol = 2), coverage_r = rnorm(100))

  pdf(NULL)
  plot <- plot_CI(data, input)
  dev.off()

  expect_s3_class(plot, "plotly")
})

test_that("build_table works correctly", {
  input <- list(distr = 1, mu = 0, sigma = 1, n = 10, R = 5)
  group1 <- list(all_y_r = replicate(5, rnorm(10), simplify = FALSE),
                coverage_r = rnorm(5))

  table <- build_table(input, group1)
  expect_s3_class(table, "data.frame")
  expect_true(all(c("Method", "Coverage CI", "Type I error", "Power")
              %in% colnames(table)))
})

test_that("plot_pvalue works correctly", {
  input <- list(
    distr = 1, mu = 0, sigma = 1, group2.mu = 0, group2.sigma = 1, n = 10,
    R = 5
  )
  group1 <- list(
    all_y_r = list(
      rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)
    )
  )
  group2 <- list(
    all_y_r = list(
      rnorm(10), rnorm(10), rnorm(10), rnorm(10), rnorm(10)
    )
  )

  distribution <- data.frame(
    id = c("norm"),
    discrete = c(FALSE),
    x.low = c(-Inf),
    x.high = c(Inf),
    sigma.value = c(TRUE)
  )

  pdf(NULL)
  plot_pvalue(group1, group2, input)
  dev.off()

  expect_true(TRUE)
})