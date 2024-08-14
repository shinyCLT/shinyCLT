library(testthat)
library(shinyCLT)
library(callr)

test_that("CLT function is working correctly", {
    # Test for invalid mode
    expect_warning(
        CLT(mode = "invalid_mode"),
        shiny::stopApp()
    )
    # Test for mode 'app'
    expect_silent(CLT(mode = "app"))

    # Test for mode 'server'
    expect_silent(CLT(mode = "server"))
})

test_that("Check invalid n.cores input", {
    # Test invalid n.cores
    expect_message(CLT(n.cores = -1))
})

test_that("Check valid n.cores input", {

    # Test valid n.cores
    expect_message(CLT(n.cores = 4), "Listening")
})


test_that("Check valid n.cores input", {

# Define a function to run the Shiny app
run_shiny <- function() {
    CLT(n.cores = 4)
}
# Run the Shiny app in a background process
    shiny_process <- r_bg(run_shiny)
    capture.output(shiny_process)
# Allow the Shiny app to run for 3 seconds
    # Sys.sleep(3)
# Stop the Shiny app
browser()
    shiny_process$kill()
    # Check if the process has stopped
expect_true(shiny_process$is_alive() == FALSE)
})
