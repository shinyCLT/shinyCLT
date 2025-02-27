# Plot functions ----------------------------------------------------------

generate_distribution_plot <- function(input, distribution, data) {
  x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                    list(p = c(0.0005, 0.9995), mu = input$mu,
                    sigma = input$sigma)[1:(2 +
                    as.numeric(!is.na(distribution[input$distr,
                    "sigma.value"])))]
  )

  x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(x0range[1], distribution[input$distr, "x.low"])),
        min(c(x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    seq(max(c(x0range[1], distribution[input$distr, "x.low"])),
        min(c(x0range[2]), distribution[input$distr, "x.high"]), length = 1000)
  }

  d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                  list(x0, mu = input$mu, sigma = input$sigma)[1:(2 +
                as.numeric(!is.na(distribution[input$distr, "sigma.value"])))])

  xlim0 <- range(c(x0, unlist(data$y_r)))
  ylim0 <- max(d.x0) * c(-1, 1)

  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

p <- plot_ly()

# Check for 'discreet' distribution and add segments/points
if (distribution[input$distr, "discrete"]) {
  for (i in 1:length(x0)) {
    p <- p %>%
      add_segments(x = x0[i], xend = x0[i], y = 0, yend = d.x0[i],
                  line = list(color = "blue")) %>%
      add_markers(x = x0[i], y = d.x0[i],
                  marker = list(color = "blue", symbol = "circle"),
                  hovertemplate = paste("Value: %{x:.2f}",
                                        "<br>Density: %{y:.2f}<extra></extra>"))
  }
} else {
  p <- p %>%
    add_lines(x = x0, y = d.x0, line = list(color = "blue", width = 1.3),
              hovertemplate = paste("Value: %{x:.2f}",
                                    "<br>Density: %{y:.2f}<extra></extra>"))
}

p <- p %>%
  add_segments(x = input$mu, xend = input$mu, y = 0 - max(d.x0) * .1,
               yend = max(d.x0) * 1.2,
                line = list(dash = "dash", width = 1, color = "blue"),
                text = paste0("&mu; = ", input$mu),
                hoverinfo = "text") %>%
  add_text(x = input$mu, y = max(d.x0) * 1.2, text = "&mu;",
            color = I("blue"), hoverinfo = "none") %>%
  layout(title = "",
          xaxis = list(title = "",
                      range = list(xlim0[1] - diff, xlim0[2] + diff),
                      fixedrange = TRUE,
                      showline = FALSE,
                      showgrid = FALSE,
                      zeroline = FALSE),
          yaxis = list(title = "Density",
                      fixedrange = TRUE,
                      showline = FALSE,
                      showgrid = FALSE),
          showlegend = FALSE)
# Display the plot
p
}

generate_5samples_plot <- function(input, distribution, data) {

  if (input$distr == "Uniform") {
    delta = sqrt(12 * input$sigma^2) / 2
    a <- input$mu - delta
    b <- input$mu + delta
    x0range <- sapply(c(0.0005, 0.9995), \(x) qunif(x, min = a, max = b))
    x0 <- seq(x0range[1], x0range[2], length = 1000)
    d.x0 = dunif(x0, a, b)
  } else {
  x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                      list(p = c(0.0005, 0.9995), mu = input$mu,
                      sigma = input$sigma)[1:(2 +
                      as.numeric(!is.na(distribution[input$distr,
                      "sigma.value"])))]
  )

  x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(x0range[1], distribution[input$distr, "x.low"])),
        min(c(x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    seq(max(c(x0range[1], distribution[input$distr, "x.low"])),
        min(c(x0range[2]), distribution[input$distr, "x.high"]), length = 1000)
  }

  d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                  list(x0, mu = input$mu, sigma = input$sigma)[1:(2 +
                as.numeric(!is.na(distribution[input$distr, "sigma.value"])))]
  )
  }
  xlim0 <- range(c(x0, unlist(data$y_r)))
  ylim0 <- max(d.x0) * c(-1, 1)
  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

  plot_ly() %>%
      {
      fig <- .
      col <- paste0(rainbow(5), "55")
      for (r in 1:5) {
        fig <- fig %>% add_markers(x = as.vector(data$y_r[[r]]), y = r,
                                  marker = list(symbol = "circle", size = 6,
                                  color = col[r]),
                                  name = paste("Sample",  r),
                                  hovertemplate = paste(
                                  "Value: %{x:.2f}<extra></extra>")) %>%
                        add_markers(x = mean(as.vector(data$y_r[[r]])), y = r,
                                    marker = list(symbol = "cross-thin",
                                                  size = 7,
                                                  line = list(
                                                        color = "#444444",
                                                        width = 2)),
                                    name = paste("Sample", r),
                                    hovertemplate = paste(
                                    "Sample mean: %{x:.2f}<extra></extra>"),
                                    hoverlabel = list(bgcolor = col[r]))
      }
      fig
    } %>%
    layout(
      xaxis = list(
                  range = list(xlim0[1] - diff, xlim0[2] + diff),
                  showgrid = FALSE,
      showline = FALSE,
      zeroline = FALSE,
      fixedrange = TRUE),
      yaxis = list(autorange = "reversed",
                  title = "Sample",
                  showline = FALSE,
                  showgrid = TRUE,
                  showticklabels = TRUE,
                  dtick = 1,
                  zeroline = FALSE,
                  fixedrange = TRUE),
      showlegend = FALSE)  %>%
    config(displayModeBar = FALSE)
}

generate_uniform_plot <- function(input, distribution, data) {
  delta = sqrt(12 * input$sigma^2) / 2
  a <- input$mu - delta
  b <- input$mu + delta

  x0range <- sapply(c(0.0005, 0.9995), \(x) qunif(x, min = a, max = b))

  x0 <- seq(x0range[1], x0range[2], length = 1000)

  d.x0 = dunif(x0, a, b)

  xlim0 <- range(c(x0, unlist(data$y_r)))
  ylim0 <- max(d.x0) * c(-1, 1)

  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

  p <- plot_ly()

    p <- p %>%
      add_lines(x = x0, y = d.x0, line = list(color = "blue", width = 1.3),
                hovertemplate = paste("Value: %{x:.2f}",
                                      "<br>Density: %{y:.2f}<extra></extra>"))

  p <- p %>%
    add_segments(x = input$mu, xend = input$mu, y = 0 - max(d.x0) * .1,
                 yend = max(d.x0) * 1.1,
                line = list(dash = "dash", width = 1, color = "blue"),
                text = paste0("&mu; = ", input$mu),
                hoverinfo = "text") %>%
    add_segments(a, 0, a, 1 / (b - a), line = list(dash = "dot", width = 1,
                color = "blue"),hoverinfo = "none") %>%
    add_segments(b, 0, b, 1 / (b - a), line = list(dash = "dot",
                width = 1, color = "blue"),hoverinfo = "none") %>%
    add_segments(a -  (xlim0[2] - xlim0[1]) * 0.2, 0, a, 0,
                line = list(dash = "dot", width = 1,
                color = "blue"),
                hoverinfo = "none") %>%
    add_segments(b, 0, b + (xlim0[2] - xlim0[1]) * 0.2, 0,
                line = list(dash = "dot", width = 1, color = "blue"),
                hoverinfo = "none") %>%
    add_text(x = input$mu, y = max(d.x0) * 1.15, text = "&mu;",
              color = I("blue"), hoverinfo = "none") %>%
    layout(title = "",
            xaxis = list(title = "",
                        range = list(xlim0[1] - diff, xlim0[2] + diff),
                        showgrid = FALSE,
                        showline = TRUE,
                        ticks = "outside",
                        tickwidth = 2,
                        ticklen = 2,
                        tickmode = "auto",
                        zeroline = FALSE,
                        fixedrange = TRUE),
            yaxis = list(title = "Density",
                        fixedrange = TRUE,
                        showline = FALSE,
                        showgrid = FALSE,
                        zeroline = FALSE,
                        range = c(0 - max(d.x0) * 0.01, max(d.x0) * 1.2)),
            showlegend = FALSE)
  p
}

generate_compare_uniform_plot <- function(
                                        input, distribution, .group1, .group2) {
  # Plot --------------------------------------------------------------------
  delta = sqrt(12 * input$sigma^2) / 2
  a <- input$mu - delta
  b <- input$mu + delta

  delta2 = sqrt(12 * input$group2.sigma^2) / 2
  a2 <- input$group2.mu - delta2
  b2 <- input$group2.mu + delta2

  group1.x0range <- sapply(c(0.0005, 0.9995), \(x) qunif(x, min = a, max = b))

  group2.x0range <- sapply(c(0.0005, 0.9995), \(x) qunif(x, min = a2, max = b2))

  group1.x0 <- seq(group1.x0range[1],  group1.x0range[2], length = 1000)
  group2.x0 <- seq(group2.x0range[1], group2.x0range[2], length = 1000)

  group1.d.x0 = dunif(group1.x0, a, b)
  group2.d.x0 = dunif(group2.x0, a2, b2)

  xlim0 <- range(c(group1.x0, unlist(.group1$y_r),
                  group2.x0, unlist(.group2$y_r)))

  ylim0 <- max(group1.d.x0, group2.d.x0) * c(.1, 1.2)

  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

  p <- plot_ly()

    p <- p %>%
      add_lines(x = group1.x0, y = group1.d.x0,
                line = list(color = "blue", width = 1.3),
                hovertemplate = paste("Value: %{x:.2f}",
                                  "<br>Density: %{y:.2f}<extra></extra>")) %>%
      add_lines(x = group2.x0, y = group2.d.x0,
                line = list(color = "red", width = 1.3),
                hovertemplate = paste("Value: %{x:.2f}",
                                      "<br>Density: %{y:.2f}<extra></extra>"))

  p <- p %>%
    add_segments(x = input$mu, xend = input$mu, y = 0, yend = ylim0[2] * 1.08,
                  line = list(dash = "dash", width = 1, color = "blue"),
                  text = paste0("&mu;<sub>1</sub> = ", input$mu),
                  hoverinfo = "text") %>%
    add_text(x = input$mu + abs(xlim0[2] - xlim0[1]) * -0.0101,
             y = ylim0[2] * 1.10, text = "&mu;<sub>1</sub>",
              color = I("blue"), hoverinfo = "none") %>%
    add_segments(x = input$group2.mu, xend = input$group2.mu,
                  y = 0, yend = ylim0[2] * 1.08,
                  line = list(dash = "dash", width = 1, color = "red"),
                  text = paste0("&mu;<sub>2</sub> = ", input$group2.mu),
                  hoverinfo = "text") %>%
    add_segments(a, 0, a, 1 / (b - a), line = list(dash = "dot", width = 1,
                  color = "blue"),hoverinfo = "none") %>%
    add_segments(b, 0, b, 1 / (b - a), line = list(dash = "dot", width = 1,
                  color = "blue"),hoverinfo = "none") %>%
    add_segments(a -  (xlim0[2] - xlim0[1]) * 0.1, 0, a, 0,
                line = list(dash = "dot", width = 1, color = "blue"),
                hoverinfo = "none") %>%
    add_segments(b, 0, b + (xlim0[2] - xlim0[1]) * 0.1, 0,
                line = list(dash = "dot", width = 1, color = "blue"),
                hoverinfo = "none") %>%
    add_segments(a2, 0, a2, 1 / (b2 - a2), line = list(dash = "dot", width = 1,
                color = "red"),hoverinfo = "none") %>%
    add_segments(b2, 0, b2, 1 / (b2 - a2), line = list(dash = "dot", width = 1,
                color = "red"),hoverinfo = "none") %>%
    add_segments(a2 -  (xlim0[2] - xlim0[1]) * 0.1, 0, a2, 0,
                line = list(dash = "dot", width = 1, color = "red"),
                hoverinfo = "none") %>%
    add_segments(b2, 0, b2 + (xlim0[2] - xlim0[1]) * 0.1, 0,
                  line = list(dash = "dot", width = 1, color = "red"),
                  hoverinfo = "none") %>%
    add_text(x = input$group2.mu + abs(xlim0[2] - xlim0[1]) * 0.0101,
              y = ylim0[2] * 1.1, text = "&mu;<sub>2</sub>",
              color = I("red"), hoverinfo = "none") %>%
    layout(title = "",
            xaxis = list(title = "",
                        range = list(xlim0[1] - diff, xlim0[2] + diff),
                        fixedrange = TRUE,
                        showline = FALSE,
                        showgrid = FALSE,
                        zeroline = FALSE),
            yaxis = list(title = "Density",
                        fixedrange = TRUE,
                        showline = FALSE,
                        showgrid = FALSE),
            showlegend = FALSE)
  # Display the plot
  p
}

generate_compare_distributions_plot <- function(input, distribution,
                                                .group1, .group2) {
  # group1 ------------------------------------------------------------------

  group1.x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                            list(p = c(0.0005, 0.9995), mu = input$mu,
                                sigma = input$sigma)[1:(2 +
                                as.numeric(!is.na(distribution[input$distr,
                                "sigma.value"])))]
  )

  group1.x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(group1.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group1.x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    seq(max(c(group1.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group1.x0range[2]), distribution[input$distr, "x.high"]),
        length = 1000)
  }

  group1.d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                        list(group1.x0, mu = input$mu,
                        sigma = input$sigma)[1:(2 +
                        as.numeric(!is.na(distribution[input$distr,
                        "sigma.value"])))]
  )

  # group2 -------------------------------------------------------------------

  group2.x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                            list(p = c(0.0005, 0.9995), mu = input$group2.mu,
                            sigma = input$group2.sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))]
  )

  group2.x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(group2.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group2.x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    p
    seq(max(c(group2.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group2.x0range[2]), distribution[input$distr, "x.high"]),
              length = 1000)
  }

  group2.d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                        list(group2.x0, mu = input$group2.mu,
                        sigma = input$group2.sigma)[1:(2 +
                        !is.na(distribution[input$distr, "sigma.value"]))]
  )

  # margins -----------------------------------------------------------------
  xlim0 <- range(c(group1.x0, unlist(.group1$y_r), group2.x0,
                  unlist(.group2$y_r)))
  ylim0 <- max(group1.d.x0, group2.d.x0) * c(.1, 1.2)

  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

  # density plots --------------------------------------------------------------
  p <- plot_ly()

  # Check for 'discreet' distribution and add segments/points
  if (distribution[input$distr, "discrete"]) {

    for (i in 1:length(group1.x0)) {
      p <- p %>%
        add_segments(x = group1.x0[i], xend = group1.x0[i], y = 0,
                    yend = group1.d.x0[i],
                    line = list(color = "blue")) %>%
        add_markers(x = group1.x0[i], y = group1.d.x0[i],
                    marker = list(color = "blue", symbol = "circle"),
                    hovertemplate = paste("Value: %{x:.2f}",
                                        "<br>Density: %{y:.2f}<extra></extra>"))
    }
    for (i in 1:length(group2.x0)) {
      p <- p %>%
        add_segments(x = group2.x0[i], xend = group2.x0[i],
                      y = 0, yend = group2.d.x0[i],
                      line = list(color = "red")) %>%
        add_markers(x = group2.x0[i], y = group2.d.x0[i],
                    marker = list(color = "red", symbol = "circle"),
                    hovertemplate = paste("Value: %{x:.2f}",
                                        "<br>Density: %{y:.2f}<extra></extra>"))
    }
  } else {
    p <- p %>%
      add_lines(x = group1.x0, y = group1.d.x0, line = list(color = "blue",
                width = 1.3),
                hovertemplate = paste("Value: %{x:.2f}",
                                  "<br>Density: %{y:.2f}<extra></extra>")) %>%
      add_lines(x = group2.x0, y = group2.d.x0, line = list(color = "red",
                width = 1.3),
                hovertemplate = paste("Value: %{x:.2f}",
                                      "<br>Density: %{y:.2f}<extra></extra>"))
  }

  p <- p %>%
    add_segments(x = input$mu, xend = input$mu, y = 0 - ylim0[1],
                  yend = ylim0[2],
                  line = list(dash = "dash", width = 1, color = "blue"),
                  text = paste0("&mu;<sub>1</sub> = ", input$mu),
                  hoverinfo = "text") %>%
    add_text(x = input$mu + abs(xlim0[2] - xlim0[1]) * -0.0101, y = ylim0[2],
            text = "&mu;<sub>1</sub>", color = I("blue"),
            hoverinfo = "none") %>%
    add_segments(x = input$group2.mu, xend = input$group2.mu, y = 0 - ylim0[1],
                  yend = ylim0[2],
                  line = list(dash = "dash", width = 1, color = "red"),
                  text = paste0("&mu;<sub>2</sub> = ", input$group2.mu),
                  hoverinfo = "text") %>%
    add_text(x = input$group2.mu + abs(xlim0[2] - xlim0[1]) * 0.0101,
            y = ylim0[2], text = "&mu;<sub>2</sub>", color = I("red"),
            hoverinfo = "none") %>%
    layout(title = "",
            xaxis = list(title = "",
                        range = list(xlim0[1] - diff, xlim0[2] + diff),
                        fixedrange = TRUE,
                        showline = FALSE,
                        showgrid = FALSE,
                        zeroline = FALSE),
            yaxis = list(title = "Density",
                        fixedrange = TRUE,
                        showline = FALSE,
                        showgrid = FALSE),
            showlegend = FALSE)
  # Display the plot
  p
}

group1_5samples_plot <- function(input, distribution, .group1, .group2) {
  if (check_input_gr1(input)) return()
  if (check_input_gr2(input)) return()
  # if (input$distr != "Exponential" |  input$distr != "Bernoulli")  {
  # if (is.null(input$group2.sigma)) return()
  # }

  if (input$distr == "Uniform") {
    delta = sqrt(12 * input$sigma^2) / 2
    a <- input$mu - delta
    b <- input$mu + delta

    delta2 = sqrt(12 * input$group2.sigma^2) / 2
    a2 <- input$group2.mu - delta2
    b2 <- input$group2.mu + delta2

    group1.x0range <- sapply(c(0.0005, 0.9995),
                                  \(x) qunif(x, min = a, max = b))

    group2.x0range <- sapply(c(0.0005, 0.9995),
                                  \(x) qunif(x, min = a2, max = b2))

    group1.x0 <- seq(group1.x0range[1],  group1.x0range[2], length = 1000)
    group2.x0 <- seq(group2.x0range[1], group2.x0range[2], length = 1000)

    group1.d.x0 = dunif(group1.x0, a, b)
    group2.d.x0 = dunif(group2.x0, a2, b2)
  } else {
  group1.x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                            list(p = c(0.0005, 0.9995), mu = input$mu,
                                  sigma = input$sigma)[1:(2 +
                                  as.numeric(!is.na(distribution[input$distr,
                                  "sigma.value"])))]
  )

  group1.x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(group1.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group1.x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    seq(max(c(group1.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group1.x0range[2]), distribution[input$distr, "x.high"]),
              length = 1000)
  }

  group1.d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                          list(group1.x0, mu = input$mu,
                          sigma = input$sigma)[1:(2 +
                          as.numeric(!is.na(distribution[input$distr,
                          "sigma.value"])))]
  )


  # group2 -------------------------------------------------------------------
  group2.x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                            list(p = c(0.0005, 0.9995), mu = input$group2.mu,
                            sigma = input$group2.sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))]
  )

  group2.x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(group2.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group2.x0range[2]), distribution[input$distr, "x.high"]))
  } else {
  p
    seq(max(c(group2.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group2.x0range[2]), distribution[input$distr, "x.high"]),
              length = 1000)
  }

  group2.d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                          list(group2.x0, mu = input$group2.mu,
                          sigma = input$group2.sigma)[1:(2 +
                          !is.na(distribution[input$distr, "sigma.value"]))]
  )
  }

  # margins -----------------------------------------------------------------
  xlim0 <- range(c(group1.x0, unlist(.group1$y_r), group2.x0,
                  unlist(.group2$y_r)))
  ylim0 <- max(group1.d.x0, group2.d.x0) * c(.1, 1.2)

  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

  plot_ly() %>%
    {
      fig <- .
      col <- paste0(rainbow(5), "55")
      for (r in 1:5) {
        fig <- fig %>% add_markers(x = as.vector(.group1$y_r[[r]]), y = r,
                                    marker = list(symbol = "circle", size = 6,
                                    color = col[r]),
                                    name = paste("Sample", r),
                  hovertemplate = paste("Value: %{x:.2f}<extra></extra>")) %>%
                      add_markers(x = mean(as.vector(.group1$y_r[[r]])), y = r,
                                    marker = list(symbol = "triangle-up-open",
                                    color = "#444444", size = 7,
                                    line = list(color = "#444444", width = 2)),
                                    name = paste("Sample", r),
                  hovertemplate = paste("Sample mean: %{x:.2f}<extra></extra>"),
                                    hoverlabel = list(bgcolor = col[r]))
      }
      fig
    } %>%
    layout(
      xaxis = list(
        range = list(xlim0[1] - diff, xlim0[2] + diff),
        showgrid = FALSE,
        showline = FALSE,
        zeroline = FALSE,
        fixedrange = TRUE),
      yaxis = list(autorange = "reversed",
                    title = "Group 1<br>Sample",
                    showline = FALSE,
                    showgrid = TRUE,
                    showticklabels = TRUE,
                    dtick = 1,
                    zeroline = FALSE,
                    fixedrange = TRUE),
      showlegend = FALSE)  %>%
    config(displayModeBar = FALSE)
}

group2_5samples_plot <- function(input, distribution, .group1, .group2) {

  if (input$distr == "Uniform") {
    delta = sqrt(12 * input$sigma^2) / 2
    a <- input$mu - delta
    b <- input$mu + delta

    delta2 = sqrt(12 * input$group2.sigma^2) / 2
    a2 <- input$group2.mu - delta2
    b2 <- input$group2.mu + delta2

    group1.x0range <- sapply(c(0.0005, 0.9995),
                            \(x) qunif(x, min = a, max = b))
    group2.x0range <- sapply(c(0.0005, 0.9995),
                            \(x) qunif(x, min = a2, max = b2))

    group1.x0 <- seq(group1.x0range[1],  group1.x0range[2], length = 1000)
    group2.x0 <- seq(group2.x0range[1], group2.x0range[2], length = 1000)

    group1.d.x0 = dunif(group1.x0, a, b)
    group2.d.x0 = dunif(group2.x0, a2, b2)
  } else {
  group1.x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                            list(p = c(0.0005, 0.9995), mu = input$mu,
                                  sigma = input$sigma)[1:(2 +
                                  as.numeric(!is.na(distribution[input$distr,
                                  "sigma.value"])))]
  )

  group1.x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(group1.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group1.x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    seq(max(c(group1.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group1.x0range[2]), distribution[input$distr, "x.high"]),
              length = 1000)
  }

  group1.d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                          list(group1.x0, mu = input$mu,
                              sigma = input$sigma)[1:(2 +
                              as.numeric(!is.na(distribution[input$distr,
                              "sigma.value"])))]
  )

  # group2 -------------------------------------------------------------------
  group2.x0range <- do.call(paste0("q", distribution[input$distr, "id"]),
                            list(p = c(0.0005, 0.9995), mu = input$group2.mu,
                                  sigma = input$group2.sigma)[1:(2 +
                                  as.numeric(!is.na(distribution[input$distr,
                                  "sigma.value"])))]
  )

  group2.x0 <- if (distribution[input$distr, "discrete"]) {
    seq(max(c(group2.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group2.x0range[2]), distribution[input$distr, "x.high"]))
  } else {
    p
    seq(max(c(group2.x0range[1], distribution[input$distr, "x.low"])),
        min(c(group2.x0range[2]), distribution[input$distr, "x.high"]),
        length = 1000)
  }

  group2.d.x0 <- do.call(paste0("d", distribution[input$distr, "id"]),
                          list(group2.x0, mu = input$group2.mu,
                          sigma = input$group2.sigma)[1:(2 +
                          !is.na(distribution[input$distr,
                          "sigma.value"]))]
  )
  }
  # margins -----------------------------------------------------------------
  xlim0 <- range(c(group1.x0, unlist(.group1$y_r), group2.x0,
                  unlist(.group2$y_r)))
  ylim0 <- max(group1.d.x0, group2.d.x0) * c(.1, 1.2)

  diff <- abs(xlim0[2] - xlim0[1]) * 0.009

  if (is.null(.group2)) {
  return()
  }
  plot_ly() %>%
    {
      fig <- .
      col <- paste0(rainbow(5), "55")
      for (r in 1:5) {
        fig <- fig %>% add_markers(x = as.vector(.group2$y_r[[r]]), y = r,
                                    marker = list(symbol = "circle", size = 6,
                                    color = col[r]),
                                    name = paste("Sample", r),
                  hovertemplate = paste("Value: %{x:.2f}<extra></extra>")) %>%
                      add_markers(x = mean(as.vector(.group2$y_r[[r]])), y = r,
                                    marker = list(symbol = "square-open",
                                    size = 7, color = "#444444",
                                    line = list(color = "#444444", width = 2)),
                                    name = paste("Sample", r),
                  hovertemplate = paste("Sample mean: %{x:.2f}<extra></extra>"),
                                    hoverlabel = list(bgcolor = col[r]))
      }
      fig
    } %>%
    layout(
      xaxis = list(
        range = list(xlim0[1] - diff, xlim0[2] + diff),
        showgrid = FALSE,
        showline = FALSE,
        zeroline = FALSE,
        fixedrange = TRUE),
      yaxis = list(autorange = "reversed",
                    title = "Group 2<br>Sample",
                    showline = FALSE,
                    showgrid = TRUE,
                    showticklabels = TRUE,
                    dtick = 1,
                    zeroline = FALSE,
                    fixedrange = TRUE),
      showlegend = FALSE)  %>%
    config(displayModeBar = FALSE)
}

# Plot hist ---------------------------------------------------------------
plot_density_qq <- function(data, input) {

oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))

  hist_muhat <- hist(data$mean_r, plot = FALSE, nclass = ceiling(sqrt(input$R)))
  mean_muhat <- mean(data$mean_r)
  sdev_muhat <- sqrt(var(data$mean_r))

  # Limits
  xlim1 <- input$mu + c(-1, 1) * abs(max(data$mean_r - input$mu))
  x1 <- seq(xlim1[1], xlim1[2], length = 1000)
  d.x1 <- dnorm(x1, mean_muhat, sdev_muhat)
  ylim1 <- c(0, max(c(unlist(d.x1), hist_muhat$density)) * 1.2)

  ## Plots

  par(mfrow = c(1, 2))

  # Plot estimation of density of the mean
  plot(1, 1, xlim = xlim1, ylim = ylim1, pch = "", axes = FALSE,
        xlab = "mean values", ylab = "", main = "Estimated density of the mean")
  hist(data$mean_r, col = "light gray", border = "light gray",
        nclass = ceiling(sqrt(input$R)), add = TRUE, prob = TRUE)
  axis(1, pos = 0)
  axis(2, las = 2)
  abline(h = 0, col = "black")
  lines(x1, d.x1, col = "blue")
  abline(v = input$mu, col = "blue", lty = 3, lwd = 2)
  axis(3, at = input$mu, labels = expression(mu), tick = FALSE,
        col.axis = "blue", cex.axis = 1.25, padj = 1.5)
  legend("topright", legend = "Normal\napproximation", lty = 1, col = "blue",
          box.lwd = NA, cex = 1)

  # Add mean of samples 1 to 5
  R <- length(data$y_r)
  for (r in 1:length(data$y_r)) {
    points(mean(data$y_r[[r]]), 0, pch = 16, col = rainbow(R)[r], cex = 1.1,
                                  lwd = 1)
  }
  par(new = FALSE)
  # Plot qq norm
  qqnorm((data$mean_r - mean_muhat) / sdev_muhat, col = "gray", axes = FALSE)
  axis(1)
  axis(2, las = 2)
  abline(0, 1, col = "blue")
}

# -------------------------------------------------------------------------
plot_compare_density_qq <- function(.group1, .group2, input) {

oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))

  par(xpd = TRUE)

  group1_hist_muhat <- hist(.group1$mean_r, plot = FALSE,
                            nclass = ceiling(sqrt(input$R)))
  group1_mean_muhat <- mean(.group1$mean_r)
  group1_sdev_muhat <- sqrt(var(.group1$mean_r))

  group2_hist_muhat <- hist(.group2$mean_r, plot = FALSE,
                            nclass = ceiling(sqrt(input$R)))
  group2_mean_muhat <- mean(.group2$mean_r)
  group2_sdev_muhat <- sqrt(var(.group2$mean_r))

  # Limits
  xlim1 <- range(max(input$mu) + c(-1, 1) * abs(max(.group1$mean_r - input$mu)),
                 max(input$group2.mu) + c(-1, 1) *
                  abs(max(.group2$mean_r - input$group2.mu)))

  x1 <- seq(xlim1[1], xlim1[2], length = 1000)
  d.x1 <- dnorm(x1, group1_mean_muhat, group1_sdev_muhat)
  d.x2 <- dnorm(x1, group2_mean_muhat, group2_sdev_muhat)
  ylim1 <- c(0, max(c(unlist(list(d.x1, d.x2)), group1_hist_muhat$density,
              group2_hist_muhat$density)) * 1.2)

  # ## Plots
  par(mfrow = c(1, 2), mar = c(7,2,4,2))

  # Plot estimation of density of the mean
  plot(1, 1, xlim = xlim1, ylim = ylim1, pch = "", axes = FALSE,
        xlab = "", ylab = "", main = "Estimated density of the mean")
  hist(.group1$mean_r, col = "#8DC5FF", border = "light gray",
        nclass = ceiling(sqrt(input$R)), add = TRUE, prob = TRUE)
  lines(x1, d.x1, col = "blue")
  segments(x0 = input$mu, x1 = input$mu, y0 = ylim1[1] * -0.1, ylim1[2] * 1.1,
            lty = 3, lwd = 2, col = "blue")
  axis(3, at = input$mu, labels = expression(mu[1]), tick = FALSE,
        col.axis = "blue", cex.axis = 1.25, padj = 1.5, hadj = 1.1)

  # GR2 ---------------------------------------------------------------------
  par(new = TRUE)
  # Plot estimation of density of the mean
  plot(1, 1, xlim = xlim1, ylim = ylim1, pch = "", axes = FALSE,
      xlab = "", ylab = "", main = "")
  hist(.group2$mean_r, col = paste0("#FFB48D", 60), border = "light gray",
        nclass = ceiling(sqrt(input$R)),
      add = TRUE, prob = TRUE)
  axis(1, pos = 0)
  axis(2, las = 2)
  segments(x0 = xlim1[1] + xlim1[1] * .075,x1 = xlim1[2] + xlim1[2] * .075,
            y0 = 0, y1 = 0, lwd = 2, col = "black")
  lines(x1, d.x2, col = "red")
  segments(x0 = input$group2.mu, x1 = input$group2.mu, y0 = ylim1[1] * - 0.1,
            ylim1[2] * 1.1, lty = 3, lwd = 2, col = "red")
  axis(3, at = input$group2.mu, labels = expression(mu[2]), tick = FALSE,
        col.axis = "red", cex.axis = 1.25, padj = 1.5, hadj = -.5)
  par(new = TRUE)
  legend("bottomright", inset = c(.0, -0.42),
          legend = c("Normal approximation (group 1)",
                      "Normal approximation (group 2)"),
          lty = c(1, 1), col = c("blue", "red"), box.lwd = NA, cex = 1,
                y.intersp = 2, bg = NULL)

  # Add mean of samples 1 to 5
  group1.R <- length(.group1$y_r)
  for (r in 1:length(.group1$y_r)) {
    points(mean(.group1$y_r[[r]]), 0, pch = 17, col = rainbow(group1.R)[r])
  }

  second.palette.R <- c("#DECCB7", "#98DFAF", "#5FB49C",
                        "#414288", "#682D63")

  group2.R <- length(.group2$y_r)
  for (r in 1:group2.R) {
    points(mean(.group2$y_r[[r]]), 0, pch = 15, col = rainbow(group2.R)[r])
  }

  # Plot qq norm
  qqnorm((.group2$mean_r - group2_mean_muhat) / group2_sdev_muhat,
        col = "#FF000040", axes = FALSE)
  axis(1)
  axis(2, las = 2)
  segments(-3, -3, 3, 3, col = "red", lty = 2, lwd = 1.5)

  par(new = TRUE)
  qqnorm((.group1$mean_r - group1_mean_muhat) / group1_sdev_muhat,
          col = "#0000FF40", axes = FALSE)
  par(new = TRUE)
  segments(-3, -3, 3, 3, col = "blue", lty = 2, lwd = 1.5)

}

plot_compare_means_qq <- function(.group1, .group2, input) {

oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))

  group1_mean_muhat <- mean(.group1$mean_r)
  group1_sdev_muhat <- sqrt(var(.group1$mean_r))

  group2_mean_muhat <- mean(.group2$mean_r)
  group2_sdev_muhat <- sqrt(var(.group2$mean_r))

  par(mfrow = c(1, 2), mar = c(2.2, 2.4, 7, 2), bg = "#FFFFFF00")

  y1.h <- max(dnorm(.group1$mean_r - .group2$mean_r,
                    sd = sd(.group1$mean_r - .group2$mean_r),
                    mean = mean(.group1$mean_r - .group2$mean_r)))
  yhat <- max(hist(.group1$mean_r - .group2$mean_r,plot = FALSE)$density)

  hist(.group1$mean_r - .group2$mean_r,
        main = "Estimated density of differences in the means",
        prob = TRUE, col = "grey80", border = "#FFFFFF00",
        ylim = c(0, y1.h), las = 1)
  curve(dnorm(x, mean = mean(.group1$mean_r - .group2$mean_r),
  sd = sd(.group1$mean_r - .group2$mean_r)), col = "black", lwd = 1,
                                            lty = 1, add = TRUE)
  segments(x0 = input$mu - input$group2.mu, x1 = input$mu - input$group2.mu,
           y0 = yhat * -0.07, y1 = yhat * 1.2, lty = 3, lwd = 1.5,
            col = "black")
  group2.R <- length(.group2$y_r)
  for (r in 1:group2.R) {
    points((.group1$mean_r[r] - .group2$mean_r[r]), 0, pch = 3,
            col = rainbow(5)[r], cex = 1.2, lwd = 1.7)
  }

  qqnorm((((.group1$mean_r - .group2$mean_r) -
          (group1_mean_muhat - group2_mean_muhat)) /
          (group1_sdev_muhat - group2_sdev_muhat)),
            col = "grey80", axes = FALSE)
  axis(1)
  axis(2, las = 2)
  qqline((((.group1$mean_r - .group2$mean_r) -
          (group1_mean_muhat - group2_mean_muhat)) /
          (group1_sdev_muhat - group2_sdev_muhat)), col = "blue", lty = 2)
}

# Plot CI -----------------------------------------------------------------
plot_CI <- function(data, input) {

if (is.null(data)) return()
  plot_data <- as.data.frame(data$ci_r) %>%
    rename(ci_r1 = V1, ci_r2 = V2) %>%
    mutate(ci_r1 = ifelse(is.nan(ci_r1) | is.na(ci_r1), 0.00001, ci_r1)) %>%
    mutate(ci_r2 = ifelse(is.nan(ci_r2) | is.na(ci_r2), -0.00001, ci_r2)) %>%
    mutate(
      ci_contains_mu = ifelse(apply(cbind(ci_r1, ci_r2), 1, isin,
                                          theta = input$mu),
                              "CI_TRUE", "CI_FALSE")
    ) %>%
    slice(1:100) %>%
    mutate(sample = 1:100) %>%
    mutate(ci_points = ifelse(round(ci_r1,5) == round(ci_r2,5),
            "CI_POINTS", "CI_NOPOINTS"))

  lim.range <- abs(max(plot_data$ci_r1, plot_data$ci_r2) -
                    min(plot_data$ci_r1, plot_data$ci_r2))
  expand.min <- min(plot_data$ci_r1, input$mu) - lim.range * 0.05
  expand.max <- max(plot_data$ci_r2, input$mu) + lim.range * 0.05

  plot_ly() %>%
    add_segments(data = plot_data[plot_data$ci_contains_mu == "CI_TRUE", ],
                  x = ~ci_r1, xend = ~ci_r2, y = ~sample, yend = ~sample,
                  line = list(color = "grey", width = .5),
                  name = "CI contain the true mean value",
                  text = ~paste("CI low:", round(ci_r1, 2),
                  "<br>CI high:", round(ci_r2, 2), "<br> Sample:", sample),
                  hoverinfo = "text",
                  hoverlabel = list(bgcolor = "white")) %>%
    add_segments(data = plot_data[plot_data$ci_contains_mu == "CI_FALSE", ],
                  x = ~ci_r1, xend = ~ci_r2, y = ~sample, yend = ~sample,
                  line = list(color = "red", width = .5),
                  name = "CI does not contain the true mean value",
                  text = ~paste("CI low:", round(ci_r1, 2), "<br>CI high:",
                  round(ci_r2, 2), "<br> Sample:", sample),
                  hoverinfo = "text") %>%
    add_lines(x = c(input$mu, input$mu), y = c(-7, 107),
              line = list(dash = "dash", width = 1, color = "blue"),
              showlegend = FALSE,
              text = paste0("&#956;"," = ", input$mu),
              hoverinfo = "text")  %>%
    add_markers(data = plot_data[plot_data$ci_points == "CI_POINTS", ],
                x = ~ci_r1,
                y = ~sample,
                marker = list(size = 2,
                              color = "red"),
                symbols = "circle",
                name = "CI does not contain the true mean value",
                text = ~paste("CI low:", pval(ci_r1), "<br>CI high:",
                pval(ci_r2), "<br> Sample:", sample),
                hoverinfo = "text",
                showlegend = FALSE) %>%
    layout(autosize = 0,
            title = "",
            xaxis = list(
              title = paste0("Estimated coverage of 95% CI over ", input$R,
                            " samples = ",
                            round(mean(data$coverage_r) * 100, 2), "%"),
              showgrid = FALSE,
              showline = TRUE,
              ticks = "outside",
              tickwidth = 2,
              ticklen = 2,
              range = c(expand.min, expand.max),
              tickmode = "auto",
              zeroline = FALSE),
            yaxis = list(title = "Sample",
                        showline = FALSE,
                        showgrid = FALSE,
                        showticklabels = TRUE,
                        dtick = 10,
                        tickmode = "array",
                        tickvals = c(1,seq(10,100,10)),
                        zeroline = FALSE,
                        autorange = "reversed"),
            showlegend = TRUE)

}

plot_CI_group2 <- function(.group1, .group2, input, .ttests) {
  if (is.null(input$ttest)) {
  return()
  } else {
    .results <- data.frame(
      switch(
        input$ttest,
        "Wilcoxon" = .ttests[["wilcox"]],
        "Welch" = .ttests[["welch"]],
        "Student's" = .ttests[["student"]],
        stop("Invalid ttest input")
      )
    )
    if (length(.results) <= 0) return()
    ci_both <- .results[1:2]
    pvalue <- .results[3]
    }

    rownames(ci_both) <- c()
    truediff <- input$mu - input$group2.mu

    xlim1 <- c(min(ci_both, truediff), max(ci_both, truediff))
    ylim1 <- c(0, 100)

    colnames(ci_both) <- c("X1", "X2")

    plot_data <- as.data.frame(ci_both) %>%
      rename(ci_r1 = X1, ci_r2 = X2) %>%
      mutate(
        ci_contains_mu = ifelse(ci_r1 <= truediff & ci_r2 >= truediff,
                                "CI_TRUE", "CI_FALSE")
      ) %>%
      slice(1:100) %>%
      mutate(sample = 1:100) %>%
      mutate(ci_points = ifelse(round(ci_r1,5) == round(ci_r2,5),
                                "CI_POINTS", "CI_NOPOINTS"))

lim.range <- abs(max(plot_data$ci_r1, plot_data$ci_r2, truediff) -
                min(plot_data$ci_r1, plot_data$ci_r2, truediff))
expand.min <- min(plot_data$ci_r1, truediff) - lim.range * 0.05
expand.max <- max(plot_data$ci_r2, truediff) + lim.range * 0.05

    plot_ly() %>%
      add_segments(data = plot_data[plot_data$ci_contains_mu == "CI_TRUE", ],
                    x = ~ci_r1, xend = ~ci_r2, y = ~sample, yend = ~sample,
                    line = list(color = "grey", width = .5),
                    name = "CI contain the true mean value",
                    text = ~paste("CI low:", round(ci_r1, 2), "<br>CI high:",
                                  round(ci_r2, 2), "<br> Sample:", sample),
                    hoverinfo = "text",
                    hoverlabel = list(bgcolor = "white")) %>%
      add_segments(data = plot_data[plot_data$ci_contains_mu == "CI_FALSE", ],
                    x = ~ci_r1, xend = ~ci_r2, y = ~sample, yend = ~sample,
                    line = list(color = "red", width = .5),
                    name = "CI does not contain the true mean value",
                    text = ~paste("CI low:", round(ci_r1, 2),
                    "<br>CI high:", round(ci_r2, 2), "<br> Sample:", sample),
                    hoverinfo = "text") %>%
      add_lines(x = c(truediff, truediff), y = c(-7, 107),
                line = list(dash = "dash", width = 1, color = "blue"),
                showlegend = FALSE,
                text = paste0("&mu;<sub>1</sub> - &mu;<sub>2</sub> "),
                hoverinfo = "text")  %>%
      add_markers(data = plot_data[plot_data$ci_points == "CI_POINTS", ],
                  x = ~ci_r1,
                  y = ~sample,
                  marker = list(size = 2,
                                color = "red"),
                  symbols = "circle",
                  name = "CI does not contain the true mean value",
                  text = ~paste("CI low:", pval(ci_r1), "<br>CI high:",
                                  pval(ci_r2), "<br> Sample:", sample),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
      layout(autosize = 0,
              title = "",
              xaxis = list(
                title = "",
                showgrid = FALSE,
                showline = TRUE,
                ticks = "outside",
                tickwidth = 2,
                ticklen = 2,
                range = c(expand.min, expand.max),
                tickmode = "array",
                zeroline = FALSE),
              yaxis = list(title = "Sample",
                          showline = FALSE,
                          showgrid = FALSE,
                          showticklabels = TRUE,
                          dtick = 10,
                          tickmode = "array",
                          tickvals = c(1,seq(10,100,10)),
                          zeroline = FALSE,
                          autorange = "reversed"),
              showlegend = TRUE)
}

build_table_compare <- function(input, .ttests) {
  if (length(.ttests) <= 0) return()

  ci.st     <- data.frame(.ttests[["student"]])[1:2]
  ci.wlch   <- data.frame(.ttests[["welch"]])[1:2]
  ci.wilcox <- data.frame(.ttests[["wilcox"]])[1:2]

  pvalues.st     <- data.frame(.ttests[["student"]])[3]
  pvalues.wlch   <- data.frame(.ttests[["welch"]])[3]
  pvalues.wilcox <- data.frame(.ttests[["wilcox"]])[3]

  truediff <- input$mu - input$group2.mu

  median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                            args = list(0.5, mu = input$mu,
                            sigma = input$sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))])

  median.group2 <- do.call(paste0("q", distribution[input$distr, "id"]),
                            args = list(0.5, mu = input$group2.mu,
                            sigma = input$group2.sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))])

  true.median <- median.group1 - median.group2

  cover.st     <- round(mean(ci.st[,1] <= truediff &
                             ci.st[,2] >= truediff) * 100, 2)
  cover.wlch   <- round(mean(ci.wlch[,1] <= truediff &
                             ci.wlch[,2] >= truediff) * 100, 2)
  cover.wilcox <- round(mean(ci.wilcox[,1] <= true.median &
                             ci.wilcox[,2] >= true.median) * 100, 2)

  power.st     <-  round(mean(pvalues.st < .05) * 100, 2)
  power.wlch   <-  round(mean(pvalues.wlch  < .05) * 100, 2)
  power.wilcox <-  round(mean(pvalues.wilcox < .05) * 100, 2)

  methods <- c("Student's t-test", "Welch's t-test", "Wilcoxon")

  result.table <- as.data.frame(cbind(methods, rbind(cover.st, cover.wlch,
                                                      cover.wilcox),
                                      rbind(ifelse(input$mu == input$group2.mu,
                                                    power.st, "-"),
                                          ifelse(input$mu == input$group2.mu,
                                                    power.wlch, "-"),
                                          ifelse(median.group1 == median.group2,
                                                    power.wilcox, "-")),
                                      rbind(ifelse(input$mu != input$group2.mu,
                                                    power.st, "-"),
                                          ifelse(input$mu != input$group2.mu,
                                                  power.wlch, "-"),
                                          ifelse(median.group1 != median.group2,
                                                  power.wilcox, "-"))))

  colnames(result.table) <- c("Method", "Coverage CI", "Type I error", "Power")

result.table
}

build_table <- function(input, .group1) {

    median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                            args = list(0.5, mu = input$mu,
                            sigma = input$sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))])

    sample1 <- .group1$all_y_r

      R <- input$R
      plan(user_plan)

      result <- future({

        if (is.null(n.cores)) {
        no_cores <- ifelse(detectCores() > 1, ceiling(detectCores() / 2),
                                              detectCores())
        } else {
        no_cores  <- n.cores
        }
        cl <- makeCluster(no_cores)

        clusterExport(cl, c("sample1", "wilcox.test", "R"),
                        envir = environment())

        wilcox_results <- parLapply(cl, 1:R, function(i) {
          # Perform a one-sample Wilcoxon test without specifying mu
          wilcox.test(sample1[[i]], conf.int = TRUE, exact = TRUE)
        })
        stopCluster(cl)

        wilcox_results
      })

      res <- value(result)
      plan(sequential)

      wilcox_pvalue <- sapply(res, `[[`, "p.value")
      wilcox_ci_both <- t(sapply(res, `[[`, "conf.int"))
      colnames(wilcox_ci_both) <- c("ci_low", "ci_high")

      t_test_wilc <- list(wilcox_ci_both, wilcox_pvalue)
      ci.wilcox <- data.frame(t_test_wilc)[1:2]
      pvalues.wilcox <- data.frame(t_test_wilc)[3]

      ttest.st <- NULL

      tryCatch({
        t_test_results <- mapply(function(y) {
                                t.test(y, mu = input$mu, var.equal = TRUE)
                          }, .group1$all_y_r, SIMPLIFY = FALSE)

        t_test_results = lapply(t_test_results, function(x) {
          if (is.nan(x$conf.int[1])) {
          x$p.value = NA
          }
            x})
        p_values <- sapply(t_test_results, function(x) x$p.value)

        conf_int <- t(sapply(t_test_results, function(x) x$conf.int))

        ttest.st <- as.data.frame(cbind(conf_int, p_values))
        colnames(ttest.st) <- c("CI low", "CI high", "p-value")
        rownames(ttest.st) <- seq(1, nrow(ttest.st), 1)

      }, error = function(e) {
        ttest.st <- as.data.frame(matrix("NA", ncol = 3, nrow = 1))
        colnames(ttest.st) <- c("CI low", "CI high", "p-value")
        rownames(ttest.st) <- "1"
      })

  cover.st     <- round(mean(.group1$coverage_r) * 100, 3)
  cover.wilcox <- round(mean(ci.wilcox[,1] <= median.group1 &
                        ci.wilcox[,2] >= median.group1) * 100, 2)
  power.st     <-  round(mean(ttest.st[3] < .05) * 100, 3)
  # if (is.nan(power.st)) power.st = "NA"
  power.wilcox <-  round(mean(pvalues.wilcox < .05) * 100, 2)
  # if (is.na(cover.wilcox)) power.wilcox = "NA"
  methods <- c("Student's t-test", "Wilcoxon")

  result.table <- as.data.frame(cbind(methods, rbind(cover.st, cover.wilcox),
                              rbind(ifelse(input$mu == 0, power.st, "-"),
                                ifelse(median.group1 == 0, power.wilcox, "-")),
                                    rbind(ifelse(input$mu != 0, power.st, "-"),
                                ifelse(median.group1 != 0, power.wilcox, "-"))))

  colnames(result.table) <- c("Method", "Coverage CI", "Type I error", "Power")

  result.table
}

# p-value -----------------------------------------------------------------
plot_pvalue <- function(.group1, .group2, input) {
  pvalues.st  <-   t(data.frame(mapply(t.test, .group1$all_y_r, .group2$all_y_r,
              var.equal = TRUE)["p.value", ]))
  pvalues.wlch  <- t(data.frame(mapply(t.test, .group1$all_y_r, .group2$all_y_r,
  var.equal = FALSE)["p.value", ]))

  sample1 <- .group1$all_y_r
  sample2 <- .group2$all_y_r
  R <- input$R

  median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                        args = list(0.5, mu = input$mu, sigma =
                        input$sigma)[1:(2 +
                        as.numeric(!is.na(distribution[input$distr,
                        "sigma.value"])))])

  median.group2 <- do.call(paste0("q", distribution[input$distr, "id"]),
                          args = list(0.5, mu = input$group2.mu,
                          sigma = input$group2.sigma)[1:(2 +
                          as.numeric(!is.na(distribution[input$distr,
                          "sigma.value"])))])

  true.median <- median.group1 - median.group2
      plan(user_plan)

  result <-  future({
    if (is.null(n.cores)) {
        no_cores <- ifelse(detectCores() > 1, ceiling(detectCores() / 2),
                                              detectCores())
        } else {
        no_cores  <- n.cores
        }

    cl <- makeCluster(no_cores)

    clusterExport(cl, c("sample1", "sample2", "wilcox.test", "R"),
                  envir = environment())

    wilcox_results <- parLapply(cl, 1:R, function(i) {
      wilcox.test(sample1[[i]], sample2[[i]], exact = FALSE)
    })

    stopCluster(cl)

    wilcox_results
  })

  res <- value(result)
  plan(sequential)

  wilcox.pvalue <- sapply(res, `[[`, "p.value")

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mfrow = c(1, 3), mar = c(1, 3, 2, 2))
  par(oma = c(3,2,3,3))

  maxfrq.pvalue <- hist(pvalues.st, breaks = c(0, .05, seq(.1, 1, .05)),
                        plot = FALSE)
  hist(pvalues.st, prob = TRUE, breaks = c(0, .05, seq(.1, 1, .05)),
            main = "Student t-test", xlab = "",
            col = ifelse(maxfrq.pvalue$breaks < .05, "#F6695D", "grey90"),
            border = "grey50", xaxt = "n", cex.main = 1.5,
            cex.axis = 1.3, las = 1)
  axis(1, at = c(0, seq(.2, 1, .2), 1),
            labels = c(0, seq(.2, 1, .2), 1), cex.axis = 1.3)
  axis(1, at = 0.05, labels = ".05", cex.axis = 1.3)
  abline(h = 1, lwd = 1.7, col = "grey30", lty = 3)

  maxfrq.pvalue <- hist(pvalues.wlch, breaks = c(0, .05, seq(.1, 1, .05)),
  plot = FALSE)
  hist(pvalues.wlch, prob = TRUE, breaks = c(0, .05, seq(.1, 1, .05)),
        main = "Welch t-test", xlab = "",
        col = ifelse(maxfrq.pvalue$breaks < .05, "#F6695D", "grey90"),
        border = "grey50", xaxt = "n", cex.main = 1.5,
        cex.axis = 1.3, las = 1)
  axis(1, at = c(0, seq(.2, 1, .2), 1),
        labels = c(0, seq(.2, 1, .2), 1), cex.axis = 1.3)
  axis(1, at = 0.05, labels = ".05", cex.axis = 1.3)
  abline(h = 1, v = -.3, lwd = 1.7, col = "grey30", lty = 3)

  hist(wilcox.pvalue, prob = TRUE, breaks = c(0, .05, seq(.1, 1, .05)),
        main = "Wilcoxon test", xlab = "",
        col = ifelse(maxfrq.pvalue$breaks < .05, "#F6695D", "grey90"),
        border = "grey50", xaxt = "n", cex.main = 1.5,
        cex.axis = 1.3, las = 1)
  axis(1, at = c(0, seq(.2, 1, .2), 1),
        labels = c(0, seq(.2, 1, .2), 1), cex.axis = 1.3)
  axis(1, at = 0.05, labels = ".05", cex.axis = 1.3)
  abline(h = 1, lwd = 1.7, col = "grey30", lty = 3)
}

plot_onesample_pvalue <- function(.group1, input) {

  pvalues.st <- t(data.frame(mapply(t.test, .group1$all_y_r, mu = input$mu,
                  var.equal = TRUE)["p.value", ]))

  median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                            args = list(0.5, mu = input$mu,
                            sigma = input$sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))])

  wilcox.pvalue <- t(data.frame(mapply(wilcox.test, .group1$all_y_r,
                      exact = FALSE)["p.value", ]))

  par(mfrow = c(1, 3), mar = c(1, 3, 2, 2))
  par(oma = c(3,2,3,3))

  maxfrq.pvalue <- hist(pvalues.st, breaks = c(0, .05, seq(.1, 1, .05)),
                        plot = FALSE)
  hist(pvalues.st, prob = TRUE, breaks = c(0, .05, seq(.1, 1, .05)),
            main = "Student t-test", xlab = "",
            col = ifelse(maxfrq.pvalue$breaks < .05, "#F6695D", "grey90"),
                    border = "grey50", xaxt = "n", cex.main = 1.5,
                    cex.axis = 1.3, las = 1)
  axis(1, at = c(0, seq(.2, 1, .2), 1),
        labels = c(0, seq(.2, 1, .2), 1), cex.axis = 1.3)
  axis(1, at = 0.05, labels = ".05", cex.axis = 1.3)
  abline(h = 1, lwd = 1.7, col = "grey30", lty = 3)

  if (input$distr == "Bernoulli") {
    hist(numeric(0), prob = TRUE, ylim = c(0, 4),
        breaks = c(0, .05, seq(.1, 1, .05)), main = "Wilcoxon test", xlab = "",
        col = ifelse(maxfrq.pvalue$breaks < .05, "#F6695D", "grey90"),
        border = "grey50", xaxt = "n", cex.main = 1.5,  cex.axis = 1.3, las = 1)
    axis(1, at = c(0, seq(.2, 1, .2), 1),
          labels = c(0, seq(.2, 1, .2), 1), cex.axis = 1.3)
    axis(1, at = 0.05, labels = ".05", cex.axis = 1.3)
    abline(h = 1, lwd = 1.7, col = "grey30", lty = 3)
  } else {
    hist(wilcox.pvalue, prob = TRUE, breaks = c(0, .05, seq(.1, 1, .05)),
          main = "Wilcoxon test", xlab = "",
    col = ifelse(maxfrq.pvalue$breaks < .05, "#F6695D", "grey90"),
                border = "grey50", xaxt = "n", cex.main = 1.5,
                cex.axis = 1.3, las = 1)
    axis(1, at = c(0, seq(.2, 1, .2), 1),
          labels = c(0, seq(.2, 1, .2), 1), cex.axis = 1.3)
    axis(1, at = 0.05, labels = ".05", cex.axis = 1.3)
    abline(h = 1, lwd = 1.7, col = "grey30", lty = 3)
  }
}

build_ttable_st <- function(.group1, .group2 = NULL, input) {

  if (is.null(.group2)) {
    t_test_st  <- mapply(t.test, .group1$all_y_r,
                        mu = input$mu, var.equal = TRUE)
  } else {
  t_test_st  <- mapply(t.test, .group1$all_y_r, .group2$all_y_r,
                       var.equal = TRUE)
    }
  p_values <- t_test_st["p.value", ]
  conf_int <- t(data.frame(t_test_st["conf.int", ]))
  param <- t_test_st["parameter", ]

  ttest.st <- as.data.frame(cbind(conf_int, p_values, param))
  colnames(ttest.st) <- c("CI low", "CI high", "p-value", "df")
  rownames(ttest.st) <- seq(1, length(p_values), 1)
  ttest.st$"p-value" <- sapply(ttest.st$"p-value", pval)

  ttest.st[1:5, 1:3]
}

build_ttable_wlch <- function(.group1, .group2 = NULL, input) {
    if (is.null(.group2)) return()
  else t_test_wlch  <- mapply(t.test, .group1$all_y_r,
  .group2$all_y_r, var.equal = FALSE)

  p_values <- t_test_wlch["p.value", ]
  conf_int <- t(data.frame(t_test_wlch["conf.int", ]))
  param    <- t_test_wlch["parameter", ]

  test.wlch <- as.data.frame(cbind(conf_int, p_values, param))
  colnames(test.wlch) <- c("CI low", "CI high", "p-value", "df")
  rownames(test.wlch) <- seq(1, length(p_values), 1)

  test.wlch$"p-value" <-  sapply(test.wlch$"p-value", pval)
  test.wlch[1:5, 1:3]
}

build_ttable_wilc <- function(.group1, .group2 = NULL, input) {
  if (is.null(.group2)) {
    median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                            args = list(0.5, mu = input$mu,
                            sigma = input$sigma)[1:(2 +
                            as.numeric(!is.na(distribution[input$distr,
                            "sigma.value"])))])

    t_test_wilc  <- t(data.frame(purrr::map2(.group1$all_y_r[1:5],
                                              median.group1,
                      function(x, y) {
                  test <- suppressWarnings(wilcox.test(x,
                                                conf.int = TRUE, exact = FALSE))
                                  c(test$conf.int, test$p.value, test$statistic)
                                                      })))
  } else  if (is.null(.group1)) {
  return()
  } else {

      median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                              args = list(0.5, mu = input$mu,
                              sigma = input$sigma)[1:(2 +
                              as.numeric(!is.na(distribution[input$distr,
                              "sigma.value"])))])

      median.group2 <- do.call(paste0("q", distribution[input$distr, "id"]),
                              args = list(0.5, mu = input$group2.mu,
                              sigma = input$group2.sigma)[1:(2 +
                              as.numeric(!is.na(distribution[input$distr,
                              "sigma.value"])))])

      true.median <- median.group1 - median.group2

      t_test_wilc  <- t(data.frame(purrr::map2(.group1$all_y_r[1:5],
                                                .group2$all_y_r[1:5],
                        function(x, y) {
                    test <- wilcox.test(x, y, conf.int = TRUE, exact = FALSE)
                          c(test$conf.int, test$p.value, test$statistic)
                                                })))
}

  t_test_wilc <- data.frame(t_test_wilc)
  if (input$distr == "Bernoulli" & is.null(.group2)) {
    t_test_wilc <- data.frame(
      "CI low" = c(NA,NA,NA,NA,NA),
      "CI high" = c(NA,NA,NA,NA,NA),
      "p-value" = c(NA,NA,NA,NA,NA)
    )
  }

  colnames(t_test_wilc) <- c("CI low", "CI high", "p-value")
  rownames(t_test_wilc) <- seq(1, 5, 1)

  t_test_wilc$"p-value" <- sapply(t_test_wilc$"p-value", pval)
  t_test_wilc[1:5, 1:3]
}
