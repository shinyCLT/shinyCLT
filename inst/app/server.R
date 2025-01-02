server =
  function(input, output, session) {

addResourcePath("www", system.file("app/www", package = "shinyCLT"))
# Check input options #########################################################

    if (mode != "app" && mode != "server") {
      stop("Invalid mode. Mode must be either 'app' or 'server'.")
    }
    if (mode == "app") {
    session$onSessionEnded(function() {
    stopApp()
  })
  }

      if (!is.null(n.cores) && (n.cores <= 0 || is.na(n.cores) ||
      n.cores %% 1 != 0 || n.cores > detectCores())) {
        warning(paste("n.cores must be a positive integer less than or equal",
          "to the number of available cores"))
          stopApp()
      }


          if (!user_plan %in% c("cluster", "multicore", "multisession")) {
          warning(paste("Invalid user_plan: it should be one of 'cluster',",
          "'multicore', or 'multisession'."))
          stopApp()
    }

# Creating input values #######################################################
    v <- reactiveValues(simul = FALSE)

    output$choose_distr <- renderUI({
      selectInput(inputId = "distr","Statistical distribution",
      choices = distribution$fullname)
    })

    output$choose_mu <- renderUI({
      if (is.null(input$distr)) return()
      sliderInput("mu",
                  distribution[input$distr,"mu.name"],
                  min   = distribution[input$distr,"mu.low"],
                  max   = distribution[input$distr,"mu.high"],
                  value = distribution[input$distr,"mu.value"],
                  round = -2)
    })

    output$choose_sigma <- renderUI({
      if (is.null(input$distr)) return()

      if (input$distr != "Bernoulli" & input$distr != "Poisson" &
      input$distr != "Exponential") {
        sliderInput("sigma",
                    distribution[input$distr,"sigma.name"],
                    min   = distribution[input$distr,"sigma.low"],
                    max   = distribution[input$distr,"sigma.high"],
                    value = distribution[input$distr,"sigma.value"],
                    round = -2)
      }
    })

    output$choose_nu <- renderUI({
      if (is.null(input$distr)) return()

      if (input$distr == "Sichel") {
        sliderInput("nu",
                    distribution[input$distr,"nu.name"],
                    min   = distribution[input$distr,"nu.low"],
                    max   = distribution[input$distr,"nu.high"],
                    value = distribution[input$distr,"nu.value"],
                    round = -2)
      }
    })

    output$choose_n <- renderUI({
      sliderInput("n",
                  "Sample size of simulated sample",
                  min   = 5,
                  max   = 1000,
                  value = 50,
                  step  = 5)
    })

    output$choose_R <- renderUI({
      sliderInput("R",
                  "Number of simulated samples",
                  min   = 1000,
                  max   = 10000,
                  value = 1000)
    })

    # group 2 UI ##############################################################
    output$group2_ui <- renderUI({
      checkboxInput("group2_ui",
                    "Add group 2",
                    FALSE)
    })

    output$group2_sliders <- renderUI({
      if (is.null(input$distr)) return()
      tagList(
        sliderInput("group2.mu",
                    distribution[input$distr,"mu.name"],
                    min   = distribution[input$distr,"mu.low"],
                    max   = distribution[input$distr,"mu.high"],
                    value = distribution[input$distr,"mu.value"],
                    round = -2
        ),
        if (input$distr != "Bernoulli" & input$distr != "Poisson" &
        input$distr != "Exponential") {
          sliderInput("group2.sigma",
                      distribution[input$distr,"sigma.name"],
                      min   = distribution[input$distr,"sigma.low"],
                      max   = distribution[input$distr,"sigma.high"],
                      value = distribution[input$distr,"sigma.value"],
                      round = -2)
        },
        if (input$distr == "Sichel") {
          sliderInput("group2.nu",
                      distribution[input$distr,"nu.name"],
                      min   = distribution[input$distr,"nu.low"],
                      max   = distribution[input$distr,"nu.high"],
                      value = distribution[input$distr,"nu.value"],
                      round = -2)
        },
        sliderInput("group2_n",
                    "Sample size of group2 simulated Sample",
                    min   = 5,
                    max   = 1000,
                    value = 50,
                    step  = 5)
      )
    })

# Define tabs for two groups ###################################################
    output$density_tab_title <- renderUI({
      if (input$group2_ui == FALSE) {
        return("Theoretical density")
      } else {
        return("Theoretical densities")
      }
    })

    output$mean_tab_title <- renderUI({
      if (input$group2_ui == FALSE) {
        return("Estimated mean density")
      } else {
        return("Estimated mean densities")
      }
    })

# Text output #################################################################
output$tab1_legend <- renderText({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()

  distr_type <- ifelse(input$distr == "Bernoulli" | input$distr == "Poisson",
                        "density", "function")

markdown(paste0("**Upper plot:** Assumed probability mass ", distr_type,
". The vertical dashed line shows the theoretical mean per group.
  
**Lower plot:** dot plots of the ", input$n,
" observations for the 5 first simulated samples out of ", input$R,
". The black crosses correspond to the estimated mean per sample.
  
**Table:** true mean and median values.
"))
})

output$tab1_legend_group <- renderText({
      if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()
  if (check_input_gr2(input)) return()

      distr_type <- ifelse(input$distr == "Bernoulli" |
      input$distr == "Poisson", "density", "functions per group")

      markdown(paste0("**Upper plot:** Assumed probability mass ", distr_type,
      ". The vertical dashed lines show the theoretical mean per group.
  
**Lower plots:** dot plots of the ", input$n  ,
" observations for the 5 first simulated samples out of ", input$R,
" per group. The triangles and squares correspond to the estimated mean per sample.
  
**Table:** true mean and median values per group and true difference in
  means/medians.
"))
    })

output$tab2_legend <- renderText({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()
  if (input$group2_ui == FALSE) {
    markdown(
      paste0("**Left:** Histogram of the ", input$R," sample means. 
      \nThe solid line shows the estimated mean density obtained when using
        a normal approximation. 
            \nThe vertical dashed line shows the true theoretical mean.
            \nThe coloured dots correspond to the sample means of the 5 first
            simulated samples.
      \n**Right:** Normal Q-Q plot of the ", input$R," sample means.
            The normal approximation is suitable if all points are close to it
              the blue solid line.")
  )
  } else {
    if (check_input_gr1(input)) return()
    if (check_input_gr2(input)) return()
    markdown(
      paste0("**Upper left:** Histogram of the ", input$R," estimated
      differences in means between groups (‘group 1’ minus ‘group 2’).
      The black solid line
    shows the estimated density for the difference in means obtained when using
    a normal approximation. The black vertical dashed line shows the
    true/theoretical difference in means. The coloured crosses correspond to the
    difference in sample means estimated on the 5 first simulated samples. 
    \n**Upper right:** Normal Q-Q plot of the ", input$R," estimated differences
      in means between groups. The normal approximation is suitable if all
      points are close to it the black solid line.
    \n**Lower left:** Histograms of the ", input$R," sample means per group.
    The coloured solid lines show the estimated mean density per group obtained
    when using a normal approximation. The coloured vertical dashed lines show
    the true/theoretical means  per group. The coloured triangles and squares
    show the sample means of the 5 first simulated samples for each group. 
    \n**Right:** Normal Q-Q plot of the ", input$R," sample means per group.
    The normal approximation is suitable if all points of a (coloured) group
    are close to it the corresponding coloured dashed line."))
  }
  })


output$tab3_legend <- renderText({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()
    markdown(
      paste0("\n**Table:** Column ‘Coverage CIs’: Estimated probability of 95%
              confidence intervals (CI) for the location parameter for the two
              methods of interest:  
              \n*[i] the Student’s CIs*, requiring the normality approximation
              to be suitable,  
              \n*[ii] the Mann-Whitney-Wilcoxon CIs*.  
              For [i] the target location parameter is the mean and for [ii] the
              target location parameter is the median.  
             \nColumn **‘Type I error’**: estimated probability of rejecting the
              null hypothesis and concluding that the location parameter is
              different from zero when it is not.
             \nColumn **‘Power’**: estimated probability of rejecting the null
              hypothesis and concluding that the location parameter is different
              from zero when it is.
              \n**Plot:** 95% Student’s confidence intervals for the mean for
              the 100 first simulated samples (out of  ", input$R,").   
             The dashed blue vertical line denotes the **true mean**.
              \n**Confidence intervals** appear in grey if they contain the true
              mean and in red otherwise.
              \nDegenerate confidence intervals (ie, same upper and lower
                values) are displayed with a dot.  
             **The percentage of intervals** (out of ", input$R,") containing
              the true mean is indicated. This percentage is close to 95% when
              the normal approximation is suitable."))
})

output$tab3_legend_group <- renderText({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()
  if (check_input_gr2(input)) return()
    markdown(
      paste0("**Table**: Column ‘Coverage CIs’: Estimated probability of 95%
        confidence intervals (CI) for the difference in location parameters 
        between groups of containing the true value for three methods of 
        interest:  
             *[i] the Student’s CIs*, requiring the normality approximation 
            to be suitable as well as homoscedasticity,  
             *[ii] the Welch’s CIs*, requiring the normality approximation 
            to be suitable,  
             *[iii] the Mann-Whitney-Wilcoxon CIs*, requiring both groups to be
              independently and identically distributed with different location
              parameter but with same nuisance parameters
                (homoscedasticity/homomorphism). 
            For [i] and [ii], the target location parameter is the difference
              in means. For [iii], the target location parameter is the
              difference in medians.  
             Column **‘Type I error’** (only available when the location
              parameters of both groups are equal): estimated probability of
              rejecting the null hypothesis and concluding that groups have
                different location parameters when they don’t.  
             Column **‘Power’** (only available when the location parameters of
              both groups are different): estimated probability of rejecting the
              null hypothesis and concluding that groups have different
                location parameters when they do.  
              \n**Plot:** 95% confidence intervals for the differences in
              location parameters between groups (‘group 1’ minus ‘group 2’)
                for the 100 first simulated samples (out of ", input$R,")
                for the method of interest.  
            The dashed blue vertical line denotes the true difference in mean.  
             **Confidence intervals** appear in grey if they contain the true
              mean and in red otherwise.  
             The **percentage of intervals** (out of ", input$R,") containing
              the true mean is indicated. This percentage is close to 95% when
              the normal approximation is suitable."))
})

output$tab4_legend <- renderText({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()
  markdown(
    paste0("**Plots:** Histogramm of the ", input$R, " p-values per statistical
    test. The dashed horizontal line corresponds to the uniform distribution, 
          i.e. the theoretical distribution of p-values under the null
          hypothesis.  
           **Tables:** 95% Student’s confidence intervals for the location
            parameter for the 5 first simulated samples 
          (out of ", input$R,") and p-values of the corresponding test of
          location parameter."))
})

output$tab4_legend_group <- renderText({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()
  if (check_input_gr2(input)) return()
  markdown(
    paste0("**Plot:** histograms of the  ", input$R," p-values of the tests of
      equality of the location parameters performed on each simulated sample.  
           When the **null hypothesis** is true (equality of the location
            parameters of both groups), the p-values should follow a uniform
            distribution (and the histogram should be close to the dashed
              straight line).  
           When the **alternative hypothesis** is true (difference in the
            location parameters of both groups), p-values should be
            right-skewed.  
            The first histogram bar, displayed in red, corresponds to p-values
            smaller than 0.05, the assumed **type I error**."))
})

# Calculations #################################################################
    observeEvent(input$sidebar, {
      v$simul <- FALSE
    })

    observeEvent(input$go, {
      v$simul <- input$go
    })

    simulationResults <- reactive({
      group1 <- calculate_statistics(input, distribution)
    })

    group2_SimulationResults <- reactive({
      if (input$group2_ui) {
        req(input$group2.mu)
        group2 <- calculate_statistics_group2(input, distribution)
      }
    })

    # Density plot #############################################################
    output$mean_table <- renderTable({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()

      median.group1 <- do.call(paste0("q", distribution[input$distr, "id"]),
                              args = list(0.5, mu = input$mu,
                              sigma = input$sigma)[1:(2 +
                              as.numeric(!is.na(distribution[input$distr,
                              "sigma.value"])))])

      df.table <- data.frame(c("Mean", "Median"),
                c(input$mu, ifelse(median.group1 <= 0, median.group1,
                pval(median.group1))))
      names(df.table) <- c("Parameter", "Value")
      if (input$distr == "Bernoulli" & median.group1 <= 0) df.table[2,2] <- 0
      df.table
    })

    output$mean_table_group <- renderTable({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()
      if (check_input_gr2(input)) return()

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

      df.table <- data.frame(c("Mean", "Median"),
                              c(input$mu, median.group1),
                              c(input$group2.mu, median.group2),
                              c(input$mu - input$group2.mu,
                              median.group1 - median.group2))
      names(df.table) <- c("Statistic","Group 1", "Group 2", "Difference")
      df.table
    })

    output$density <- renderPlotly({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()

      group1 <- simulationResults()

      if (input$distr == "Uniform") {

        generate_uniform_plot(input, distribution, group1)
      } else {
      generate_distribution_plot(input, distribution, group1)
      }
    })

    output$density_2groups <- renderPlotly({
      if (v$simul == FALSE) return()
      if (check_input_gr2(input)) return()

      group1 <- simulationResults()
      group2 <- group2_SimulationResults()

      if (input$distr == "Uniform") {
        generate_compare_uniform_plot(input, distribution, group1, group2)

      } else {
        generate_compare_distributions_plot(input, distribution, group1, group2)
      }
    })

    output$samples_plot <- renderPlotly({
      if (v$simul == FALSE) return()

      if (check_input_gr1(input)) return()

      group1 <- simulationResults()

      generate_5samples_plot(input, distribution, group1)
    })

    output$group1_samples_plot <- renderPlotly({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()
      if (check_input_gr2(input)) return()

      group1 <- simulationResults()
      group2 <- group2_SimulationResults()

      group1_5samples_plot(input, distribution, group1, group2)
    })

    output$group2_samples_plot <- renderPlotly({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()
      if (check_input_gr2(input)) return()

      group1 <- simulationResults()
      group2 <- group2_SimulationResults()
      group2_5samples_plot(input, distribution, group1, group2)
    })

    # Estimated mean density tab ###############################################

    output$mean <- renderPlot({
      if (v$simul == FALSE) return()
      
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))


      if (check_input_gr1(input)) return()
      group1 <- simulationResults()
      plot_density_qq(group1, input)

      if (input$group2_ui) {

        if (check_input_gr2(input)) return()

        group2 <- group2_SimulationResults()

        plot_compare_density_qq(group1, group2, input)
      }
    })

    output$diffmean <- renderPlot({
      if (v$simul == FALSE) return()
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))

      if (input$group2_ui) {

        if (check_input_gr1(input)) return()
        group1 <- simulationResults()
        if (check_input_gr2(input)) return()
        group2 <- group2_SimulationResults()

        plot_compare_means_qq(group1, group2, input)
      }
    })

# Coverage tab #################################################################
    output$ci_summary <- renderTable({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()
      if (check_input_gr2(input)) return()

      group1 <- simulationResults()
      group2 <- group2_SimulationResults()

      ttests_temp <- calculate_wilcoxon(group1, group2, input, distribution)
      m$set("ttests_list", ttests_temp)
      build_table_compare(input, m$get("ttests_list"))

    }, sanitize.text.function = identity, align = "lccc")

    output$onesample_CI_table <- renderTable({
      if (v$simul == FALSE) return()

      if (is.null(input$mu)) return()
      if (check_input_gr1(input)) return()

      group1 <- simulationResults()
      build_table(input, group1)
    })

    output$coverage <- renderPlotly({
      if (v$simul == FALSE) return()

      if (is.null(input$mu)) return()
      group1 <- simulationResults()
      plot_CI(group1, input)
    })

    output$ci_compare <- renderPlotly({
      if (v$simul == FALSE) return()

      if (check_input_gr1(input)) return()
      if (check_input_gr2(input)) return()
      group1 <- simulationResults()
      group2 <- group2_SimulationResults()

  plot_CI_group2(group1, group2, input, m$get("ttests_list"))
    })

    output$choose_ttest <- renderUI({

      selectInput(inputId = "ttest",
                    "Choose the confidence interval to be displayed",
                  choices = c("Student's", "Welch", "Wilcoxon"),
                  selected = "Student's")
    })

  # P-value tab ################################################################
    output$pvalue <- renderPlot({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()
      
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))

      group1 <- simulationResults()
      if (input$group2_ui) {
        if (check_input_gr2(input)) return()
        group2 <- group2_SimulationResults()
        if (is.null(group1) | is.null(group2)) return()
        plot_pvalue(group1, group2, input)
      } else {
      plot_onesample_pvalue(group1, input)
      }
    })

    output$ttest_text <- renderText({
      if (v$simul == FALSE) return()
      "Summary of the first 5 tests results"
    })

    output$ttest_header <- renderText({
      if (v$simul == FALSE) return()
      "Histogram of p-values "
    })

    output$ttest.std <- renderTable({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()

      group1 <- simulationResults()
      if (input$group2_ui) {
        if (check_input_gr2(input)) return()
        group2 <- group2_SimulationResults()
        if (is.null(group1) | is.null(group2)) return()
        build_ttable_st(group1, group2, input)
      } else {
      build_ttable_st(group1, input = input)
      }
    },
    spacing = "xs",
    width = "auto",
    align = "c",
    striped = TRUE,
    hover = TRUE,
    rownames = FALSE)

    output$ttest.wlch <- renderTable({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()
      group1 <- simulationResults()

      if (input$group2_ui) {
        group2 <- group2_SimulationResults()
        if (is.null(group1) | is.null(group2)) return()
        build_ttable_wlch(group1, group2, input)
      } else {
      build_ttable_wlch(group1, input = input)
      }
    },
    spacing = "xs",
    width = "auto",
    align = "c",
    striped = TRUE,
    hover = TRUE,
    rownames = FALSE)

    output$ttest.wilc <- renderTable({
      if (v$simul == FALSE) return()
      if (check_input_gr1(input)) return()

      group1 <- simulationResults()

      if (input$group2_ui) {
        group2 <- group2_SimulationResults()
        if (is.null(group1) | is.null(group2)) return()
        build_ttable_wilc(group1, group2, input)
      } else {
      build_ttable_wilc(group1, input = input)
      }
    },
    spacing = "xs",
    width = "auto",
    align = "c",
    striped = TRUE,
    hover = TRUE,
    rownames = FALSE)

output$ttest.std_onesample <- renderTable({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()

  group1 <- simulationResults()

  build_ttable_st(group1, input = input)
},
spacing = "xs",
width = "auto",
align = "c",
striped = TRUE,
hover = TRUE,
rownames = FALSE)

output$ttest.wilc_onesample <- renderTable({
  if (v$simul == FALSE) return()
  if (check_input_gr1(input)) return()

  group1 <- simulationResults()

  build_ttable_wilc(group1, input = input)

},
spacing = "xs",
width = "auto",
align = "c",
striped = TRUE,
hover = TRUE,
rownames = FALSE)
}
