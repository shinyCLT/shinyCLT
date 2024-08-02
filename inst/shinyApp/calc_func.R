calculate_statistics <- function(input, distribution) {

    if (check_input_gr1(input)) return()
  
    

  if(distribution[input$distr, "id"] == "UNIF") {
    y_r  <- lapply(1:input$R, function(x) generate_uniform_sample(n = input$n, mean = input$mu, sigma = input$sigma))

    # Calculation -------------------------------------------------------------
    
    mean_r  <- unlist(lapply(y_r, mean))
    var_r   <- unlist(lapply(y_r, var))
    all_y_r <- y_r
    y_r     <- y_r[1:5]
    
    isin <- function(ci, theta) {
      if (!any(is.nan(ci))) {
        if (theta >= ci[1] & theta <= ci[2]) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(NA)
      }
    }
    
    ci_r <- t(apply(cbind(mean_r, sqrt(var_r/input$n)), 1, function(x, mu) {
      x[1] + qt(0.975, input$n - 1) * x[2] * c(-1, 1)
    }
    ))
    
    coverage_r <- apply(ci_r, 1, isin, theta = input$mu)
    
    data <- list(y_r = y_r, mean_r = mean_r, var_r = var_r, ci_r = ci_r, coverage_r = coverage_r, all_y_r = all_y_r)
    
  }
  else {
  
  y_r <- lapply(as.list(rep(NA, input$R)), function(x, par) {
    do.call(paste0("r", distribution[input$distr, "id"]),
            list(n = par$n, mu = par$mu, sigma = par$sigma)[1:(2 + as.numeric(!is.na(distribution[input$distr, "sigma.value"])))]
    )
  }, par = input)
  
  mean_r  <- unlist(lapply(y_r, mean))
  var_r   <- unlist(lapply(y_r, var))
  all_y_r <- y_r
  y_r     <- y_r[1:5]
  

  
  # ci_r <- t(apply(cbind(mean_r, sqrt(var_r/input$n)), 1, function(x, mu) {
  #   x[1] + qt(0.975, input$n - 1) * x[2] * c(-1, 1)
  # }
  # ))
  
  t_test_results <- mapply(function(y) t.test(y, mu = input$mu, var.equal = TRUE), all_y_r, SIMPLIFY = FALSE)
  ci_r  <- t(sapply(t_test_results, function(x) x$conf.int))

  coverage_r <- apply(ci_r, 1, isin, theta = input$mu)
  
  data <- list(y_r = y_r, mean_r = mean_r, var_r = var_r, ci_r = ci_r, coverage_r = coverage_r, all_y_r = all_y_r)

  }
}


calculate_statistics_control <- function(input, distribution) {
  if (check_input_gr2(input)) return()
  
  if(distribution[input$distr, "id"] == "UNIF") {
    
    y_r  <- lapply(1:input$R, function(x) generate_uniform_sample(n = input$control_n, mean = input$group2.mu, sigma = input$group2.sigma))
    
# Calculation -------------------------------------------------------------
    
    mean_r <- unlist(lapply(y_r, mean))
    var_r  <- unlist(lapply(y_r, var))
    all_y_r <- y_r
    y_r    <- y_r[1:5]
    
    isin <- function(ci, theta) {
      if (!any(is.na(ci))) {
        if (theta >= ci[1] & theta <= ci[2]) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(NA)
      }
    }
    
    ci_r <- t(apply(cbind(mean_r, sqrt(var_r/input$control_n)), 1, function(x, mu) {
      x[1] + qt(0.975, input$control_n - 1) * x[2] * c(-1, 1)
    }
    ))
    
    coverage_r <- apply(ci_r, 1, isin, theta = input$group2.mu)
    
    control_data <- list(y_r = y_r, mean_r = mean_r, var_r = var_r, ci_r = ci_r, coverage_r = coverage_r, all_y_r = all_y_r)
  }
  else {
  y_r <- lapply(as.list(rep(NA, input$R)), function(x, par) {
    do.call(paste0("r", distribution[input$distr, "id"]),
            list(n = par$control_n, mu = par$group2.mu, sigma = par$group2.sigma)[1:(2 + as.numeric(!is.na(distribution[input$distr, "sigma.value"])))]
    )
  }, 
  par = input)
  
  mean_r <- unlist(lapply(y_r, mean))
  var_r  <- unlist(lapply(y_r, var))
  all_y_r <- y_r
  y_r    <- y_r[1:5]
  
  isin <- function(ci, theta) {
    if (!any(is.na(ci))) {
      if (theta >= ci[1] & theta <= ci[2]) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(NA)
    }
  }
  
  # ci_r <- t(apply(cbind(mean_r, sqrt(var_r/input$control_n)), 1, function(x, mu) {
  #   x[1] + qt(0.975, input$control_n - 1) * x[2] * c(-1, 1)
  # }
  # ))
  
  t_test_results <- mapply(function(y) t.test(y, mu = input$mu, var.equal = TRUE), all_y_r, SIMPLIFY = FALSE)
  ci_r  <- t(sapply(t_test_results, function(x) x$conf.int))
  
  coverage_r <- apply(ci_r, 1, isin, theta = input$group2.mu)
  
  control_data <- list(y_r = y_r, mean_r = mean_r, var_r = var_r, ci_r = ci_r, coverage_r = coverage_r, all_y_r = all_y_r)
  
  return(control_data)
  }
}




calculate_wilcoxon <- function(.group1, .group2, input, distribution) {
  
      sample1 <- .group1$all_y_r
      sample2 <- .group2$all_y_r
    
      
      if (is.null(sample1) | is.null(sample2)) return()
      else {
    
      R <- input$R
 
      
     result <-  future({

        no_cores <- ifelse(detectCores() > 1, detectCores() - 1, detectCores())
        cl <- makeCluster(no_cores)
        
        
        
        clusterExport(cl, c("sample1", "sample2", "wilcox.test", "R"), envir = environment())
        
        wilcox_results <- parLapply(cl, 1:R, function(i) {
          wilcox.test(sample1[[i]], sample2[[i]], conf.int = TRUE, exact = FALSE)
        })
        
   
        stopCluster(cl)
       
        wilcox_results
        
      }) 
     

       
        res <- value(result)
        
        wilcox_pvalue <- sapply(res, `[[`, "p.value")
        wilcox_ci_both <- t(sapply(res, `[[`, "conf.int"))
        colnames(wilcox_ci_both) <- c("ci_low", "ci_high")
        
        wilcox_r <- list(wilcox_ci_both, wilcox_pvalue)
# Diff in means removed from mu param, cause it make no sense
        student_ci_both <- t(data.frame(mapply(t.test, .group1$all_y_r, .group2$all_y_r, var.equal = TRUE)["conf.int", ]))
        student_pvalue  <- t(data.frame(mapply(t.test, .group1$all_y_r, .group2$all_y_r, var.equal = TRUE)["p.value", ]))
        
        welch_ci_both <- t(data.frame(mapply(t.test, .group1$all_y_r, .group2$all_y_r, var.equal = FALSE)["conf.int", ]))
        welch_pvalue  <- t(data.frame(mapply(t.test, .group1$all_y_r, .group2$all_y_r, var.equal = FALSE)["p.value", ]))
        
        
        my_list <- list(
          student = list(student_ci_both, pvalue = student_pvalue),
          welch   = list(welch_ci_both, pvalue = welch_pvalue),
          wilcox  = wilcox_r)

      }
      
}

