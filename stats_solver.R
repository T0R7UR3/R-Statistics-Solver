# Program: Menu Driven Statistics Solver
# Author: Murdock MacAskill (Zenith Edition - Final Submission)
# Date: 03/25/2026

# ---------------------------
# HELPER FUNCTIONS: INPUTS
# ---------------------------

get_choice <- function(prompt_text, valid_options) {
  repeat {
    input <- trimws(tolower(readline(prompt_text)))
    if (input %in% valid_options) return(input)
    cat(sprintf("Invalid choice. Please enter one of: %s\n", paste(valid_options, collapse = ", ")))
  }
}

get_yes_no <- function(prompt_text) { return(get_choice(prompt_text, c("yes", "no"))) }

get_number <- function(prompt_text) {
  repeat {
    num <- suppressWarnings(as.numeric(readline(prompt_text)))
    if (!is.na(num)) return(num)
    cat("Invalid input. Please enter a valid number.\n")
  }
}

get_positive_number <- function(prompt_text) {
  repeat {
    num <- get_number(prompt_text)
    if (num > 0) return(num)
    cat("Value must be greater than 0.\n")
  }
}

get_count <- function(prompt_text, min_val = 0) {
  repeat {
    num <- get_number(prompt_text)
    if (num == floor(num) && num >= min_val) return(num)
    cat(sprintf("Value must be a whole number >= %d.\n", min_val))
  }
}

get_prob <- function(prompt_text) {
  repeat {
    num <- get_number(prompt_text)
    if (num >= 0 && num <= 1) return(num)
    cat("Value must be a decimal between 0 and 1 (inclusive).\n")
  }
}

get_strict_prob <- function(prompt_text) {
  repeat {
    num <- get_number(prompt_text)
    if (num > 0 && num < 1) return(num)
    cat("Value must be a decimal strictly between 0 and 1 (e.g., 0.95).\n")
  }
}

get_vector <- function(prompt_text) {
  repeat {
    input <- readline(paste0(prompt_text, " (comma-separated): "))
    vec <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input, ",")))))
    if (!any(is.na(vec)) && length(vec) > 0) return(vec)
    cat("Invalid input. Enter numbers separated by commas.\n")
  }
}

get_count_vector <- function(prompt_text) {
  repeat {
    vec <- get_vector(prompt_text)
    if (all(vec >= 0) && all(vec == floor(vec)) && sum(vec) > 0) return(vec)
    cat("Invalid input. Values must be non-negative whole numbers (total > 0).\n")
  }
}

# ---------------------------
# MATH ENGINE: HELPERS
# ---------------------------

print_mean_results <- function(stat, df = NULL, alt, conf_level, xb, se, mu0) {
  alpha <- 1 - conf_level
  conf_pct <- conf_level * 100
  
  if (is.null(df)) {
    p_val <- if (alt == "two.sided") 2 * pnorm(-abs(stat)) else if (alt == "less") pnorm(stat) else pnorm(stat, lower.tail = FALSE)
    crit <- if (alt == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
    crit_name <- "Critical Value (Z)"
  } else {
    p_val <- if (alt == "two.sided") 2 * pt(-abs(stat), df) else if (alt == "less") pt(stat, df) else pt(stat, df, lower.tail = FALSE)
    crit <- if (alt == "two.sided") qt(1 - alpha/2, df) else qt(1 - alpha, df)
    crit_name <- "Critical Value (T)"
  }
  
  margin <- crit * se
  
  if (alt == "two.sided") {
    ci <- c(xb - margin, xb + margin)
    bound_label <- sprintf("%g%% Confidence Interval", conf_pct)
  } else if (alt == "less") {
    ci <- c(-Inf, xb + margin)
    bound_label <- sprintf("%g%% Confidence Upper Bound", conf_pct)
  } else {
    ci <- c(xb - margin, Inf)
    bound_label <- sprintf("%g%% Confidence Lower Bound", conf_pct)
  }
  
  cat("\n--- DETAILED TEST RESULTS ---\n")
  cat(sprintf("Null Value (mu0): %.4f\n", mu0))
  cat(sprintf("Point Estimate (x_bar / diff): %.4f\n", xb))
  cat(sprintf("Standard Error (SE): %.4f\n", se))
  cat(sprintf("%s: %.4f\n", crit_name, crit))
  cat(sprintf("Margin of Error (ME): %.4f\n", margin))
  cat(sprintf("Test Statistic: %.4f\n", stat))
  if (!is.null(df)) cat(sprintf("Degrees of Freedom (DF): %.4f\n", df))
  cat(sprintf("P-Value: %.4e\n", p_val))
  cat(sprintf("%s: [%.4f, %.4f]\n", bound_label, ci[1], ci[2]))
  cat("-----------------------------\n")
}

# ---------------------------
# MAIN APPLICATION LOOP
# ---------------------------

while (TRUE) {
  cat("\n========================================\n")
  cat("      STATISTICS SOLVER MENU SYSTEM      \n")
  cat("========================================\n")
  cat("  'p'     : Proportions (1 or 2 Samples)\n")
  cat("  'mu'    : Means (Z, T, Paired, Independent)\n")
  cat("  'anova' : One-Way ANOVA (Compare 2+ groups)\n")
  cat("  'chisq' : Chi-Square (Goodness of Fit or Independence)\n")
  cat("  'n'     : Find Required Sample Size\n")
  cat("  'q'     : Quit Program\n")
  cat("----------------------------------------\n")
  
  type <- get_choice("Select an option ('p', 'mu', 'anova', 'chisq', 'n', 'q'): ", c("p", "mu", "anova", "chisq", "n", "q"))
  if (type == "q") { cat("\nExiting. Goodbye!\n"); break }

  cat("\n--- DATA ENTRY ---\n")

  if (type == "n") {
    cat("\n  'p'  : Sample Size for a Proportion\n")
    cat("  'mu' : Sample Size for a Mean\n")
    sub <- get_choice("Calculate for a proportion ('p') or a mean ('mu')? ", c("p", "mu"))
    conf <- get_strict_prob("Desired Confidence Level (e.g., 0.95): ")
    E <- get_positive_number("Acceptable Margin of Error (E): ")
    z <- qnorm(1 - (1 - conf)/2)
    
    if (sub == "p") {
      p_hat <- get_strict_prob("Estimated past proportion (use 0.5 for safest estimate): ")
      n_req <- ceiling((p_hat * (1 - p_hat)) * (z/E)^2)
    } else {
      sigma <- get_positive_number("Estimated Population Std Dev (Sigma): ")
      n_req <- ceiling(((z * sigma)/E)^2)
    }
    cat("\nRequired Sample Size (n):", n_req, "\n"); next
  }

  if (type == "anova") {
    k <- get_count("Number of groups/treatments to compare: ", 2); data_list <- list(); labels <- c()
    for (i in 1:k) {
      repeat {
        g_data <- get_vector(sprintf("Enter data values for Group %d", i))
        if (length(g_data) >= 2) break
        cat("Error: Min 2 values per group.\n")
      }
      data_list[[i]] <- g_data
      labels <- c(labels, rep(paste0("G", i), length(g_data)))
    }
    cat("\n--- ONE-WAY ANOVA RESULTS ---\n")
    tryCatch(print(summary(aov(unlist(data_list) ~ factor(labels)))), error = function(e) cat("Error:", e$message, "\n"))
    next
  }

  if (type == "chisq") {
    cat("\n  'g' : Goodness of Fit (1 row of data against expected probabilities)\n")
    cat("  't' : Test for Independence (2-way table / matrix)\n")
    mode <- get_choice("Select Chi-Square test ('g' or 't'): ", c("g", "t"))
    
    if (mode == "g") {
      obs <- get_count_vector("Enter the list of observed counts")
      if (get_yes_no("Are all categories expected to be equal? (yes/no): ") == "yes") {
        tryCatch(print(chisq.test(obs)), error = function(e) cat("Error:", e$message, "\n"))
      } else {
        repeat {
          probs <- get_vector("Enter the expected probabilities for each category")
          if (length(probs) == length(obs) && abs(sum(probs) - 1) <= 0.001 && all(probs > 0)) break
          cat("Error: Must be positive, match length of counts, and sum approximately to 1.\n")
        }
        tryCatch(print(chisq.test(obs, p = probs)), error = function(e) cat("Error:", e$message, "\n"))
      }
    } else {
      cat("[!] Note: Do NOT include 'Total' columns or rows in your dimensions.\n")
      nr <- get_count("Number of Rows: ", 2); nc <- get_count("Number of Columns: ", 2); vals <- c()
      for (i in 1:nr) {
        repeat {
          row <- get_count_vector(sprintf("Enter the %d counts for Row %d", nc, i))
          if (length(row) == nc) { vals <- c(vals, row); break }
          cat(sprintf("Error: Expected exactly %d values.\n", nc))
        }
      }
      tryCatch(print(chisq.test(matrix(vals, nrow = nr, byrow = TRUE), correct = FALSE)), error = function(e) cat("Error:", e$message, "\n"))
    }
    next
  }

  samples <- as.numeric(get_choice("Number of samples (1 or 2): ", c("1", "2")))
  
  cat("\n  'two'   : Two-sided test (Not equal to)\n")
  cat("  'left'  : Left-tailed test (Less than)\n")
  cat("  'right' : Right-tailed test (Greater than)\n")
  test_tail <- get_choice("Select tail type ('two', 'left', 'right'): ", c("two", "left", "right"))
  
  conf <- get_strict_prob("Confidence level (e.g., 0.95): ")
  alt <- c("two" = "two.sided", "left" = "less", "right" = "greater")[test_tail]
  alpha <- 1 - conf
  
  if (type == "p") {
    tryCatch({
      if (samples == 1) {
        p0 <- get_strict_prob("Baseline/Null Proportion (p0, e.g., 0.5): ")
        repeat { 
          x1 <- get_count("Number of successes (x): ")
          n1 <- get_count("Total sample size (n): ", 1)
          if (x1 <= n1) break
          cat("Error: Number of successes (x) cannot be greater than sample size (n).\n") 
        }
        
        p_hat <- x1 / n1
        se_ci <- sqrt(p_hat * (1 - p_hat) / n1)
        se_test <- sqrt(p0 * (1 - p0) / n1)
        z_stat <- (p_hat - p0) / se_test
        crit <- if (alt == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
        margin <- crit * se_ci
        
        cat("\n--- DETAILED METRICS (Wald Z-Approximation) ---\n")
        cat(sprintf("Null Value (p0): %.4f\n", p0))
        cat(sprintf("Point Estimate (p_hat): %.4f\n", p_hat))
        cat(sprintf("Standard Error (SE for CI): %.4f\n", se_ci))
        cat(sprintf("Critical Value (Z): %.4f\n", crit))
        cat(sprintf("Margin of Error (ME): %.4f\n", margin))
        cat(sprintf("Test Statistic (Z): %.4f\n", z_stat))
        cat("-----------------------------------------------\n")
        cat("[i] Note: The final output below uses Pearson's Chi-Square method (prop.test).\n")
        cat("    Intervals and P-Values may differ slightly from the Wald approximation above.\n\n")
        
        print(prop.test(x1, n1, p = p0, alternative = alt, conf.level = conf, correct = FALSE))
      } else {
        repeat { 
          x1 <- get_count("Number of successes in Sample 1 (x1): ")
          n1 <- get_count("Total size of Sample 1 (n1): ", 1)
          x2 <- get_count("Number of successes in Sample 2 (x2): ")
          n2 <- get_count("Total size of Sample 2 (n2): ", 1)
          if (x1 <= n1 && x2 <= n2) break
          cat("Error: Number of successes cannot exceed sample size for either group.\n")
        }
        
        p_hat1 <- x1 / n1
        p_hat2 <- x2 / n2
        p_diff <- p_hat1 - p_hat2
        p_pool <- (x1 + x2) / (n1 + n2)
        
        se_ci <- sqrt((p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2))
        se_test <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
        z_stat <- p_diff / se_test
        crit <- if (alt == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
        margin <- crit * se_ci
        
        cat("\n--- DETAILED METRICS (Wald Z-Approximation) ---\n")
        cat(sprintf("Null Value (Expected Diff): 0.0000\n"))
        cat(sprintf("Point Estimate (p_hat1 - p_hat2): %.4f\n", p_diff))
        cat(sprintf("Standard Error (SE for CI): %.4f\n", se_ci))
        cat(sprintf("Critical Value (Z): %.4f\n", crit))
        cat(sprintf("Margin of Error (ME): %.4f\n", margin))
        cat(sprintf("Test Statistic (Z): %.4f\n", z_stat))
        cat("-----------------------------------------------\n")
        cat("[i] Note: The final output below uses Pearson's Chi-Square method (prop.test).\n")
        cat("    Intervals and P-Values may differ slightly from the Wald approximation above.\n\n")
        
        print(prop.test(c(x1, x2), c(n1, n2), alternative = alt, conf.level = conf, correct = FALSE))
      }
    }, error = function(e) cat("Error:", e$message, "\n"))
    
  } else if (type == "mu") {
    tryCatch({
      mu0 <- get_number("Null hypothesis mean or expected difference (mu0): ")
      if (samples == 1) {
        xb <- get_number("Sample Mean (x_bar): ")
        n <- get_count("Total sample size (n): ", 2)
        if (get_yes_no("Is the true population standard deviation known? (yes/no): ") == "yes") {
          sig <- get_positive_number("Population Standard Deviation (Sigma): ")
          print_mean_results((xb-mu0)/(sig/sqrt(n)), NULL, alt, conf, xb, sig/sqrt(n), mu0)
        } else {
          s <- get_positive_number("Sample Standard Deviation (s): ")
          print_mean_results((xb-mu0)/(s/sqrt(n)), n-1, alt, conf, xb, s/sqrt(n), mu0)
        }
      } else {
        if (get_yes_no("Is the data paired/dependent? (yes/no): ") == "yes") {
          md <- get_number("Mean of the differences (x_bar_diff): ")
          sd <- get_positive_number("Standard deviation of the differences (s_diff): ")
          np <- get_count("Total number of pairs (n): ", 2)
          print_mean_results((md-mu0)/(sd/sqrt(np)), np-1, alt, conf, md, sd/sqrt(np), mu0)
        } else {
          xb1 <- get_number("Mean of Sample 1 (x_bar1): ")
          s1 <- get_positive_number("Standard Deviation of Sample 1 (s1): ")
          n1 <- get_count("Total size of Sample 1 (n1): ", 2)
          xb2 <- get_number("Mean of Sample 2 (x_bar2): ")
          s2 <- get_positive_number("Standard Deviation of Sample 2 (s2): ")
          n2 <- get_count("Total size of Sample 2 (n2): ", 2)
          if (get_yes_no("Assume equal variances? (yes/no): ") == "yes") {
            df <- n1+n2-2; sp <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/df); se <- sp*sqrt(1/n1+1/n2)
          } else {
            se <- sqrt(s1^2/n1+s2^2/n2); df <- (s1^2/n1+s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
          }
          print_mean_results((xb1-xb2-mu0)/se, df, alt, conf, xb1-xb2, se, mu0)
        }
      }
    }, error = function(e) cat("Error:", e$message, "\n"))
  }
}
