# Program: Menu Driven Statistics Solver
# Author: Murdock MacAskill (Galactic Edition - Final Polish)
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
    if (num > 0 && num < 1) return(num)
    cat("Value must be a decimal between 0 and 1.\n")
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

print_mean_results <- function(stat, df = NULL, alt, conf_level, xb, se) {
  alpha <- 1 - conf_level
  conf_pct <- conf_level * 100
  
  if (is.null(df)) {
    p_val <- if (alt == "two.sided") 2 * pnorm(-abs(stat)) else if (alt == "less") pnorm(stat) else pnorm(stat, lower.tail = FALSE)
    crit <- if (alt == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
  } else {
    p_val <- if (alt == "two.sided") 2 * pt(-abs(stat), df) else if (alt == "less") pt(stat, df) else pt(stat, df, lower.tail = FALSE)
    crit <- if (alt == "two.sided") qt(1 - alpha/2, df) else qt(1 - alpha, df)
  }
  
  if (alt == "two.sided") {
    ci <- c(xb - crit * se, xb + crit * se)
    bound_label <- sprintf("%g%% Confidence Interval", conf_pct)
  } else if (alt == "less") {
    ci <- c(-Inf, xb + crit * se)
    bound_label <- sprintf("%g%% Confidence Upper Bound", conf_pct)
  } else {
    ci <- c(xb - crit * se, Inf)
    bound_label <- sprintf("%g%% Confidence Lower Bound", conf_pct)
  }
  
  cat(sprintf("\nStatistic: %.4f\nP-Value: %.4e\n%s: [%.4f, %.4f]\n", stat, p_val, bound_label, ci[1], ci[2]))
}

# ---------------------------
# MAIN APPLICATION LOOP
# ---------------------------

while (TRUE) {
  cat("\n========================================\n")
  cat("      STATISTICS SOLVER MENU SYSTEM      \n")
  cat("========================================\n")
  
  type <- get_choice("Select ('p', 'mu', 'anova', 'chisq', 'n', 'q'): ", c("p", "mu", "anova", "chisq", "n", "q"))
  if (type == "q") { cat("\nExiting. Goodbye!\n"); break }
  
  cat("\n--- DATA ENTRY ---\n")
  
  if (type == "n") {
    sub <- get_choice("For 'p' or 'mu'? ", c("p", "mu"))
    conf <- get_prob("Conf Level: "); E <- get_positive_number("Margin of Error: ")
    z <- qnorm(1 - (1 - conf)/2)
    
    if (sub == "p") {
      p_hat <- get_prob("Est. p (use 0.5 if unknown): ")
      n_req <- ceiling((p_hat * (1 - p_hat)) * (z/E)^2)
    } else {
      sigma <- get_positive_number("Sigma: ")
      n_req <- ceiling(((z * sigma)/E)^2)
    }
    cat("\nRequired n:", n_req, "\n"); next
  }
  
  if (type == "anova") {
    k <- get_count("Num Groups: ", 2); data_list <- list(); labels <- c()
    for (i in 1:k) {
      repeat {
        g_data <- get_vector(sprintf("Group %d Data", i))
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
    mode <- get_choice("'g' for Goodness of Fit, 't' for Independence: ", c("g", "t"))
    if (mode == "g") {
      obs <- get_count_vector("Observed counts")
      if (get_yes_no("Equal Probabilities? ") == "yes") {
        tryCatch(print(chisq.test(obs)), error = function(e) cat("Error:", e$message, "\n"))
      } else {
        repeat {
          probs <- get_vector("Custom Probabilities")
          if (length(probs) == length(obs) && abs(sum(probs) - 1) < 1e-6 && all(probs > 0)) break
          cat("Error: Must be positive, match length, and sum to 1.\n")
        }
        tryCatch(print(chisq.test(obs, p = probs)), error = function(e) cat("Error:", e$message, "\n"))
      }
    } else {
      nr <- get_count("Rows: ", 2); nc <- get_count("Cols: ", 2); vals <- c()
      for (i in 1:nr) {
        repeat {
          row <- get_count_vector(sprintf("Row %d Data", i))
          if (length(row) == nc) { vals <- c(vals, row); break }
          cat(sprintf("Error: Expected exactly %d values.\n", nc))
        }
      }
      tryCatch(print(chisq.test(matrix(vals, nrow = nr, byrow = TRUE), correct = FALSE)), error = function(e) cat("Error:", e$message, "\n"))
    }
    next
  }
  
  samples <- as.numeric(get_choice("Samples (1 or 2): ", c("1", "2")))
  test_tail <- get_choice("Tail ('two', 'left', 'right'): ", c("two", "left", "right"))
  conf <- get_prob("Confidence level: ")
  alt <- c("two" = "two.sided", "left" = "less", "right" = "greater")[test_tail]
  
  if (type == "p") {
    tryCatch({
      if (samples == 1) {
        p0 <- get_prob("Null p0: ")
        repeat { x1 <- get_count("Successes x: "); n1 <- get_count("n: ", 1); if (x1 <= n1) break; cat("Error: x > n.\n") }
        print(binom.test(x1, n1, p = p0, alternative = alt, conf.level = conf))
      } else {
        repeat { 
          x1 <- get_count("x1: "); n1 <- get_count("n1: ", 1)
          x2 <- get_count("x2: "); n2 <- get_count("n2: ", 1)
          if (x1 <= n1 && x2 <= n2) break
          cat("Error: Number of successes (x) cannot exceed sample size (n).\n")
        }
        print(prop.test(c(x1, x2), c(n1, n2), alternative = alt, conf.level = conf, correct = FALSE))
      }
    }, error = function(e) cat("Error:", e$message, "\n"))
  } else if (type == "mu") {
    tryCatch({
      mu0 <- get_number("Null mu0/diff: ")
      if (samples == 1) {
        xb <- get_number("Mean: "); n <- get_count("n: ", 2)
        if (get_yes_no("Sigma known? ") == "yes") {
          sig <- get_positive_number("Sigma: "); print_mean_results((xb-mu0)/(sig/sqrt(n)), NULL, alt, conf, xb, sig/sqrt(n))
        } else {
          s <- get_positive_number("s: "); print_mean_results((xb-mu0)/(s/sqrt(n)), n-1, alt, conf, xb, s/sqrt(n))
        }
      } else {
        if (get_yes_no("Paired? ") == "yes") {
          md <- get_number("Mean diff: "); sd <- get_positive_number("s diff: "); np <- get_count("Pairs: ", 2)
          print_mean_results((md-mu0)/(sd/sqrt(np)), np-1, alt, conf, md, sd/sqrt(np))
        } else {
          xb1 <- get_number("Mean 1: "); s1 <- get_positive_number("s1: "); n1 <- get_count("n1: ", 2)
          xb2 <- get_number("Mean 2: "); s2 <- get_positive_number("s2: "); n2 <- get_count("n2: ", 2)
          if (get_yes_no("Equal Var? ") == "yes") {
            df <- n1+n2-2; sp <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/df); se <- sp*sqrt(1/n1+1/n2)
          } else {
            se <- sqrt(s1^2/n1+s2^2/n2); df <- (s1^2/n1+s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
          }
          print_mean_results((xb1-xb2-mu0)/se, df, alt, conf, xb1-xb2, se)
        }
      }
    }, error = function(e) cat("Error:", e$message, "\n"))
  }
}