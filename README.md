# 📊 Interactive Statistics Solver (R)

A robust, menu-driven command-line application built in R for performing a wide range of inferential statistical tests. This solver was designed with strict defensive programming principles to be resilient against user input errors while maintaining perfect mathematical precision for academic and professional use.

## 🚀 Statistical Capabilities

- **Proportions:** - 1-Sample: Utilizes Pearson's exact binomial method (`binom.test`) for maximum accuracy.
  - 2-Sample: Approximate difference testing.
- **Means:** Handles One-Sample Z/T tests, Paired T-tests, and Independent T-tests (dynamically switches between Student's Pooled and Welch's formulas based on variance).
- **ANOVA:** One-Way Analysis of Variance with strict group-size validation.
- **Chi-Square:** - Goodness of Fit: Supports both equal distributions and custom expected probabilities (validated to sum to 1).
  - Test for Independence: Matrix-based 2-way table analysis.
- **Planning:** Required Sample Size determination for both Means and Proportions.

## 🛠️ Built-in Guardrails & UX

- **Strict Input Validation:** Custom helper functions force clean data entry (e.g., blocking negative counts, forcing confidence levels to be strictly between 0 and 1, preventing successes from exceeding sample sizes).
- **Mathematical Transparency:** Explicitly prints out the Null Value, Point Estimate, Standard Error, Critical Values (Z/T), and Margin of Error before presenting the final R test output.
- **Dynamic Precision:** Automatically relabels outputs to identify **Confidence Bounds** for one-sided tests and **Confidence Intervals** for two-sided tests.
- **Error Handling:** Uses `tryCatch` blocks to catch mathematical impossibilities without crashing the program loop.

## 📖 How to Use

1. Open R or RStudio.
2. Source the script: `source("stats_solver.R")`
3. Follow the heavily labeled, plain-English prompts to select your test and enter your data.
4. Type `q` at the main menu to exit the application safely.
