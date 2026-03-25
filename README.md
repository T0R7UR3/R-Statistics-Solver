# 📊 Interactive Statistics Solver (R)

A robust, menu-driven command-line tool built in R for performing a wide range of inferential statistical tests. This solver was designed to be resilient against user input errors and mathematically precise for academic and professional use.

## 🚀 Features

- **Proportions:** 1-Sample (Exact Binomial Test) and 2-Sample (Approximate) tests.
- **Means:** Handles One-Sample Z/T tests, Paired T-tests, and Independent T-tests (Student's Pooled & Welch's).
- **ANOVA:** One-Way Analysis of Variance with group-size validation.
- **Chi-Square:** Both Goodness of Fit and Tests for Independence (Matrix-based).
- **Planning:** Required Sample Size determination for both Means and Proportions.

## 🛠️ Built-in Guardrails

- **Strict Input Validation:** Custom helpers for whole numbers, probabilities (0-1), and yes/no prompts to prevent "silent failures."
- **Mathematical Accuracy:** Dynamically generates **Confidence Bounds** for one-sided tests and **Confidence Intervals** for two-sided tests.
- **Error Handling:** Uses `tryCatch` blocks to catch mathematical impossibilities (like successes exceeding sample size) without crashing the program.

## 📖 How to Use

1. Open R or RStudio.
2. Source the script: `source("stats_solver.R")`.
3. Follow the on-screen prompts to select your test and enter your data.
4. Type `q` at the main menu to exit safely.

## 📈 Example Output (Mean Test)
```text
Statistic: 2.1540
P-Value: 3.1245e-02
95% Confidence Interval: [37.2100, 42.7900]
