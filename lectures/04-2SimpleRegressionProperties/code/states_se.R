# states_se.R --- standard errors for the Lecture 4 running example (deck 4-2, frame "College Degrees
# and the 2024 Trump Vote, 50 States", after "Estimating the Variance of OLS Estimators").
#
# Y = Trump's % of the two-party (Trump + Harris) vote in 2024, X = % of adults 25+ with a bachelor's
# degree or higher (ACS 2019-23), 50 states, DC excluded. Same data and model as 4-1's code/fig_states.R;
# this script checks that its numbers match 4-1's states_output.txt and adds the standard errors by hand.
#
# Data: code/data/state_pres.csv, a copy of lectures/_data/us_elections/state_pres.csv (MEDSL state
# totals, doi:10.7910/DVN/42MVDX, CC0; ACS table B15003, public domain; see that folder's README.md).
#
# Run from the deck folder:  Rscript code/states_se.R
# Writes: code/states_se_output.txt (every number the slide prints). No figures.
#
# Replaces the R output of the Black-elected-officials data (41 states, source unknown) in the
# 2026-10-11 examples rebuild.

suppressPackageStartupMessages(library(lmtest))

raw <- read.csv("code/data/state_pres.csv", colClasses = c(state_fips = "character"))
stopifnot(nrow(raw) == 51, sum(raw$is_dc) == 1)
states <- data.frame(state = raw$state, st = raw$state_po,
                     trump = 100 * raw$rep2p_2024, ba = raw$pct_ba)[raw$is_dc == 0, ]
stopifnot(nrow(states) == 50, !anyNA(states))

options(useFancyQuotes = FALSE, width = 80, digits = 7)
lm.out <- lm(trump ~ ba, data = states)
s <- summary(lm.out)
cf <- coef(s)
# the numbers in 4-1 v2's code/states_output.txt
stopifnot(round(cf[2, 1], 4) == -1.4537, round(cf[2, 2], 4) == 0.1738,
          round(cf[1, 1], 4) == 102.8376, round(cf[1, 2], 4) == 5.9818,
          round(s$r.squared, 4) == 0.5932, s$df[2] == 48)

n <- nrow(states); x <- states$ba
ssr <- sum(resid(lm.out)^2)
sig2 <- ssr / (n - 2)
sxx <- sum((x - mean(x))^2)
se1 <- sqrt(sig2 / sxx)
se0 <- sqrt(sig2 * (1 / n + mean(x)^2 / sxx))
stopifnot(abs(se1 - cf[2, 2]) < 1e-10, abs(se0 - cf[1, 2]) < 1e-10)
bp <- bptest(lm.out)

out <- c(
  "# states_se.R output (Lecture 4-2): the Lecture 4 running example, 50 states, DC excluded",
  "",
  "> lm.out <- lm(trump ~ ba, data = states)",
  "> summary(lm.out)",
  capture.output(s),
  "",
  "# ---- standard errors by hand (the formulas on the previous slide)",
  sprintf("n = %d; sum of squared residuals = %.2f; sigma-hat^2 = %.2f / %d = %.4f; sigma-hat = %.4f",
          n, ssr, ssr, n - 2, sig2, sqrt(sig2)),
  sprintf("mean(ba) = %.4f; Sxx = sum((x - xbar)^2) = %.4f", mean(x), sxx),
  sprintf("SE(b1) = sqrt(%.4f / %.4f) = %.4f (R: %.4f)", sig2, sxx, se1, cf[2, 2]),
  sprintf("SE(b0) = sqrt(%.4f * (1/%d + %.4f^2 / %.4f)) = %.4f (R: %.4f)",
          sig2, n, mean(x), sxx, se0, cf[1, 2]),
  sprintf("rounded for the slide: sigma-hat^2 = %.1f; Sxx = %.1f; SE(b1) = %.3f; SE(b0) = %.2f",
          sig2, sxx, se1, se0),
  "",
  "# ---- Assumption V check (Breusch-Pagan, studentized; taught in Lecture 6-3)",
  sprintf("BP = %.3f, df = %d, p = %.3f", bp$statistic, bp$parameter, bp$p.value)
)
writeLines(out, "code/states_se_output.txt")
cat(out, sep = "\n")
