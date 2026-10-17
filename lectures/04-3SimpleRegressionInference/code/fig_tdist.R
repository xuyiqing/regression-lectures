# fig_tdist.R --- the t test, p-value and confidence interval for the running example of
# Lecture 4 (Lecture 4-3, "The Testing Workflow" to "Testing with Confidence Intervals", and the
# interpretation slide under "Practical Advice").
#
# Running example (2026-10-11 examples rebuild; replaces the Black-elected-officials data):
# Y = trump, Trump's % of the two-party vote in 2024 (MEDSL state totals, doi:10.7910/DVN/42MVDX, CC0);
# X = ba, % of adults 25+ with a bachelor's degree or higher (ACS 2019-23 5-year, table B15003).
# 50 states, DC excluded. n = 50, so T ~ t with 48 df under H0. Same model as 4-1's fig_states.R.
#
# Run from the deck folder:  Rscript code/fig_tdist.R
# Reads code/data/state_pres.csv (a copy of lectures/_data/us_elections/state_pres.csv).
# Writes figs/tdist_null.pdf, figs/tdist_reject.pdf, figs/tdist_observed.pdf and
# code/tdist_output.txt (every number the slides print about this test).

source("../course_figures.R")

raw <- read.csv("code/data/state_pres.csv", colClasses = c(state_fips = "character"))
states <- data.frame(st = raw$state_po, trump = 100 * raw$rep2p_2024,
                     ba = raw$pct_ba)[raw$is_dc == 0, ]
stopifnot(nrow(states) == 50, !anyNA(states))

options(width = 80, digits = 7)
lm.out <- lm(trump ~ ba, data = states)
s <- summary(lm.out)
df <- lm.out$df.residual
tc <- qt(0.975, df)
tc1 <- qt(0.95, df)
b1 <- coef(s)["ba", "Estimate"]; se1 <- coef(s)["ba", "Std. Error"]
tobs <- coef(s)["ba", "t value"]
pobs <- coef(s)["ba", "Pr(>|t|)"]
# the numbers 4-1 v2 prints (04-1SimpleRegressionMechanics/v2/code/states_output.txt)
stopifnot(df == 48, round(tc, 2) == 2.01, round(tobs, 2) == -8.37,
          round(b1, 4) == -1.4537, round(se1, 4) == 0.1738)

z <- seq(-4.2, 4.2, length.out = 500)
tframe <- function() {
  plot(z, dt(z, df), type = "n", xlim = c(-4.2, 4.2), ylim = c(0, 0.5),
       xaxs = "i", yaxs = "i", axes = FALSE, xlab = "t", ylab = "density")
  axis(1, at = c(-4, -2, 0, 2, 4)); axis(2, at = c(0, 0.2, 0.4))
  box(bty = "l")
}
tcurve <- function() {
  lines(z, dt(z, df), lwd = 2, col = col_dark)
  mtext(bquote("null distribution " * T %~% tau[.(df)]), side = 3, line = 0.3, cex = 0.85)
}
reject <- function() {
  for (sgn in c(-1, 1)) {
    zz <- if (sgn < 0) z[z <= -tc] else z[z >= tc]
    polygon(c(zz, rev(zz)), c(dt(zz, df), rep(0, length(zz))),
            col = alpha_col(col_cardinal, 0.55), border = NA)
    segments(sgn * tc, 0, sgn * tc, 0.47, lty = 2, col = col_cardinal)
    text(sgn * 3.15, 0.07, "2.5%", col = col_cardinal, cex = 0.75)
  }
  lines(z, dt(z, df), lwd = 2, col = col_dark)
  text(c(-3.1, 0, 3.1), 0.445, c("reject", "fail to reject", "reject"),
       col = c(col_cardinal, col_dark, col_cardinal), cex = 0.8)
  axis(1, at = c(-tc, tc), labels = sprintf(c("−%.2f", "%.2f"), tc),
       col.axis = col_cardinal, cex.axis = 0.75, mgp = c(2.1, 1.35, 0), tcl = -0.25)
}

fig_pdf("tdist_null", width = 2.7, height = 2.4, mar = c(3.1, 3.4, 1.5, 0.4),
        { tframe(); tcurve() })
fig_pdf("tdist_reject", width = 2.7, height = 2.4, mar = c(3.1, 3.4, 1.5, 0.4),
        { tframe(); tcurve(); reject() })
# The observed t is -8.37, far off the left edge of the chart: the arrow points left.
fig_pdf("tdist_observed", width = 2.7, height = 2.4, mar = c(3.1, 3.4, 1.5, 0.4), {
  tframe(); tcurve(); reject()
  text(-4.15, 0.385, "observed", adj = c(0, 0.5), col = pal[["blue"]], cex = 0.66)
  text(-4.15, 0.345, bquote(T == .(sub("^-", "−", sprintf("%.2f", tobs)))), adj = c(0, 0.5), cex = 0.66,
       col = pal[["blue"]])
  arrows(-2.35, 0.30, -4.12, 0.30, length = 0.06, lwd = 1.8, col = pal[["blue"]])
})

ci <- b1 + c(-1, 1) * tc * se1
out <- c(
  "# fig_tdist.R output: tests and intervals for the running example (Lecture 4-3)",
  "# Y = trump (Trump % of two-party vote, 2024); X = ba (% adults 25+ with BA+, ACS 2019-23); 50 states",
  "",
  "> lm.out <- lm(trump ~ ba, data = states)",
  "> summary(lm.out)",
  capture.output(s),
  "",
  "> confint(lm.out)",
  capture.output(confint(lm.out)),
  "",
  "# ---- numbers on the slides",
  sprintf("n = %d; residual df = %d", nrow(states), df),
  sprintf("b1 = %.4f; SE(b1) = %.5f", b1, se1),
  sprintf("two-sided critical value qt(0.975, %d) = %.4f (slides: 2.01)", df, tc),
  sprintf("one-sided critical value qt(0.95, %d) = %.4f (slides: 1.68; lower tail -1.68)", df, tc1),
  sprintf("T for H0: b1 = 0: %.4f / %.5f = %.4f (slides: -1.4537/0.17376 = -8.37)", b1, se1, tobs),
  sprintf("two-sided p-value = %.3g", pobs),
  sprintf("one-sided p-value for H1: b1 < 0 = %.3g", pt(tobs, df)),
  sprintf("95%% CI by hand: %.4f +/- %.4f * %.4f = [%.4f, %.4f] (slides: [-1.80, -1.10])",
          b1, tc, se1, ci[1], ci[2]),
  sprintf("with rounded inputs: -1.45 +/- 2.01 * 0.174 = [%.4f, %.4f]",
          -1.45 - 2.01 * 0.174, -1.45 + 2.01 * 0.174),
  sprintf("interpretation slide: a state with a 1-point higher BA share has a fitted Trump share %.2f points lower",
          -b1)
)
writeLines(out, "code/tdist_output.txt")
cat(sprintf("df = %d, critical value = %.4f, observed t = %.3f\n", df, tc, tobs))
