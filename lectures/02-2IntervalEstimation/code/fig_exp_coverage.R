# fig_exp_coverage.R --- coverage of 95% intervals for the mean of a skewed
# population, as the sample size grows.
#
# Population: X ~ Exponential with mean mu = 1. For each sample size n, draw
# R = 20,000 samples, form the normal interval Xbar_n -/+ 1.96 S_n / sqrt(n)
# and the t interval Xbar_n -/+ t_{.025, n-1} S_n / sqrt(n), and record the
# share of intervals that contain mu = 1.
#
#   figs/exp_coverage_step1.pdf  the population density only
#   figs/exp_coverage_step2.pdf  the density and the coverage curves
#                                (same layout and axes; step 1 leaves the
#                                right panel empty)
#
# Slide "Simulation Study: 95% Asymptotic Confidence Interval".
# Takes a few seconds. Run from the deck folder:
#   Rscript code/fig_exp_coverage.R

source("../course_figures.R")
source("code/lib/stable_pdf.R")   # stable_pdf(): fig_pdf() that skips unchanged figures

set.seed(450)
mu <- 1
R  <- 20000
ns <- c(2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 25, 30, 40, 50, 60, 80, 100,
        125, 150, 200, 250, 300, 350, 400)

cover <- t(sapply(ns, function(n) {
  X  <- matrix(rexp(R * n, rate = 1 / mu), nrow = R)
  m  <- rowMeans(X)
  se <- sqrt(rowSums((X - m)^2) / (n - 1)) / sqrt(n)   # S_n / sqrt(n)
  c(n = n,
    normal = mean(abs(m - mu) <= qnorm(0.975) * se),
    t      = mean(abs(m - mu) <= qt(0.975, df = n - 1) * se))
}))
cover <- as.data.frame(cover)
cat(sprintf("Monte Carlo SE of a coverage near .95: %.4f\n", sqrt(0.95 * 0.05 / R)))
print(format(cover, digits = 3), row.names = FALSE)

minus <- function(x) sub("-", "\u2212", format(x, trim = TRUE))

draw_population <- function() {
  par(mar = c(3.2, 3.6, 1.9, 1.2))
  plot(NA, xlim = c(0, 5), ylim = c(0, 1.05), xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "x", ylab = "Density")
  axis(1, at = 0:5)
  axis(2, at = seq(0, 1, 0.25), labels = c("0", ".25", ".50", ".75", "1"))
  xs <- seq(0, 5, length.out = 300)
  polygon(c(0, xs, 5), c(0, dexp(xs, 1 / mu), 0), col = alpha_col(col_grey, 0.25), border = NA)
  lines(xs, dexp(xs, 1 / mu), lwd = 2.2, col = col_dark)
  segments(mu, 0, mu, dexp(mu, 1 / mu), lty = 2, col = col_dark, lwd = 1.3)
  text(mu + 0.12, dexp(mu, 1 / mu) + 0.04, expression(mu == 1), adj = c(0, 0), col = col_dark)
  mtext("Population: Exp(1)", side = 3, line = 0.5, adj = 0, font = 2)
}

draw_coverage <- function() {
  par(mar = c(3.2, 3.8, 1.9, 0.8))
  plot(NA, xlim = c(1.8, 440), ylim = c(0.6, 1), log = "x", xaxs = "i", yaxs = "i",
       axes = FALSE, xlab = "Sample size n (log scale)", ylab = "Coverage")
  axis(1, at = c(2, 5, 10, 20, 50, 100, 200, 400))
  axis(2, at = seq(0.6, 1, 0.1), labels = c(".60", ".70", ".80", ".90", "1"))
  abline(h = 0.95, col = col_grey, lty = 2)
  text(440, 0.95, ".95", adj = c(1.1, -0.4), col = col_grey)
  lines(cover$n, cover$t, col = pal[["blue"]], lwd = 2)
  points(cover$n, cover$t, col = pal[["blue"]], pch = 19, cex = 0.55)
  lines(cover$n, cover$normal, col = col_cardinal, lwd = 2)
  points(cover$n, cover$normal, col = col_cardinal, pch = 19, cex = 0.55)
  text(2.3, cover$t[1] + 0.012, "t interval", adj = c(0, 0), col = pal[["blue"]])
  text(2.6, cover$normal[1], "normal interval", adj = c(0, 0.5), col = col_cardinal)
  mtext("Coverage of the 95% intervals", side = 3, line = 0.5, adj = 0, font = 2)
}

panels <- function() layout(matrix(1:2, 1), widths = c(1, 1.55))

stable_pdf("exp_coverage_step1", {
  panels()
  draw_population()
  plot.new()
})

stable_pdf("exp_coverage_step2", {
  panels()
  draw_population()
  draw_coverage()
})
