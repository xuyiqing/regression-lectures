# fig_ci_sim.R --- coverage of the known-variance 95% interval, by simulation.
#
# 100 samples of size n = 100 from N(0, 1). For each sample, the interval
# Xbar_n -/+ 1.96 / sqrt(n) (sigma = 1 known). Drawn in three animation steps
# with identical axes and size:
#
#   figs/ci_sim_step1.pdf  sample 1 only
#   figs/ci_sim_step2.pdf  samples 1-2
#   figs/ci_sim_step3.pdf  all 100 samples; intervals that miss mu = 0 in cardinal
#
# Slide "Simulating Coverage Probability of Confidence Intervals" (frame 6;
# problem set 2, problem 4B, refers to it as "slide 6").
# Run from the deck folder:  Rscript code/fig_ci_sim.R
#
# Seed rule. The seed is the first one >= 450 whose 100 intervals include
# 3 to 7 misses, each of which clears mu by at least 0.03 (about 4pt on the
# slide), so that no red interval looks as if it touches the mu line. Seeds
# 450-501 fail the rule (seed 450 has two misses within 0.011 of mu; seed 486
# has no miss at all). Seed 502 is the first that passes: 97 of 100
# intervals cover mu, and the misses clear it by 0.091, 0.090 and 0.063.
# The stopifnot() below checks the rule on every run.

source("../course_figures.R")
source("code/lib/stable_pdf.R")   # stable_pdf(): fig_pdf() that skips unchanged figures

set.seed(502)
R  <- 100                      # number of samples
n  <- 100                      # size of each sample
mu <- 0
X    <- matrix(rnorm(R * n, mean = mu, sd = 1), nrow = R)
xbar <- rowMeans(X)
half <- qnorm(0.975) * 1 / sqrt(n)
lo   <- xbar - half
hi   <- xbar + half
covers <- lo <= mu & mu <= hi
k <- sum(covers)
gap <- pmax(lo - mu, mu - hi)[!covers]     # how far each miss is from mu
stopifnot(R - k >= 3, R - k <= 7, all(gap >= 0.03))   # the seed rule above

f2 <- function(x) sub("-", "\u2212", sprintf("%.2f", x))
f1 <- function(x) sub("-", "\u2212", ifelse(x == 0, "0", sprintf("%.1f", x)))
cat(sprintf("half-width = %.4f\n", half))
cat(sprintf("sample 1: xbar = %.4f  interval [%.4f, %.4f]\n", xbar[1], lo[1], hi[1]))
cat(sprintf("sample 2: xbar = %.4f  interval [%.4f, %.4f]\n", xbar[2], lo[2], hi[2]))
cat(sprintf("%d of %d intervals contain mu = 0; misses: samples %s; distance from mu: %s\n",
            k, R, paste(which(!covers), collapse = ", "),
            paste(sprintf("%.3f", gap), collapse = ", ")))

ylim <- c(-0.55, 0.55)
stopifnot(min(lo) > ylim[1], max(hi) < ylim[2])   # same axes in every step

draw_step <- function(m, caption) {
  plot(NA, xlim = c(0, R + 1), ylim = ylim, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "Sample", ylab = expression(bar(X)[n] %+-% 1.96 / sqrt(n)))
  axis(1, at = c(1, seq(20, R, 20)))
  yt <- seq(-0.4, 0.4, 0.2)
  axis(2, at = yt, labels = f1(yt))
  abline(h = mu, col = col_dark, lwd = 1.1)
  mtext(expression(mu == 0), side = 4, at = mu, las = 1, line = 0.3, col = col_dark)
  s <- seq_len(m)
  col_s <- ifelse(covers[s], col_grey, col_cardinal)
  segments(s, lo[s], s, hi[s], col = col_s, lwd = ifelse(covers[s], 1.4, 2.0),
           lend = "butt")          # line ends exactly at the interval ends
  points(s, xbar[s], pch = 19, cex = 0.45, col = ifelse(covers[s], col_dark, col_cardinal))
  mtext(caption, side = 3, line = 0.35, adj = 0, cex = 0.95, col = col_dark)
}

stable_pdf("ci_sim_step1", {
  draw_step(1, sprintf("Sample 1:  interval [%s, %s]", f2(lo[1]), f2(hi[1])))
}, mar = c(3.2, 4.0, 1.8, 2.6))

stable_pdf("ci_sim_step2", {
  draw_step(2, sprintf("Sample 2:  interval [%s, %s]", f2(lo[2]), f2(hi[2])))
}, mar = c(3.2, 4.0, 1.8, 2.6))

stable_pdf("ci_sim_step3", {
  draw_step(R, sprintf("%d of %d intervals contain \u03bc = 0;  the %d that miss are in red",
                       k, R, R - k))
}, mar = c(3.2, 4.0, 1.8, 2.6))
