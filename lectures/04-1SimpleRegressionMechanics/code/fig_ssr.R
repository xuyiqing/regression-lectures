# fig_ssr.R --- least-squares objective functions (Lecture 4-1, pp. 16-19).
#
# Run from the deck folder:  Rscript code/fig_ssr.R
#
# One variable: S(mu) = sum_i (y_i - mu)^2 for a simulated sample of
# n = 25 values (y_i ~ N(50, 20^2), seed 450), and its tangent lines.
# (The two-coefficient surface and contours for the running example moved to
# fig_states.R in the 2026-10-11 rebuild: figs/states_ssr_*.pdf.)
#
# Writes figs/ssr_mu*.pdf.

source("../course_figures.R")

fmt <- function(v, d = 0, big = ",") {
  sub("^-", "\u2212", formatC(v, format = "f", digits = d, big.mark = big))
}

# ------------------------------------------------------------- one variable
set.seed(450)
n <- 25
y <- round(rnorm(n, mean = 50, sd = 20), 1)
ybar <- mean(y)
S <- function(m) vapply(m, function(mm) sum((y - mm)^2), numeric(1))
dS <- function(m) -2 * sum(y - m)            # equals 2 n (m - ybar)
mu <- seq(0, 100, length.out = 401)
ylim_mu <- c(0, max(S(mu)) * 1.04)

mu_frame <- function() {
  plot(mu, S(mu) / 1000, type = "n", xlim = c(0, 100), ylim = ylim_mu / 1000,
       xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = expression(tilde(mu)),
       ylab = expression(S(tilde(mu)) ~ "(thousands)"))
  axis(1, at = seq(0, 100, 20)); axis(2)
  box(bty = "l")
  lines(mu, S(mu) / 1000, col = col_cardinal, lwd = 2.2)
  text(99, ylim_mu[2] / 1000 * 0.06,
       sprintf("simulated sample, n = %d", n), adj = c(1, 0.5), col = col_grey, cex = 0.8)
}

fig_pdf("ssr_mu", mu_frame())

tangent <- function(m0, label_side = 4) {
  s0 <- S(m0); d0 <- dS(m0)
  clip(0, 100, 0, ylim_mu[2] / 1000)
  abline(a = (s0 - d0 * m0) / 1000, b = d0 / 1000, col = pal[["blue"]], lwd = 1.8)
  do.call(clip, as.list(par("usr")))
  segments(m0, 0, m0, s0 / 1000, lty = 3, col = col_grey)
  points(m0, s0 / 1000, pch = 21, bg = pal[["blue"]], col = "white", cex = 1.3)
  lab <- if (abs(d0) < 1e-8) {
    bquote({"slope " * dS / d * tilde(mu) == 0} * "  at  " * {tilde(mu) == bar(y)} == .(fmt(ybar, 1)))
  } else {
    bquote({"slope " * dS / d * tilde(mu) == .(fmt(d0, 0))} * "  at  " * tilde(mu) == .(m0))
  }
  text(if (abs(d0) < 1e-8) m0 else m0 + 3, s0 / 1000 + if (abs(d0) < 1e-8) 17 else 30, lab,
       adj = c(if (abs(d0) < 1e-8) 0.5 else 0, 0.5), col = pal[["blue"]])
}
fig_pdf("ssr_mu_tangent1", { mu_frame(); tangent(20) })
fig_pdf("ssr_mu_tangent2", { mu_frame(); tangent(35) })
fig_pdf("ssr_mu_tangent3", { mu_frame(); tangent(ybar) })

cat(sprintf("univariate sample: n = %d, mean = %.2f, S(mean) = %.1f\n", n, ybar, S(ybar)))
cat(sprintf("tangent slopes: at 20: %.1f; at 35: %.1f; at mean: %.1f\n", dS(20), dS(35), dS(ybar)))
