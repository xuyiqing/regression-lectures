# fig_condvar.R --- lecture 6-3, "Example: V[u|X] = sigma^2 Homoskedasticity"
# and "Example: V[u|X] = sigma_i^2 Heteroskedasticity".
#
# Replaces two 2009 scans of textbook figures (homo1.pdf, homo2.pdf). Same
# point: the conditional distribution of y given x is centred on the
# regression line E[y|x] = b0 + b1 x; under homoskedasticity its spread is the
# same at every x, under heteroskedasticity the spread grows with x.
# Simulated data (fixed seed). Both figures share axes, size and density scale
# (every density has the same area, so a wider one is also lower).
#
# Run from the deck folder:  Rscript code/fig_condvar.R
# Writes figs/condvar_homo.pdf and figs/condvar_hetero.pdf.

source("../course_figures.R")

b0 <- 1
b1 <- 0.8
xk <- c(2, 5, 8)                              # where the densities are drawn
sd_homo   <- function(x) rep(1, length(x))
sd_hetero <- function(x) (x + 1) / 6          # 0.5, 1.0, 1.5 at x = 2, 5, 8

xlim <- c(0, 10.4)
ylim <- c(-1.9, 14)
scale <- 0.9 * sqrt(2 * pi)                   # a density with sd 1 is 0.9 x-units wide

set.seed(6302)
n <- 300
x <- runif(n, 0.8, 9.7)
z <- pmax(pmin(rnorm(n), 3), -3)              # keep every point inside the frame

draw_condvar <- function(sdfun) {
  y <- b0 + b1 * x + sdfun(x) * z
  plot(NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")
  points(x, y, pch = 16, cex = 0.55, col = alpha_col(col_grey, 0.5))
  for (k in seq_along(xk)) {
    m <- b0 + b1 * xk[k]
    s <- sdfun(xk[k])
    yy <- seq(m - 3.2 * s, m + 3.2 * s, length.out = 200)
    dx <- scale * dnorm(yy, m, s)
    segments(xk[k], ylim[1], xk[k], ylim[2], col = col_light, lty = 2)
    polygon(c(xk[k] + dx, xk[k]), c(yy, yy[1]), border = NA,
            col = alpha_col(pal["blue"], 0.25))
    lines(xk[k] + dx, yy, col = pal["blue"], lwd = 1.6)
  }
  abline(a = b0, b = b1, col = col_cardinal, lwd = 2.2)

  # legend, upper left (empty in both figures)
  lx <- 0.35
  segments(lx, 12.9, lx + 0.8, 12.9, col = col_cardinal, lwd = 2.2)
  text(lx + 1.0, 12.9, expression(E * "[" * y ~ "|" ~ x * "]" == beta[0] + beta[1] * x),
       adj = c(0, 0.5), col = col_dark)
  rect(lx, 10.5, lx + 0.8, 11.3, col = alpha_col(pal["blue"], 0.25), border = pal["blue"])
  text(lx + 1.0, 10.9, expression(f(y ~ "|" ~ x) * ",  the distribution of " * y * " at a given " * x),
       adj = c(0, 0.5), col = col_dark)

  axis(1, at = xk, labels = expression(x[1], x[2], x[3]), lwd = 0, lwd.ticks = 1,
       col = col_dark, pos = ylim[1])
  box(bty = "l", col = col_dark)
  mtext("x", side = 1, line = 1.9, at = xlim[2], adj = 1)
  mtext("y", side = 2, line = 0.6, at = ylim[2], adj = 1, las = 1)
}

fig_pdf("condvar_homo",   draw_condvar(sd_homo),   mar = c(3.0, 1.8, 0.6, 0.6))
fig_pdf("condvar_hetero", draw_condvar(sd_hetero), mar = c(3.0, 1.8, 0.6, 0.6))
