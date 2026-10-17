# fig_errors.R --- conditional distributions of Y given X
# (Lecture 4-2, pp. 22, 23; cond_normal.pdf is drawn by
# 04-3's copy of this script). Replaces photographs of textbook pages.
#
# Run from the deck folder:  Rscript code/fig_errors.R
#
# A generic population line E[Y|X] = 1 + 0.7 X. At three values of X the
# conditional density of Y is drawn sideways; grey dots are 200 draws from the
# same model (seed 54). Homoskedastic: sd(u|X) = 0.9. Heteroskedastic:
# sd(u|X) = 0.2 + 0.16 X. All three figures share the same axes.
#
# Writes figs/cond_homo.pdf, figs/cond_hetero.pdf.
# Lecture 4 was one deck until 2026-10-09, when it was split into 4-1, 4-2 and 4-3.

source("../course_figures.R")

a <- 1; b <- 0.7
xs <- c(2, 5, 8)
sd_homo <- function(x) rep(0.9, length(x))
sd_het  <- function(x) 0.2 + 0.16 * x
xlim <- c(0, 10.6); ylim <- c(-2.2, 12.2)
w <- 1.6 / dnorm(0, 0, 0.65)          # common scale: equal areas across curves

cond_plot <- function(sdf, fill = FALSE, labels = NULL, note = NULL, seed = 54) {
  set.seed(seed)
  xx <- runif(200, 0.3, 9.7)
  yy <- a + b * xx + rnorm(200, 0, sdf(xx))
  plot(NA, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "X", ylab = "Y")
  axis(1, at = xs, labels = expression(x[1], x[2], x[3]))
  axis(1, at = xlim, labels = FALSE, tcl = 0)
  box(bty = "l")
  points(xx, yy, pch = 16, cex = 0.5, col = col_light)
  clip(xlim[1], xlim[2], ylim[1], ylim[2])
  abline(a, b, col = col_cardinal, lwd = 2.2)
  do.call(clip, as.list(par("usr")))
  for (j in seq_along(xs)) {
    m <- a + b * xs[j]; s <- sdf(xs[j])
    yv <- seq(m - 3.2 * s, m + 3.2 * s, length.out = 200)
    dv <- xs[j] + w * dnorm(yv, m, s)
    if (fill) polygon(c(dv, rep(xs[j], 200)), c(yv, rev(yv)),
                      col = alpha_col(pal[["blue"]], 0.25), border = NA)
    segments(xs[j], min(yv), xs[j], max(yv), col = col_grey, lwd = 1)
    lines(dv, yv, col = pal[["blue"]], lwd = 2)
    segments(xs[j], m, xs[j] + w * dnorm(0, 0, s), m, lty = 3, col = pal[["blue"]])
    points(xs[j], m, pch = 16, col = col_cardinal, cex = 0.9)
    if (!is.null(labels)) text(xs[j] + 0.15, m - 3.4 * s, labels[[j]],
                               adj = c(0, 1), cex = 0.78, col = pal[["blue"]])
  }
  text(0.3, 11.7, expression("population line " * E(Y ~ "|" ~ X) == beta[0] + beta[1] * X),
       adj = c(0, 1), col = col_cardinal)
  if (!is.null(note)) {
    text(0.3, 10.3, note[[1]], adj = c(0, 1), col = pal[["blue"]])
    text(0.3, 9.1, note[[2]], adj = c(0, 1), col = pal[["blue"]])
  }
}

fig_pdf("cond_homo", cond_plot(sd_homo,
        note = list("same spread at every value of X:",
                    expression(Var(u ~ "|" ~ X) == sigma[u]^2))))
fig_pdf("cond_hetero", cond_plot(sd_het,
        note = list("spread grows with X:",
                    expression(Var(u ~ "|" ~ X == x[1]) < Var(u ~ "|" ~ X == x[3])))))
