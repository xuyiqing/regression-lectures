# fig_quartet.R --- Anscombe's quartet (Lecture 4-1, p. 33, "R^2 in context").
#
# Run from the deck folder:  Rscript code/fig_quartet.R
#
# Data: datasets::anscombe (F. J. Anscombe, 1973, "Graphs in Statistical
# Analysis," The American Statistician 27(1): 17-21). Four data sets with the
# same OLS line and R^2 but very different patterns. Each panel prints its own
# fit, computed here; all four round to y-hat = 3.00 + 0.500 x and R^2 = 0.67.
# (The 2011 figure this replaces printed "beta_0 = .3" and "r^2 = .66".)
#
# Writes figs/anscombe_quartet.pdf and prints the four fits.

source("../course_figures.R")

a <- datasets::anscombe
fits <- lapply(1:4, function(k) lm(a[[paste0("y", k)]] ~ a[[paste0("x", k)]]))

fig_pdf("anscombe_quartet", width = 5.4, height = 3.3, pointsize = 11,
        mar = c(2.3, 2.5, 0.4, 0.3), {
  par(mfrow = c(2, 2), cex = 1, mgp = c(1.35, 0.4, 0), tcl = -0.25)
  for (k in 1:4) {
    x <- a[[paste0("x", k)]]; y <- a[[paste0("y", k)]]
    cf <- coef(fits[[k]]); r2 <- summary(fits[[k]])$r.squared
    plot(x, y, xlim = c(3, 20), ylim = c(2.5, 13.5), xaxs = "i", axes = FALSE,
         type = "n", xlab = bquote(x[.(k)]), ylab = bquote(y[.(k)]))
    axis(1, at = c(5, 10, 15, 20)); axis(2, at = c(4, 8, 12))
    box(bty = "l")
    abline(cf, col = col_cardinal, lwd = 1.8)
    points(x, y, pch = 21, bg = alpha_col(pal[["blue"]], 0.65), col = col_dark, cex = 0.95)
    text(19.7, 4.8, bquote(hat(y) == .(sprintf("%.2f", cf[1])) + .(sprintf("%.3f", cf[2])) * x),
         adj = c(1, 0), col = col_cardinal, cex = 0.95)
    text(19.7, 2.9, bquote(R^2 == .(sprintf("%.2f", r2))),
         adj = c(1, 0), col = col_cardinal, cex = 0.95)
  }
})

for (k in 1:4) {
  s <- summary(fits[[k]])
  cat(sprintf("set %d: intercept %.4f  slope %.4f  R2 %.4f\n", k,
              coef(fits[[k]])[1], coef(fits[[k]])[2], s$r.squared))
}
