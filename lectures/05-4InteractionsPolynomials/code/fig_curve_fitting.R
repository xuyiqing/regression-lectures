# fig_curve_fitting.R --- polynomials of degree 1 to 4 fitted to data
# generated from a sine function.
#
# Replaces the old raster image Curve_fitting1.pdf. The slide caption names the
# colours: red = degree 1, green = 2, orange = 3, blue = 4.
#
# Run from the deck folder:  Rscript code/fig_curve_fitting.R

source("../course_figures.R")

set.seed(450)
x <- seq(0, 10, length.out = 60)
y <- sin(x) + rnorm(length(x), sd = 0.12)
xg <- seq(0, 10, length.out = 400)
cols <- c(col_cardinal, pal[["green"]], pal[["orange"]], pal[["blue"]])
fits <- lapply(1:4, function(d) lm(y ~ poly(x, d, raw = TRUE)))
cat("R-squared by degree:", round(sapply(fits, function(f) summary(f)$r.squared), 3), "\n")

fig_pdf("poly_sine_fits", width = 6, height = 3.0, mar = c(3.0, 3.4, 1.6, 0.8), {
  plot(x, y, pch = 16, cex = 0.6, col = col_dark, ylim = c(-1.5, 1.5),
       xlab = expression(X), ylab = expression(Y))
  for (d in 1:4) {
    lines(xg, predict(fits[[d]], data.frame(x = xg)), col = cols[d], lwd = 2.2)
  }
  legend("top", inset = c(0, -0.13), xpd = NA, horiz = TRUE, bty = "n", cex = 0.95,
         legend = paste("degree", 1:4), col = cols, lwd = 2.2, seg.len = 1.6)
})
