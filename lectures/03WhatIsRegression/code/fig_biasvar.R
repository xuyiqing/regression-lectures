# fig_biasvar.R --- Lecture 3, subsection "Bias-Variance Tradeoff".
# A simulation with a known population. X is held at 32 evenly spaced
# values from 0.26 to 0.76 (until 2026-10 these were the 32 inequality values
# of the Chirot data, which the deck no longer uses); for each X, Y is drawn from
#     Y | X = x  ~  N( m(x), 1 ),   m(x) = -2 + 0.25 exp(5.3 (x - 0.26)).
# From this population we draw 2,000 data sets and apply two interval
# (binned-mean) estimators of E[Y|X]: X split into 4 or 8 ranges holding
# about the same number of observations, and the mean of Y taken in each.
#
#   figs/bv_true_persp.pdf      the conditional density f(y|x), as a surface
#   figs/bv_true_contour.pdf    the same density, shaded, with E[Y|X] on top
#   figs/bv_sim_data.pdf        one simulated data set and E[Y|X]
#   figs/bv_two_estimators.pdf  the 4- and 8-interval estimates for that data set
#   figs/bv_bias_4bins.pdf      the 4-interval estimates from all 2,000 data sets
#   figs/bv_var_8bins.pdf       the 8-interval estimates from all 2,000 data sets
#
# Numbers printed by this script go to code/biasvar_output.txt.
#
# Run from the deck folder:  Rscript --vanilla code/fig_biasvar.R

source("../course_figures.R")

x <- seq(0.26, 0.76, length.out = 32)   # fixed design, n = 32, evenly spaced
n <- length(x)
m <- function(x) -2 + 0.25 * exp(5.3 * (x - 0.26))   # true E[Y | X = x]
sigma <- 1
S <- 2000

sink("code/biasvar_output.txt", split = TRUE)
cat(sprintf("X: %d evenly spaced values from %.2f to %.2f; S = %d simulated data sets\n",
            n, min(x), max(x), S))
set.seed(20070)
ysim <- matrix(rnorm(S * n, mean = rep(m(x), each = S), sd = sigma), nrow = S)
y1 <- ysim[1, ]                          # the data set shown on the slides

# Interval estimators ------------------------------------------------------------
# Cut X at its k-quantiles. Each cut sits midway between the distinct X values
# on either side, so no observation lies on a cut (ties stay together).
cuts <- function(k) {
  ux <- sort(unique(x))
  q <- quantile(x, probs = seq_len(k - 1) / k, names = FALSE)
  vapply(q, function(v) (max(ux[ux <= v]) + min(ux[ux > v])) / 2, numeric(1))
}
bins <- lapply(c(k4 = 4, k8 = 8), function(k) {
  cc <- cuts(k)
  list(cuts = cc,
       id   = findInterval(x, cc) + 1,
       lo   = c(min(x), cc),
       hi   = c(cc, max(x)))
})
for (b in names(bins)) {
  cat(b, "cuts:", bins[[b]]$cuts, " counts:", tabulate(bins[[b]]$id), "\n")
}

# S x k matrix of bin means
bin_means <- function(b, ymat) {
  sapply(seq_along(b$lo), function(j) rowMeans(ymat[, b$id == j, drop = FALSE]))
}
est <- lapply(bins, bin_means, ymat = ysim)

# Bias and variance at each design point, from the simulation and exactly
summary_bv <- function(b, e) {
  fit <- e[, b$id]                       # S x n fitted values
  bias2_sim <- mean((colMeans(fit) - m(x))^2)
  var_sim   <- mean(apply(fit, 2, var))
  bias2_exact <- mean((tapply(m(x), b$id, mean)[b$id] - m(x))^2)
  var_exact   <- mean(sigma^2 / tabulate(b$id)[b$id])
  c(bias2 = bias2_sim, var = var_sim, bias2_exact = bias2_exact,
    var_exact = var_exact)
}
bv <- mapply(summary_bv, bins, est)
print(round(bv, 3))
sink()

# Shared frame ---------------------------------------------------------------------
xlim <- c(0.245, 0.775)
ylim_data <- c(-4.2, 4.2)
stopifnot(all(y1 > ylim_data[1] & y1 < ylim_data[2]))
xg <- seq(0.26, 0.76, length.out = 201)
axes_xy <- function(ylim, yat) {
  plot(NA, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "X", ylab = "Y")
  axis(1, at = seq(0.3, 0.7, 0.1))
  axis(2, at = yat)
}

# Population: surface and shaded contour --------------------------------------------
yg <- seq(-4.2, 4.2, length.out = 61)
xs <- seq(0.26, 0.76, length.out = 41)
z  <- outer(xs, yg, function(a, b) dnorm(b, m(a), sigma))
ramp <- colorRampPalette(c("#FFFFFF", "#E9C2CF", col_cardinal))

fig_pdf("bv_true_persp", {
  zf <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)]) / 4
  facet_col <- ramp(100)[cut(zf, 100)]
  pm <- persp(xs, yg, z, theta = 120, phi = 25, expand = 0.7, r = 4,
              col = facet_col, border = alpha_col(col_grey, 0.25), lwd = 0.3,
              ticktype = "detailed", nticks = 4, cex.axis = 0.8, cex.lab = 1,
              xlab = "X", ylab = "Y", zlab = "")
  # persp() would write the z title on top of the tick labels; put it
  # horizontally above the z axis, which stands at the corner (max X, min Y).
  top <- trans3d(max(xs), min(yg), max(z) * 1.13, pm)
  text(top$x, top$y, "f(y | x)", xpd = NA)
}, width = 4.2, height = 3.0, mar = c(0.4, 0.4, 0.8, 0.4))

fig_pdf("bv_true_contour", {
  zz <- outer(xg, seq(-4.2, 4.2, length.out = 201),
              function(a, b) dnorm(b, m(a), sigma))
  axes_xy(ylim_data, -4:4)
  image(xg, seq(-4.2, 4.2, length.out = 201), zz, col = ramp(60), add = TRUE,
        useRaster = TRUE)
  contour(xg, seq(-4.2, 4.2, length.out = 201), zz,
          levels = dnorm(c(2, 1.5, 1, 0.5), 0, sigma), drawlabels = FALSE,
          col = alpha_col(col_dark, 0.35), lwd = 0.6, add = TRUE)
  lines(xg, m(xg), col = col_dark, lwd = 2.2)
  legend(xlim[1] + 0.008, ylim_data[2] - 0.05, bty = "n", cex = 0.85,
         lwd = c(2.2, NA), pch = c(NA, 15), pt.cex = c(NA, 1.8),
         col = c(col_dark, col_cardinal),
         legend = c("true E[Y|X]",
                    "density of Y at each X (darker = more likely)"))
  axis(1, at = seq(0.3, 0.7, 0.1))       # redraw the axes over the shading
  axis(2, at = -4:4)
}, width = 6, height = 3.1)

# One simulated data set --------------------------------------------------------
fig_pdf("bv_sim_data", {
  axes_xy(ylim_data, -4:4)
  lines(xg, m(xg), col = col_dark, lwd = 2)
  points(x, y1, pch = 16, cex = 0.95, col = col_cardinal)
  legend(xlim[1] + 0.008, ylim_data[2] - 0.1, bty = "n", cex = 0.9,
         pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2),
         col = c(col_cardinal, col_dark),
         legend = c("one simulated data set (n = 32)", "true E[Y|X]"))
})

draw_bins <- function(b, e_row, ylim) {
  abline(v = b$cuts, col = col_grey, lty = 2, lwd = 0.8)
  segments(b$lo, e_row, b$hi, e_row, col = col_cardinal, lwd = 2.6)
}
fig_pdf("bv_two_estimators", {
  par(mfrow = c(1, 2), mar = c(3.0, 3.2, 1.6, 0.6))
  for (b in names(bins)) {
    axes_xy(ylim_data, seq(-4, 4, 2))
    draw_bins(bins[[b]], est[[b]][1, ], ylim_data)
    points(x, y1, pch = 16, cex = 0.8, col = alpha_col(col_dark, 0.8))
    mtext(sprintf("%d intervals", length(bins[[b]]$lo)), side = 3, line = 0.3,
          cex = 0.95, col = col_dark)
  }
}, width = 6, height = 2.3)

# The sampling distribution of each estimator --------------------------------------
ylim_est <- c(-3.6, 3.6)
fig_estimates <- function(name, b, e, stats) {
  stopifnot(all(e > ylim_est[1] & e < ylim_est[2]))
  fig_pdf(name, {
    axes_xy(ylim_est, -3:3)
    abline(v = b$cuts, col = col_grey, lty = 2, lwd = 0.8)
    k <- length(b$lo)
    segments(rep(b$lo, each = S), as.vector(e), rep(b$hi, each = S),
             as.vector(e), col = alpha_col(col_cardinal, 0.025), lwd = 0.8)
    lines(xg, m(xg), col = col_dark, lwd = 2.2)
    segments(b$lo, colMeans(e), b$hi, colMeans(e), col = pal[["blue"]], lwd = 3)
    legend(xlim[1] + 0.008, ylim_est[2] - 0.05, bty = "n", cex = 0.85,
           lwd = c(2.2, 1.5, 3), col = c(col_dark, col_cardinal, pal[["blue"]]),
           legend = c("true E[Y|X]",
                      sprintf("estimates, one per data set (%s)",
                              format(S, big.mark = ",")),
                      "average of the estimates"))
    mtext(sprintf("Averaged over the %d observations:  bias\u00b2 = %.2f,  variance = %.2f",
                  n, stats[["bias2"]], stats[["var"]]),
          side = 3, line = 0.15, adj = 1, cex = 0.85, col = col_dark)
  }, mar = c(3.2, 3.6, 1.4, 0.8))
}
fig_estimates("bv_bias_4bins", bins$k4, est$k4, bv[, "k4"])
fig_estimates("bv_var_8bins",  bins$k8, est$k8, bv[, "k8"])
