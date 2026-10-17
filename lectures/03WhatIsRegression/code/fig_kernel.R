# fig_kernel.R --- Lecture 3, kernel regression and local linear regression
# on a random sample of 200 US counties: 2024 Republican share of the
# two-party presidential vote (%) against log population density (people per
# square mile). Every figure shares the same axes, so each run of steps plays
# as an animation.
#
# Data: code/data/county_pres.csv, copied from lectures/_data/us_elections/
# (MEDSL precinct returns aggregated to counties; ACS 2019-23 population; 2023
# Census Gazetteer land area). 3,034 counties have a 2024 share (Alaska,
# Connecticut and Louisiana are not in the file, and 7 other counties lack
# usable returns). The sample is drawn with set.seed(450), the same draw as in
# fig_regression.R and on the slides; with all 3,034 counties the bandwidth
# barely matters, which is why the kernel slides use the sample.
#
#   figs/county_scatter.pdf         the 200 sampled counties
#   figs/kernel_unif_1..3.pdf       uniform kernel, h = 0.5, x0 = 3.0, 3.5, 4.0
#   figs/kernel_unif_4.pdf          the whole uniform-kernel curve, h = 0.5
#   figs/kernel_unif_wide_1..3.pdf  uniform kernel, h = 1.5, x0 = 2, 4, 6
#   figs/kernel_unif_both.pdf       whole curves for h = 0.5 and h = 1.5
#   figs/kernel_epa_1..3.pdf        Epanechnikov kernel, h = 1.5, x0 = 2, 4, 6
#   figs/kernel_epa_4.pdf           the whole Epanechnikov curve, h = 1.5
#   figs/kernel_epa_bw.pdf          Epanechnikov curves for h = 0.5, 1.5, 3
#   figs/loclin_1..3.pdf            local linear fits, Epanechnikov, h = 1.5,
#                                   x0 = 2, 4, 6
#
# The kernels are the ones on the slides: K(u) = 1/2 and K(u) = 3/4 (1 - u^2)
# for |u| <= 1, zero otherwise, with u = (X_i - x0) / h.
# Numbers printed by this script go to code/kernel_output.txt.
#
# Run from the deck folder:  Rscript --vanilla code/fig_kernel.R

source("../course_figures.R")

counties <- read.csv("code/data/county_pres.csv", colClasses = c(fips = "character"))
counties$rep24 <- 100 * counties$rep2p_2024
counties <- subset(counties, !is.na(rep24))
stopifnot(nrow(counties) == 3034, all(is.finite(counties$log_density)))
set.seed(450)
smp <- counties[sample(nrow(counties), 200), ]
x <- smp$log_density
y <- smp$rep24
sink("code/kernel_output.txt", split = TRUE)
ext <- counties[c(which.min(counties$density), which.max(counties$density)), ]
cat(sprintf("all %d counties: density from %.3f (%s, %s) to %.0f (%s, %s) people per sq. mile; log %.2f to %.2f\n",
            nrow(counties), ext$density[1], ext$county[1], ext$state[1], ext$density[2],
            ext$county[2], ext$state[2], ext$log_density[1], ext$log_density[2]))
cat(sprintf("log density 0 = 1 person, %.1f = 100, %.1f = 1,000 people per sq. mile\n", log(100), log(1000)))
cat(sprintf("sample of 200 counties (seed 450): log density %.2f to %.2f; share %.1f to %.1f\n",
            min(x), max(x), min(y), max(y)))

k_unif <- function(u) ifelse(abs(u) <= 1, 1 / 2, 0)
k_epa  <- function(u) ifelse(abs(u) <= 1, 3 / 4 * (1 - u^2), 0)

# u is rounded so that a point exactly h away from x0 counts as inside the
# window (|u| <= 1), whatever the floating-point error in x - x0.
kern_w <- function(x0, h, K) K(round((x - x0) / h, 10))

local_mean <- function(x0, h, K) {
  w <- kern_w(x0, h, K)
  if (sum(w) == 0) NA_real_ else sum(w * y) / sum(w)
}
local_line <- function(x0, h, K) {
  w <- kern_w(x0, h, K)
  keep <- w > 0
  stopifnot(length(unique(x[keep])) >= 2)
  coef(lm(y ~ I(x - x0), weights = w, subset = keep))   # intercept = fit at x0
}

grid <- round(seq(-0.92, 7.81, by = 0.01), 2)
curve_mean <- function(h, K) vapply(grid, local_mean, numeric(1), h = h, K = K)

# Shared frame ---------------------------------------------------------------
xlim <- c(-1.3, 8.2)
ylim <- c(0, 108)
strip_base <- 2       # zero weight
strip_top  <- 16      # the largest weight a kernel can give (3/4)
strip_y <- function(w) strip_base + (strip_top - strip_base) * w / 0.75
strip_line <- 19
stopifnot(min(y) > strip_line + 2, max(y) < 100)
col_h <- c(main = col_cardinal, wide = pal[["blue"]], narrow = pal[["orange"]])
xlab_d <- "log population density (people per sq. mile)"
ylab_r <- "Republican % of two-party vote, 2024"

frame <- function(strip = FALSE, yl = ylim) {
  plot(NA, xlim = xlim, ylim = yl, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = xlab_d, ylab = ylab_r)
  axis(1, at = seq(0, 8, 2))
  axis(2, at = seq(20, 100, 20))
  if (strip) {
    abline(h = strip_line, col = col_light, lty = 3)
    mtext("weight", side = 2, line = 0.3, at = strip_y(0.3), las = 1,
          adj = 1, cex = 0.72, col = col_grey)
  }
}
all_points <- function() points(x, y, pch = 1, cex = 0.85, col = col_grey)
note <- function(txt) text(xlim[1] + 0.2, ylim[2] - 4, txt,
                           adj = c(0, 0.5), cex = 0.9, col = col_dark)

# One step: the window around x0, the kernel weights, and the estimates so far.
# kind is "uniform" or "epa"; line = TRUE fits a weighted line (local linear).
step <- function(x0, h, kind, done = numeric(0), col = col_h[["main"]],
                 label = "", line = FALSE) {
  K <- if (kind == "uniform") k_unif else k_epa
  frame(strip = TRUE)
  rect(max(x0 - h, xlim[1]), strip_line, min(x0 + h, xlim[2]), ylim[2],
       col = alpha_col(col_grey, 0.10), border = NA)
  # the kernel weight function, drawn in the strip under the data
  uu <- seq(-1, 1, length.out = 201)
  xs <- x0 + h * uu
  ok <- xs >= xlim[1] & xs <= xlim[2]
  xk <- xs[ok]
  yk <- strip_y(K(uu[ok]))
  polygon(c(xk[1], xk, xk[length(xk)]), c(strip_base, yk, strip_base),
          col = alpha_col(col_grey, 0.25), border = NA)
  lines(c(xlim[1], xk[1]), rep(strip_base, 2), col = col_grey, lwd = 1.5)
  lines(xk, yk, col = col_grey, lwd = 1.5)
  lines(c(xk[length(xk)], xlim[2]), rep(strip_base, 2), col = col_grey, lwd = 1.5)
  if (kind == "uniform") {          # the box has vertical sides
    for (e in c(x0 - h, x0 + h)) if (e >= xlim[1] && e <= xlim[2])
      segments(e, strip_base, e, strip_y(1 / 2), col = col_grey, lwd = 1.5)
  }
  # the data: points inside the window are filled, sized by their weight
  all_points()
  w <- kern_w(x0, h, K)
  inside <- w > 0
  size <- if (kind == "uniform") rep(1.05, sum(inside)) else
    0.45 + 1.15 * w[inside] / 0.75
  points(x[inside], y[inside], pch = 16, cex = size, col = col_dark)
  # the estimate at x0 and at the earlier x0s
  if (line) {
    b <- local_line(x0, h, K)
    xl <- c(max(x0 - h, xlim[1]), min(x0 + h, xlim[2]))
    clip(xlim[1], xlim[2], strip_line, ylim[2])   # keep the line out of the weight strip
    lines(xl, b[1] + b[2] * (xl - x0), col = col, lwd = 2.2)
    do.call(clip, as.list(par("usr")))
    est <- unname(b[1])
    prev <- vapply(done, function(z) unname(local_line(z, h, K)[1]), numeric(1))
  } else {
    est <- local_mean(x0, h, K)
    prev <- vapply(done, local_mean, numeric(1), h = h, K = K)
  }
  segments(x0, strip_top + 1, x0, est, col = col, lty = 2, lwd = 1)
  if (length(done)) points(done, prev, pch = 21, bg = col, col = "white", cex = 1.4)
  points(x0, est, pch = 21, bg = col, col = "white", cex = 2.0, lwd = 1.2)
  note(label)
  invisible(est)
}

lab_step <- function(kname, h, x0)
  bquote(.(paste(kname, "kernel,")) ~~ h == .(sprintf("%.1f", h)) * "," ~~
           x[0] == .(sprintf("%.1f", x0)))

# Scatter --------------------------------------------------------------------
fig_pdf("county_scatter", {
  frame(yl = c(15, 100))        # standalone figure: no room kept for weights
  points(x, y, pch = 16, cex = 0.95, col = alpha_col(col_dark, 0.8))
})

# Uniform kernel, narrow window -----------------------------------------------
hn <- 0.5; hw <- 1.5
x0s <- c(3.0, 3.5, 4.0)
for (i in seq_along(x0s)) {
  fig_pdf(paste0("kernel_unif_", i),
          cat(sprintf("uniform h=%.1f x0=%.1f estimate=%.2f\n", hn, x0s[i],
                      step(x0s[i], hn, "uniform", done = x0s[seq_len(i - 1)],
                           label = lab_step("uniform", hn, x0s[i])))))
}
m_unif_n <- curve_mean(hn, k_unif)
m_unif_w <- curve_mean(hw, k_unif)
if (anyNA(m_unif_n)) cat("uniform h=0.5: undefined at", sum(is.na(m_unif_n)),
                         "grid points in", range(grid[is.na(m_unif_n)]), "\n")
stopifnot(!anyNA(m_unif_w))
fig_pdf("kernel_unif_4", {
  frame()
  all_points()
  lines(grid, m_unif_n, col = col_h[["main"]], lwd = 2)
  note(quote("uniform kernel," ~~ h == 0.5 * "," ~~ "fitted wherever the window has data"))
})

# Uniform kernel, wide window --------------------------------------------------
x0w <- c(2, 4, 6)
for (i in seq_along(x0w)) {
  fig_pdf(paste0("kernel_unif_wide_", i),
          cat(sprintf("uniform h=%.1f x0=%.1f estimate=%.2f\n", hw, x0w[i],
                      step(x0w[i], hw, "uniform", done = x0w[seq_len(i - 1)],
                           col = col_h[["wide"]],
                           label = lab_step("uniform", hw, x0w[i])))))
}
# legend in one row under the data (the curve figures have no weight strip)
leg <- function(cols, labs, title) {
  labs <- c(paste0(title, ", ", labs[1]), labs[-1])
  legend(xlim[1] + 0.1, 15, bty = "n", lwd = 2, cex = 0.9, horiz = TRUE,
         col = cols, legend = labs, text.width = strwidth(labs, cex = 0.9) + 0.3)
}
fig_pdf("kernel_unif_both", {
  frame()
  all_points()
  lines(grid, m_unif_n, col = col_h[["main"]], lwd = 2)
  lines(grid, m_unif_w, col = col_h[["wide"]], lwd = 2)
  leg(col_h[c("main", "wide")], c("narrow: h = 0.5", "wide: h = 1.5"), "uniform kernel")
})

# Epanechnikov kernel ------------------------------------------------------------
for (i in seq_along(x0w)) {
  fig_pdf(paste0("kernel_epa_", i),
          cat(sprintf("epanechnikov h=%.1f x0=%.1f estimate=%.2f\n", hw, x0w[i],
                      step(x0w[i], hw, "epa", done = x0w[seq_len(i - 1)],
                           label = lab_step("Epanechnikov", hw, x0w[i])))))
}
hs <- c(0.5, 1.5, 3)
m_epa <- sapply(hs, curve_mean, K = k_epa)
if (anyNA(m_epa[, 1])) cat("epanechnikov h=0.5: undefined at", sum(is.na(m_epa[, 1])),
                           "grid points in", range(grid[is.na(m_epa[, 1])]), "\n")
stopifnot(!anyNA(m_epa[, 2:3]))
fig_pdf("kernel_epa_4", {
  frame()
  all_points()
  lines(grid, m_epa[, 2], col = col_h[["main"]], lwd = 2)
  note(quote("Epanechnikov kernel," ~~ h == 1.5 * "," ~~ "fitted at every" ~ x[0]))
})
fig_pdf("kernel_epa_bw", {
  frame()
  all_points()
  lines(grid, m_epa[, 1], col = col_h[["narrow"]], lwd = 2)
  lines(grid, m_epa[, 2], col = col_h[["main"]], lwd = 2)
  lines(grid, m_epa[, 3], col = col_h[["wide"]], lwd = 2)
  leg(col_h[c("narrow", "main", "wide")], c("h = 0.5", "h = 1.5", "h = 3"),
      "Epanechnikov kernel")
})
rng <- function(v) sprintf("%.1f to %.1f", min(v, na.rm = TRUE), max(v, na.rm = TRUE))
cat("range of the Epanechnikov curves: h=0.5", rng(m_epa[, 1]), "; h=1.5", rng(m_epa[, 2]),
    "; h=3", rng(m_epa[, 3]), "\n")

# Local linear regression ------------------------------------------------------
for (i in seq_along(x0w)) {
  fig_pdf(paste0("loclin_", i),
          cat(sprintf("local linear h=%.1f x0=%.1f estimate=%.2f\n", hw, x0w[i],
                      step(x0w[i], hw, "epa", done = x0w[seq_len(i - 1)],
                           line = TRUE,
                           label = lab_step("Epanechnikov", hw, x0w[i])))))
}
sink()
