# fig_kernel_court.R --- the kernel estimator (Hainmueller, Mummolo and Xu 2019, Section 4.2), as implemented in
# the interflex package, applied to the deck's running example: the marginal effect of democracy D on executive
# corruption Y across high court independence X (V-Dem v16, 2024, 179 countries). Replaces fig_kernel_democracy.R
# (Fish 2002) in v2, 2026-10-11.
#
#   kernel_court   two panels: (a) bandwidth 0.5, chosen by hand; (b) the bandwidth chosen by 10-fold
#                  cross-validation; each with its pointwise 95% bootstrap interval, the linear-interaction
#                  estimate (dashed) and the court independence of each group (rugs). Curves are drawn only
#                  where both groups have data (from the least independent democracy's court score, 0.53, to the
#                  most independent autocracy's, 3.59), as on the marginal-effect slide.
#
# How interflex computes the estimate at an evaluation point x0: a weighted least-squares fit of
#   Y ~ mu + alpha D + eta (X - x0) + beta D (X - x0),
# with Gaussian kernel weights dnorm((X_i - x0) / h(x0)); alpha-hat(x0) is the estimated effect at x0. The
# bandwidth is adaptive: h(x0) = bw * sqrt(g / f(x0)), with f the density of X (R's density()) and g the
# geometric mean of f at the observations, so windows are wider where the data are sparse. This script redoes
# that local fit by hand at all 50 evaluation points (bandwidth 0.5) and checks it against interflex (stopifnot).
#
# Writes code/court_kernel_output.txt (numbers on the slide), code/data/kernel_court_estimates.csv (both curves)
# and code/data/kernel_court_local.csv (the 50 local fits).
#
# Needs interflex >= 1.4.1 (the version pset 5 uses). Run 2026-10-11 with CRAN 1.4.1.
# Run from the deck folder (v2/):  Rscript code/fig_kernel_court.R

source("../course_figures.R")
suppressPackageStartupMessages(library(interflex))
stopifnot(packageVersion("interflex") >= "1.4.1")

x <- read.csv("code/data/countries_xsec.csv")
d <- x[complete.cases(x[, c("execorr100", "courtind", "democracy")]), ]
stopifnot(nrow(d) == 179)
fit_int <- lm(execorr100 ~ courtind * democracy, data = d)
b <- unname(coef(fit_int))
lo_common <- min(d$courtind[d$democracy == 1]); hi_common <- max(d$courtind[d$democracy == 0])
bw_hand <- 0.5

kern <- function(bw) {
  set.seed(4504)
  args <- list(estimator = "kernel", data = d, Y = "execorr100", D = "democracy", X = "courtind",
               vartype = "bootstrap", nboots = 500, figure = FALSE, parallel = FALSE)
  if (!is.null(bw)) args$bw <- bw
  out <- suppressWarnings(suppressMessages(do.call(interflex, args)))
  e <- out$est.kernel[[1]]
  list(bw = out$bw, est = data.frame(X = e[, "X"], TE = e[, "TE"], lower = e[, "lower CI(95%)"],
                                      upper = e[, "upper CI(95%)"]), cv = out$CV.output)
}
k_hand <- kern(bw_hand)
k_cv <- kern(NULL)
cv <- k_cv$cv

# ---- the local fit by hand at every evaluation point (bandwidth 0.5)
dens <- density(d$courtind)
f_at <- function(v) dens$y[vapply(v, function(u) which.min(abs(dens$x - u)), 1L)]
g <- exp(mean(log(f_at(d$courtind))))
x0s <- k_hand$est$X                         # interflex's 50 evaluation points
local <- do.call(rbind, lapply(x0s, function(x0) {
  h <- bw_hand * sqrt(g / f_at(x0))
  w <- dnorm((d$courtind - x0) / h)
  dx <- d$courtind - x0
  cf <- coef(lm(execorr100 ~ democracy + dx + democracy:dx, data = d, weights = w))
  data.frame(x0 = x0, h = h, mu = cf[["(Intercept)"]], alpha = cf[["democracy"]], eta = cf[["dx"]],
             beta = cf[["democracy:dx"]], n_eff = sum(w)^2 / sum(w^2))
}))
stopifnot(max(abs(local$alpha - k_hand$est$TE)) < 1e-8)       # the hand fit reproduces interflex

out <- "code/court_kernel_output.txt"
sink(out)
cat("Kernel estimator, interflex", as.character(packageVersion("interflex")), "| Y execorr100, D democracy, X courtind, N", nrow(d), "\n")
cat("Linear interaction model: effect of democracy =", round(b[3], 3), "+", round(b[4], 3), "x court\n")
cat(sprintf("Common range (both groups have data): %.3f to %.3f\n", lo_common, hi_common))
cat(sprintf("Bandwidth: by hand %.2f ; cross-validated %.4f (grid of %d from %.4f to %.4f)\n",
            k_hand$bw, k_cv$bw, nrow(cv), min(cv$bw), max(cv$bw)))
cat("CV loss (10-fold, metric", cv$CV.metric.used[1], ") by bandwidth:\n")
print(round(data.frame(bw = cv$bw, loss = cv$CV.loss.used, se = cv$CV.loss.se.used, selected = cv$CV.selected), 4), row.names = FALSE)
cat("Hand fit reproduces interflex at all 50 points: max |diff| =", signif(max(abs(local$alpha - k_hand$est$TE)), 3), "\n")
cat("Three local fits (bandwidth 0.5):\n"); print(round(local[c(13, 26, 39), ], 4), row.names = FALSE)
show <- function(k, lab) {
  e <- k$est[k$est$X >= lo_common & k$est$X <= hi_common, ]
  cat("\n", lab, " (bw ", round(k$bw, 3), "), points inside the common range:\n", sep = "")
  e$linear <- b[3] + b[4] * e$X
  print(round(e, 2), row.names = FALSE)
}
show(k_hand, "By hand"); show(k_cv, "Cross-validated")
sink()

est <- rbind(cbind(fit = "hand", k_hand$est), cbind(fit = "cv", k_cv$est))
est$bw <- ifelse(est$fit == "hand", k_hand$bw, k_cv$bw)
write.csv(est, "code/data/kernel_court_estimates.csv", row.names = FALSE)
write.csv(local, "code/data/kernel_court_local.csv", row.names = FALSE)

# ---- figure
col_d <- pal[["blue"]]; col_a <- col_cardinal
minus <- function(v) sub("^-", "−", v)
inside <- function(e) e[e$X >= lo_common & e$X <= hi_common, ]
yl <- c(-90, 30)
panel <- function(k, main) {
  e <- inside(k$est)
  plot(NA, xlim = c(0, 4), ylim = yl, xaxs = "i", axes = FALSE, xlab = "Court independence (0–4)",
       ylab = "Effect of democracy on corruption", main = main, cex.main = 0.8, cex.lab = 0.85, font.main = 1)
  axis(1, at = 0:4); axis(2, at = seq(-75, 25, 25), labels = minus(seq(-75, 25, 25)))
  polygon(c(e$X, rev(e$X)), pmax(pmin(c(e$lower, rev(e$upper)), yl[2]), yl[1]),
          col = alpha_col(col_cardinal, 0.18), border = NA)
  abline(h = 0, col = col_grey, lty = 2)
  segments(lo_common, b[3] + b[4] * lo_common, hi_common, b[3] + b[4] * hi_common, col = col_dark, lty = 2, lwd = 1.4)
  lines(e$X, e$TE, col = col_cardinal, lwd = 2.2)
  rug(d$courtind[d$democracy == 0], ticksize = 0.035, col = col_a, lwd = 0.7, side = 1)
  rug(d$courtind[d$democracy == 1], ticksize = 0.035, col = col_d, lwd = 0.7, side = 3)
}
fig_pdf("kernel_court", width = 5.0, height = 2.8, mar = c(3.2, 3.4, 1.6, 0.4), {  # two thirds of the slide width
  par(mfrow = c(1, 2))
  panel(k_hand, sprintf("(a) bandwidth %.1f (by hand)", k_hand$bw))
  panel(k_cv, sprintf("(b) bandwidth %.2f (cross-validated)", k_cv$bw))
})
cat("fig_kernel_court: done; numbers in", out, "\n")
