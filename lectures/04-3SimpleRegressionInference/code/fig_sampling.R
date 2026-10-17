# fig_sampling.R --- simulated sampling distributions of the OLS estimators
# (Lecture 4-3, pp. 21-24). The figures of the sampling distributions and the
# variance come from 04-2's copy of this script, which runs the same simulation (seed 2024).
#
# Run from the deck folder:  Rscript code/fig_sampling.R
#
# Design (as on the slides): Y = beta0 + beta1 X + u with beta0 = 5,
# beta1 = -1, u ~ N(0, 4) (sd 2); n = 25 X values drawn once from
# Unif(0, 10) and then held fixed; 1,000 simulated samples (seed 2024).
# The first 100 samples also give the 95% confidence intervals. The seed was
# picked so that the build-up slides start with two intervals that clearly
# cover; the miss counts are whatever this draw gives.
#
# Writes figs/ci_sim_*.pdf.
# Lecture 4 was one deck until 2026-10-09, when it was split into 4-1, 4-2 and 4-3.

source("../course_figures.R")

fmt <- function(v, d = 2) sub("^-", "\u2212", formatC(v, format = "f", digits = d))

# ------------------------------------------------------------- simulation
set.seed(2024)
n <- 25; beta0 <- 5; beta1 <- -1; sigma <- 2; R <- 1000
x <- sort(runif(n, 0, 10))
X <- cbind(1, x)
XtXi <- solve(crossprod(X))
tcrit <- qt(0.975, df = n - 2)
sims <- vector("list", R)
for (r in seq_len(R)) {
  y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
  b <- drop(XtXi %*% crossprod(X, y))
  e <- y - drop(X %*% b)
  se <- sqrt(diag(XtXi) * sum(e^2) / (n - 2))
  sims[[r]] <- list(y = y, b = b, lo = b - tcrit * se, hi = b + tcrit * se)
}
B <- t(vapply(sims, function(s) s$b, numeric(2)))
LO <- t(vapply(sims, function(s) s$lo, numeric(2)))
HI <- t(vapply(sims, function(s) s$hi, numeric(2)))
# check against lm() for the first sample
chk <- confint(lm(sims[[1]]$y ~ x))
stopifnot(isTRUE(all.equal(unname(chk[, 1]), unname(LO[1, ]))),
          isTRUE(all.equal(unname(chk[, 2]), unname(HI[1, ]))))

xl_line <- c(0, 10); yl_line <- c(-10, 11)


# pp. 21-24: confidence intervals, built up sample by sample
xl_ci <- list(c(0.5, 9.5), c(-1.8, -0.2))
ci_panel <- function(j, k, colored = FALSE, show_axis_title = TRUE, rows = 100) {
  truth <- c(beta0, beta1)[j]
  ylim <- if (rows > 5) c(-2, 101.5) else c(0.3, rows + 0.7)
  plot(NA, xlim = xl_ci[[j]], ylim = ylim, yaxs = "i", yaxt = "n",
       xlab = if (j == 1) expression(hat(beta)[0]) else expression(hat(beta)[1]),
       ylab = if (show_axis_title) "sample" else "")
  axis(2, at = if (rows > 5) c(1, 25, 50, 75, 100) else seq_len(rows))
  miss <- LO[1:k, j] > truth | HI[1:k, j] < truth
  cols <- if (colored) ifelse(miss, col_cardinal, pal[["blue"]]) else rep(col_dark, k)
  lw <- if (colored) ifelse(miss, 2.2, 1.1) else rep(1.1, k)
  if (k <= 5) {                      # build-up steps: newest interval stands out
    cols[k] <- pal[["blue"]]; lw <- rep(2.4, k)
  }
  segments(LO[1:k, j], 1:k, HI[1:k, j], 1:k, col = cols, lwd = lw, lend = 1)
  points(B[1:k, j], 1:k, pch = 16, cex = 0.3, col = if (colored) cols else col_dark)
  abline(v = truth, lty = 2, col = col_grey, lwd = 1.2)
  invisible(sum(miss))
}

ci_step <- function(k) {
  par(mfrow = c(1, 3), cex = 1, mar = c(3.2, 3.4, 1.8, 0.8))
  y <- sims[[k]]$y
  plot(x, y, pch = 16, cex = 0.7, col = col_dark, xlim = xl_line, ylim = yl_line,
       xaxs = "i", xlab = "X", ylab = "Y")
  abline(beta0, beta1, lty = 2, col = col_grey, lwd = 1.5)
  abline(B[k, 1], B[k, 2], col = col_cardinal, lwd = 2)
  rug(x, col = col_dark)
  legend("topright", bty = "n", cex = 0.75, lty = c(1, 2), lwd = c(2, 1.5),
         col = c(col_cardinal, col_grey), legend = c("fitted", "true"))
  mtext(sprintf("sample %d", k), side = 3, line = 0.4, cex = 0.9)
  ci_panel(1, k, rows = 5)
  mtext(expression("95% CI for " * beta[0]), side = 3, line = 0.4, cex = 0.9)
  ci_panel(2, k, show_axis_title = FALSE, rows = 5)
  mtext(expression("95% CI for " * beta[1]), side = 3, line = 0.4, cex = 0.9)
}
fig_pdf("ci_sim_step1", width = 6, height = 2.45, ci_step(1))
fig_pdf("ci_sim_step2", width = 6, height = 2.45, ci_step(2))

miss_count <- c(sum(LO[1:100, 1] > beta0 | HI[1:100, 1] < beta0),
                sum(LO[1:100, 2] > beta1 | HI[1:100, 2] < beta1))
ci_all <- function(colored) {
  par(mfrow = c(1, 2), cex = 1, mar = c(3.2, 3.6, 2.0, 0.8))
  for (j in 1:2) {
    ci_panel(j, 100, colored = colored, show_axis_title = (j == 1))
    title_j <- if (j == 1) expression("95% CIs for " * beta[0] == 5) else
      expression("95% CIs for " * beta[1] == -1)
    mtext(title_j, side = 3, line = if (colored) 0.9 else 0.5, cex = 0.9)
    if (colored) mtext(sprintf("%d of 100 intervals miss the true value", miss_count[j]),
                       side = 3, line = 0.05, cex = 0.8, col = col_cardinal)
  }
}
fig_pdf("ci_sim_many", ci_all(FALSE))
fig_pdf("ci_sim_cover", ci_all(TRUE))

cat(sprintf("x values (fixed, n = %d): %s\n", n, paste(round(x, 2), collapse = " ")))
cat(sprintf("CI misses in first 100 samples: beta0 %d, beta1 %d\n", miss_count[1], miss_count[2]))
