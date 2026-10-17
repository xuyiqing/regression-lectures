# fig_sampling.R --- simulated sampling distributions of the OLS estimators
# (Lecture 4-2, pp. 13-14, 20, 26, 28). The confidence-interval figures come
# from 04-3's copy of this script, which runs the same simulation (seed 2024).
#
# Run from the deck folder:  Rscript code/fig_sampling.R
#
# Design (as on the slides): Y = beta0 + beta1 X + u with beta0 = 5,
# beta1 = -1, u ~ N(0, 4) (sd 2); n = 25 X values drawn once from
# Unif(0, 10) and then held fixed; 1,000 simulated samples (seed 2024).
# The first 100 samples also give the 95% confidence intervals. The seed was
# picked so that the build-up slides start with two intervals that clearly
# cover; the miss counts are whatever this draw gives.
# The two "intuition" figures (pp. 26, 28) use Y = 0.5 + 0.8 X + u, n = 20,
# and 30 simulated samples per panel (seed 58 and 60).
#
# Writes figs/sim_*.pdf, figs/var_*.pdf.
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

# p. 13: fitted lines and the joint distribution
fig_pdf("sim_lines_joint", mar = c(3.2, 3.6, 2.0, 0.8), {
  par(mfrow = c(1, 2), cex = 1)
  plot(NA, xlim = xl_line, ylim = yl_line, xaxs = "i", xlab = "X", ylab = "Y")
  for (r in 1:100) abline(B[r, 1], B[r, 2], col = alpha_col(col_cardinal, 0.25), lwd = 0.8)
  abline(beta0, beta1, col = col_dark, lwd = 2, lty = 2)
  legend("topright", bty = "n", cex = 0.8, lty = c(1, 2), lwd = c(1.2, 2),
         col = c(col_cardinal, col_dark),
         legend = c("fitted lines", expression("true line " * 5 - X)))
  mtext("fitted lines from 100 samples", side = 3, line = 0.5, cex = 0.9)
  plot(B[, 1], B[, 2], pch = 16, cex = 0.45, col = alpha_col(col_dark, 0.35),
       xlab = expression(hat(beta)[0]), ylab = expression(hat(beta)[1]))
  points(beta0, beta1, pch = 3, col = col_cardinal, cex = 2, lwd = 2.5)
  lab <- expression("true " * (list(beta[0], beta[1])) == (list(5, -1)))
  usr <- par("usr")
  lx <- usr[2] - 0.04 * diff(usr[1:2]); ly <- usr[4] - 0.06 * diff(usr[3:4])
  w <- strwidth(lab, cex = 0.85); h <- strheight(lab, cex = 0.85)
  rect(lx - w - 0.1, ly - h / 2 - 0.03, lx + 0.1, ly + h / 2 + 0.03, col = "white", border = NA)
  text(lx, ly, lab, adj = c(1, 0.5), col = col_cardinal, cex = 0.85)
  segments(lx - w / 2, ly - h / 2 - 0.03, beta0 + 0.05, beta1 + 0.03, col = col_cardinal, lwd = 1)
  mtext(expression("joint distribution of " * (list(hat(beta)[0], hat(beta)[1])) *
                     ", 1,000 samples"), side = 3, line = 0.5, cex = 0.9)
})

# p. 14: the two marginal distributions
fig_pdf("sim_marginals", mar = c(3.4, 3.8, 2.0, 0.8), {
  par(mfrow = c(1, 2), cex = 1)
  histo <- function(v, truth, lab, title, breaks) {
    h <- hist(v, breaks = breaks, plot = FALSE)
    plot(h, col = alpha_col(pal[["blue"]], 0.35), border = "white", main = "",
         xlab = lab, ylab = "number of samples", ylim = c(0, max(h$counts) * 1.15))
    segments(truth, 0, truth, max(h$counts) * 1.07, col = col_cardinal, lwd = 2.2)
    mtext(title, side = 3, line = 0.5, cex = 0.9)
    text(truth, max(h$counts) * 1.11, "true value", col = col_cardinal, cex = 0.8)
  }
  histo(B[, 1], beta0, expression(hat(beta)[0]),
        expression("sampling distribution of " * hat(beta)[0]), seq(1, 9, by = 0.4))
  histo(B[, 2], beta1, expression(hat(beta)[1]),
        expression("sampling distribution of " * hat(beta)[1]), seq(-1.7, -0.3, by = 0.07))
})


# p. 20: what Var[beta1-hat] measures
fig_pdf("var_beta1", width = 6, height = 2.6, mar = c(2.6, 1.5, 0.6, 0.6), {
  z <- seq(-3.6, 3.6, length.out = 400)
  plot(z, dnorm(z), type = "n", axes = FALSE, xlab = "", ylab = "",
       ylim = c(0, 0.45), xaxs = "i", yaxs = "i")
  polygon(c(z, rev(z)), c(dnorm(z), rep(0, length(z))),
          col = alpha_col(pal[["blue"]], 0.18), border = NA)
  lines(z, dnorm(z), lwd = 2.2, col = col_dark)
  segments(0, 0, 0, dnorm(0), lty = 2, col = col_grey)
  arrows(-1, dnorm(1), 1, dnorm(1), code = 3, length = 0.08, col = col_cardinal, lwd = 2)
  text(1.15, dnorm(1) + 0.01, expression("spread: " * Var(hat(beta)[1])),
       adj = c(0, 0), col = col_cardinal)
  axis(1, at = 0, labels = expression(beta[1]), tcl = -0.3, cex.axis = 1.1)
  abline(h = 0, col = col_dark)
  mtext(expression(hat(beta)[1]), side = 1, line = 1.4, at = 3.2)
  text(-3.5, 0.43, expression("sampling distribution (PDF) of " * hat(beta)[1]),
       adj = c(0, 1), cex = 0.9)
  text(0.1, 0.02, expression("centered at " * beta[1] * " (unbiased)"),
       adj = c(0, 0), cex = 0.8, col = col_grey)
})

# pp. 26, 28: why the error variance and the spread of X matter
intuition <- function(x_left, x_right, sd_left, sd_right, titles, seed, xlim, ylim) {
  set.seed(seed)
  par(mfrow = c(1, 2), cex = 1, mar = c(3.0, 3.2, 2.4, 0.6))
  for (side in 1:2) {
    xx <- if (side == 1) x_left else x_right
    s <- if (side == 1) sd_left else sd_right
    plot(NA, xlim = xlim, ylim = ylim, xlab = "X", ylab = "Y")
    for (r in 1:30) {
      yy <- 0.5 + 0.8 * xx + rnorm(length(xx), 0, s)
      cf <- coef(lm(yy ~ xx))
      abline(cf[1], cf[2], col = alpha_col(col_cardinal, 0.3), lwd = 0.8)
      if (r == 1) keep <- yy
    }
    points(xx, keep, pch = 16, cex = 0.55, col = col_dark)
    abline(0.5, 0.8, lty = 2, lwd = 2, col = col_dark)
    if (side == 2) legend("topleft", bty = "n", cex = 0.75,
      legend = c("one sample", "OLS fits, 30 samples", "true line"),
      pch = c(16, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 1, 2),
      col = c(col_dark, col_cardinal, col_dark))
    sdb <- s / sqrt(sum((xx - mean(xx))^2))
    mtext(titles[[side]], side = 3, line = 1.0, cex = 0.9)
    mtext(bquote({SD(hat(beta)[1]) == sigma[u] / sqrt(SST[x])} == .(fmt(sdb, 2))),
          side = 3, line = 0.05, cex = 0.8, col = col_cardinal)
  }
}
set.seed(20)
x20 <- rnorm(20)
fig_pdf("var_sigma", width = 6, height = 3.0,
        intuition(x20, x20, 1, 3,
                  list(expression("low error variance, " * sigma[u] == 1),
                       expression("high error variance, " * sigma[u] == 3)),
                  seed = 58, xlim = c(-2.6, 2.6), ylim = c(-9, 9)))
fig_pdf("var_spreadx", width = 6, height = 3.0,
        intuition(x20 * 0.5, x20 * 2, 1.5, 1.5,
                  list(bquote("X less spread out, " * SD(X) == .(fmt(sd(x20 * 0.5), 2))),
                       bquote("X more spread out, " * SD(X) == .(fmt(sd(x20 * 2), 2)))),
                  seed = 60, xlim = c(-5, 5), ylim = c(-7, 8)))

cat(sprintf("x values (fixed, n = %d): %s\n", n, paste(round(x, 2), collapse = " ")))
cat(sprintf("mean of %d estimates: b0 = %.3f, b1 = %.3f\n", R, mean(B[, 1]), mean(B[, 2])))
