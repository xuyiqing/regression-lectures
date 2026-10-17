# fig_multicollinearity.R --- Lecture 5.2, Multicollinearity.
#
# Feeds two slides:
#   "Multicollinearity: Simulation Example"   table typed from the printed output
#                                             (also saved to code/fig_multicollinearity_table.txt)
#   "Variance Inflation due to Multicollinearity (k=2)"
#                                             figs/multicollinearity_variance.pdf
#
# Design. n = 50. The regressors are held fixed across samples, as in the
# formula V[beta1_hat | X] = sigma_u^2 / (SST_1 (1 - R_1^2)): X1 and X2 have
# sample mean 0, sample variance 1 and sample correlation rho, so R_1^2 (from
# regressing X1 on X2) equals rho^2 exactly and SST_1 = n - 1 in every column.
# Only the error term is redrawn:
#   Y = 1 + 1 * X1 + 1 * X2 + u,   u ~ N(0, 1),   10,000 samples per R_1^2 value.
# The same 10,000 error draws are reused for every R_1^2 value, so the columns
# differ only in R_1^2.
#
# Run from the deck folder:  Rscript code/fig_multicollinearity.R

source("../course_figures.R")
set.seed(4502)

n     <- 50
reps  <- 10000
r2    <- c(0, 0.5, 0.75, 0.9, 0.99)   # values of R_1^2 in the table
beta  <- c(1, 1, 1)                   # beta_0, beta_1, beta_2
sigma <- 1                            # sd of u

# Two fixed columns with sample mean 0, sample variance 1, sample correlation 0.
z <- scale(matrix(rnorm(2 * n), n, 2))
z[, 2] <- scale(resid(lm(z[, 2] ~ z[, 1])))

U <- matrix(rnorm(n * reps, 0, sigma), n, reps)   # one column per sample

sim <- lapply(r2, function(R2) {
  rho <- sqrt(R2)
  x1  <- z[, 1]
  x2  <- rho * z[, 1] + sqrt(1 - rho^2) * z[, 2]
  X   <- cbind(1, x1, x2)
  Y   <- drop(X %*% beta) + U                      # n x reps
  B   <- solve(crossprod(X), crossprod(X, Y))      # 3 x reps: OLS in every sample
  list(b1   = B[2, ],
       R2   = summary(lm(x1 ~ x2))$r.squared,
       sst1 = sum((x1 - mean(x1))^2))
})

se_sim  <- sapply(sim, function(s) sd(s$b1))
tab <- data.frame(
  R2_target  = r2,
  R2_sample  = sapply(sim, `[[`, "R2"),
  mean_b1    = sapply(sim, function(s) mean(s$b1)),
  SE_sim     = se_sim,
  SE_formula = sigma / sqrt(sapply(sim, `[[`, "sst1") * (1 - r2)),
  ratio_sim  = se_sim / se_sim[1],
  sqrt_VIF   = 1 / sqrt(1 - r2),
  var_ratio  = (se_sim / se_sim[1])^2,
  VIF        = 1 / (1 - r2)
)
out <- capture.output(print(format(tab, digits = 4), row.names = FALSE))
writeLines(c("fig_multicollinearity.R --- numbers for the slide 'Multicollinearity: Simulation Example'",
             sprintf("n = %d, samples per column = %d, beta = (%s), sd(u) = %g, seed = 4502",
                     n, reps, paste(beta, collapse = ", "), sigma),
             "SE_sim = standard deviation of beta1_hat across samples; ratio_sim = SE_sim / SE_sim at R2 = 0",
             "", out, "",
             "As typed on the slide:",
             paste("SE    :", paste(sprintf("%.3f", se_sim), collapse = "  ")),
             paste("Ratio :", paste(sprintf("%.2f", se_sim / se_sim[1]), collapse = "  ")),
             paste("sqrtVIF:", paste(sprintf("%.2f", 1 / sqrt(1 - r2)), collapse = "  "))),
           "code/fig_multicollinearity_table.txt")
cat(readLines("code/fig_multicollinearity_table.txt"), sep = "\n")

# ---------------------------------------------------------------- figure
# Left panel: the simulated sampling distribution of beta1_hat at R_1^2 = 0, .9, .99
# (all centred on the true beta_1 = 1). Right panel: the variance of beta1_hat
# relative to R_1^2 = 0. The line is the formula 1 / (1 - R_1^2) (the VIF); the
# circles are the simulated ratios (SE_sim / SE_sim[1])^2 for the five table
# columns; the labels give the formula's value at those points.
show <- c(1, 4, 5)
cols <- c(col_grey, pal[["blue"]], col_cardinal)
xlim <- c(-3, 5)

fig_pdf("multicollinearity_variance", width = 6, height = 3.4,
        mar = c(3.2, 3.4, 2.1, 0.6), {
  layout(matrix(1:2, 1))

  dens <- lapply(sim[show], function(s)
    density(s$b1, adjust = 1.5, from = xlim[1], to = xlim[2], n = 512))
  plot(NA, xlim = xlim, ylim = c(0, 3), xaxs = "i", yaxs = "i",
       xlab = expression(hat(beta)[1]), ylab = "Density")
  mtext(expression("Sampling distribution of " * hat(beta)[1]), side = 3,
        line = 0.5, adj = 0, cex = 0.95)
  abline(v = beta[2], lty = 2, col = col_dark)
  for (k in seq_along(show)) lines(dens[[k]], col = cols[k], lwd = 2)
  text(beta[2] - 0.12, 2.8, expression("true " * beta[1]), adj = 1, col = col_dark)
  text(1.45, 2.2, expression(R[1]^2 == 0), col = cols[1], adj = 0)
  text(1.75, 0.95, expression(R[1]^2 == 0.9), col = cols[2], adj = 0)
  text(2.45, 0.42, expression(R[1]^2 == 0.99), col = cols[3], adj = 0)

  g <- seq(0, 0.99, length.out = 400)
  plot(g, 1 / (1 - g), type = "l", lwd = 2, col = col_cardinal,
       xlim = c(0, 1), ylim = c(0, 110), xaxs = "i", yaxs = "i",
       xlab = expression(R[1]^2), ylab = expression("Variance ratio" == "(SE ratio)"^2))
  mtext(expression(Var(hat(beta)[1]) * " relative to " * R[1]^2 == 0), side = 3,
        line = 0.5, adj = 0, cex = 0.95)
  abline(v = 1, lty = 3, col = col_grey)
  points(r2, tab$var_ratio, pch = 21, bg = "white", col = col_dark, cex = 1.1, lwd = 1.2,
         xpd = NA)                                        # end points sit on the panel edges
  lab <- paste0("\u00d7", formatC(round(tab$VIF), format = "d"))   # "x2", "x4", ...
  text(r2[2:3], tab$var_ratio[2:3] + 8, lab[2:3], cex = 0.85)            # above the points
  text(r2[4] - 0.04, tab$var_ratio[4] + 10, lab[4], cex = 0.85)           # above-left
  text(r2[5] - 0.035, tab$var_ratio[5], lab[5], adj = c(1, 0.5), cex = 0.85)
  legend("topleft", inset = c(0.02, 0.02), bty = "n", cex = 0.85,
         legend = c(expression(VIF == 1 / (1 - R[1]^2)), "simulation"),
         col = c(col_cardinal, col_dark), lwd = c(2, NA), lty = c(1, NA),
         pch = c(NA, 21), pt.bg = "white", pt.cex = 1.1)
})
