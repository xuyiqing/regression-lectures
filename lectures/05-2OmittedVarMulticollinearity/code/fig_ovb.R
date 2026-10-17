# fig_ovb.R --- Lecture 5.2, Omitted Variable Bias.
#
# Feeds the slide "Omitted Variable Bias: Simple Case" that states
#   BIAS[beta1_tilde | X] = E[beta1_tilde | X] - beta_1 = beta_2 * delta_tilde
# with figs/ovb_sim.pdf.
#
# Design. n = 50. The regressors are held fixed across samples (the formula is
# conditional on X). x1 has sample mean 0 and variance 1; x2 = 0.5 * x1 + v,
# where v has sample mean 0, variance 1 and is uncorrelated with x1 in the
# sample, so the slope from regressing x2 on x1 is delta_tilde = 0.5 exactly.
# Only the error term is redrawn:
#   Y = 0 + 1 * x1 + 2 * x2 + u,   u ~ N(0, 2^2),   10,000 samples.
# In every sample we fit the long regression (Y on x1, x2), giving beta1_hat,
# and the short regression (Y on x1 only), giving beta1_tilde. The formula
# predicts that beta1_tilde is centred on beta_1 + beta_2 * delta_tilde = 2,
# a bias of +1, while beta1_hat is centred on beta_1 = 1.
#
# Run from the deck folder:  Rscript code/fig_ovb.R

source("../course_figures.R")
set.seed(4502)

n      <- 50
reps   <- 10000
beta   <- c(0, 1, 2)       # beta_0, beta_1, beta_2
delta  <- 0.5              # slope of x2 on x1 (built in exactly)
sigma  <- 2                # sd of u

z  <- scale(matrix(rnorm(2 * n), n, 2))
x1 <- z[, 1]
v  <- drop(scale(resid(lm(z[, 2] ~ x1))))
x2 <- delta * x1 + v

X_long  <- cbind(1, x1, x2)
X_short <- cbind(1, x1)
Y <- drop(X_long %*% beta) + matrix(rnorm(n * reps, 0, sigma), n, reps)   # n x reps

B_long  <- solve(crossprod(X_long),  crossprod(X_long,  Y))   # 3 x reps
B_short <- solve(crossprod(X_short), crossprod(X_short, Y))   # 2 x reps
b1_hat   <- B_long[2, ]
b2_hat   <- B_long[3, ]
b1_tilde <- B_short[2, ]

delta_tilde <- unname(coef(lm(x2 ~ x1))[2])
identity_gap <- max(abs(b1_tilde - (b1_hat + b2_hat * delta_tilde)))   # sample identity, ~1e-15

res <- c(delta_tilde            = delta_tilde,
         mean_b1_hat            = mean(b1_hat),
         mean_b1_tilde          = mean(b1_tilde),
         predicted_mean_tilde   = beta[2] + beta[3] * delta_tilde,
         simulated_bias         = mean(b1_tilde) - beta[2],
         predicted_bias         = beta[3] * delta_tilde,
         sd_b1_hat              = sd(b1_hat),
         sd_b1_tilde            = sd(b1_tilde),
         max_identity_gap       = identity_gap)
print(signif(res, 4))
writeLines(c("fig_ovb.R --- numbers behind figs/ovb_sim.pdf (slide 'Omitted Variable Bias: Simple Case')",
             sprintf("n = %d, samples = %d, beta = (%s), sd(u) = %g, seed = 4502",
                     n, reps, paste(beta, collapse = ", "), sigma), "",
             sprintf("%-22s %s", names(res), formatC(res, digits = 4, format = "g"))),
           "code/fig_ovb_output.txt")
stopifnot(identity_gap < 1e-8, abs(delta_tilde - delta) < 1e-12)

# ---------------------------------------------------------------- figure
# The two simulated sampling distributions, the true beta_1, and the centre
# the formula predicts for the short regression.
xlim <- c(-0.3, 3.3)
d_hat   <- density(b1_hat,   adjust = 1.5, from = xlim[1], to = xlim[2], n = 512)
d_tilde <- density(b1_tilde, adjust = 1.5, from = xlim[1], to = xlim[2], n = 512)
ytop <- 1.75 * max(d_hat$y, d_tilde$y)
col_long  <- pal[["blue"]]
col_short <- col_cardinal

# Sized to be shown at full text width under three lines of text on the slide
# (5.9in x 1.45in at 9pt, so figure text matches the slide text).
fig_pdf("ovb_sim", width = 5.9, height = 1.45, pointsize = 9,
        mar = c(2.5, 0.8, 0.1, 0.8), {
  plot(NA, xlim = xlim, ylim = c(0, ytop), xaxs = "i", yaxs = "i",
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  axis(1, at = seq(0, 3, by = 0.5))
  mtext(expression("estimate of the coefficient on " * X[1] * ", over 10,000 simulated samples"),
        side = 1, line = 1.6, cex = 0.9)
  polygon(c(d_hat$x, rev(d_hat$x)), c(d_hat$y, 0 * d_hat$y), border = NA,
          col = alpha_col(col_long, 0.18))
  polygon(c(d_tilde$x, rev(d_tilde$x)), c(d_tilde$y, 0 * d_tilde$y), border = NA,
          col = alpha_col(col_short, 0.18))
  lines(d_hat,   col = col_long,  lwd = 2)
  lines(d_tilde, col = col_short, lwd = 2)

  # true beta_1, the centre the formula predicts, and the bias between them
  ya <- 0.72 * ytop
  ctr <- res[["predicted_mean_tilde"]]
  segments(beta[2], 0, beta[2], ya, lty = 2, col = col_dark)
  segments(ctr, 0, ctr, ya, lty = 3, col = col_dark)
  arrows(beta[2], ya, ctr, ya, length = 0.07, code = 3, col = col_dark, lwd = 1.2)
  text(mean(c(beta[2], ctr)), ya, pos = 3, offset = 0.45,
       labels = bquote({{"bias" == beta[2] %.% tilde(delta)} == .(beta[3]) %*% .(delta_tilde)}
                       == .(res[["predicted_bias"]])))
  text(beta[2] - 0.05, ya, bquote("true " * beta[1] == .(beta[2])), adj = c(1, 0.5))
  text(ctr + 0.05, ya, bquote(beta[1] + beta[2] %.% tilde(delta) == .(ctr)), adj = c(0, 0.5))

  # direct labels for the two distributions
  text(0.55, 0.36 * ytop, expression(hat(beta)[1] * ": includes " * X[2]),
       col = col_long, adj = c(1, 0.5))
  text(2.45, 0.36 * ytop, expression(tilde(beta)[1] * ": omits " * X[2]),
       col = col_short, adj = c(0, 0.5))
})
