# fig_sim_joint.R --- sampling distribution of (beta0_hat, beta1_hat) by simulation.
# Lecture 6.2, slides "Joint Normality: Simulation", "Marginal Sampling
# Distribution", "Joint Sampling Distribution".
#
# Model on the slides: Y = beta0 + beta1 X1 + u, u ~ N(0, 4), beta0 = 5,
# beta1 = -1, n = 100. X1 is drawn once from Uniform(0, 1) and held fixed, so
# the true variance-covariance matrix of the estimates is sigma^2 (X'X)^{-1}.
# Draws  figs/sim_joint_lines.pdf, figs/sim_joint_marginals.pdf,
#        figs/sim_joint_density.pdf
# and prints the variance-covariance matrix shown on the third slide.
#
# Run from the deck folder:  Rscript code/fig_sim_joint.R

source("../course_figures.R")

set.seed(450206)
n      <- 100
beta   <- c(5, -1)
sigma2 <- 4
n_sims <- 5000

x1 <- runif(n)                              # fixed design, reused in every sample
X  <- cbind(1, x1)
XtX_inv <- solve(crossprod(X))

est <- t(replicate(n_sims, {
  y <- drop(X %*% beta) + rnorm(n, sd = sqrt(sigma2))
  drop(XtX_inv %*% crossprod(X, y))
}))
colnames(est) <- c("b0", "b1")

V_true <- sigma2 * XtX_inv                  # variance-covariance matrix of the estimates
V_sim  <- cov(est)                          # the same matrix, estimated from the draws
dimnames(V_true) <- dimnames(V_sim) <- list(c("b0_hat", "b1_hat"), c("b0_hat", "b1_hat"))

cat("Fixed design: mean(x1) =", round(mean(x1), 3), " var(x1) =", round(var(x1), 3), "\n")
cat("True variance-covariance matrix sigma^2 (X'X)^{-1}  [shown on the slide]:\n")
print(round(V_true, 2))
cat("Variance-covariance matrix of the", n_sims, "simulated estimates:\n")
print(round(V_sim, 2))
cat("Correlation of the estimates (true):", round(cov2cor(V_true)[1, 2], 2), "\n")

# ---- shared axes for the whole run of three slides ---------------------------
lim0 <- c(3.0, 7.0)                         # beta0_hat axis: true value +/- 4.7 sd
lim1 <- c(-4.4, 2.4)                        # beta1_hat axis: true value +/- 4.7 sd
fig_w <- 5.8
fig_h <- 2.45
two_panel_mar <- c(3.1, 3.4, 1.6, 0.8)

lab_b0 <- expression(hat(beta)[0])
lab_b1 <- expression(hat(beta)[1])

draw_scatter <- function() {
  stopifnot(all(est[, "b0"] > lim0[1] & est[, "b0"] < lim0[2]),
            all(est[, "b1"] > lim1[1] & est[, "b1"] < lim1[2]))
  plot(est[, "b0"], est[, "b1"], type = "n", xlim = lim0, ylim = lim1,
       xlab = lab_b0, ylab = "", xaxs = "i", yaxs = "i")
  mtext(lab_b1, side = 2, line = 2.3, las = 1)      # horizontal, so the hat reads
  points(est[, "b0"], est[, "b1"], pch = 16, cex = 0.3,
         col = alpha_col(col_cardinal, 0.25))
  points(beta[1], beta[2], pch = 3, cex = 1.2, lwd = 2, col = col_dark)
}

# Bivariate normal density N(beta, V_true), written out to avoid extra packages.
dens_bvn <- function(a, b) {
  S_inv <- solve(V_true)
  d0 <- a - beta[1]
  d1 <- b - beta[2]
  q  <- S_inv[1, 1] * d0^2 + 2 * S_inv[1, 2] * d0 * d1 + S_inv[2, 2] * d1^2
  exp(-q / 2) / (2 * pi * sqrt(det(V_true)))
}

# ---- slide 1: fitted lines and the joint scatter -----------------------------
fig_pdf("sim_joint_lines", width = fig_w, height = fig_h, mar = two_panel_mar, {
  par(mfrow = c(1, 2))
  plot(NA, xlim = c(0, 1), ylim = c(2.5, 6.5), xaxs = "i",
       xlab = expression(italic(X)[1]), ylab = "")
  mtext(expression(italic(Y)), side = 2, line = 2.3, las = 1)
  for (s in 1:100) abline(est[s, "b0"], est[s, "b1"],
                          col = alpha_col(col_cardinal, 0.3), lwd = 0.8)
  abline(beta[1], beta[2], col = col_dark, lwd = 2)
  title(main = "Fitted lines from 100 samples", font.main = 1, cex.main = 0.95)
  legend("topright", legend = c("true line", "fitted lines"), bty = "n",
         col = c(col_dark, col_cardinal), lwd = c(2, 1.2), cex = 0.85)

  draw_scatter()
  title(main = paste0("Estimates from ", format(n_sims, big.mark = ","), " samples"),
        font.main = 1, cex.main = 0.95)
  legend("topright", legend = "true value", pch = 3, pt.lwd = 2, pt.cex = 1.1,
         col = col_dark, bty = "n", cex = 0.85)
})

# ---- slide 2: the two marginal distributions ----------------------------------
fig_pdf("sim_joint_marginals", width = fig_w, height = fig_h, mar = two_panel_mar, {
  par(mfrow = c(1, 2))
  marg <- function(v, lim, j, lab, main) {
    stopifnot(all(v > lim[1] & v < lim[2]))
    h <- hist(v, breaks = seq(lim[1], lim[2], length.out = 35), plot = FALSE)
    # headroom above the curve keeps the legend clear of the peak
    ymax <- max(h$density, dnorm(beta[j], beta[j], sqrt(V_true[j, j]))) * 1.35
    plot(h, freq = FALSE, xlim = lim, ylim = c(0, ymax), xaxs = "i", yaxs = "i",
         col = alpha_col(col_cardinal, 0.45), border = "white",
         main = "", xlab = lab, ylab = "Density")
    curve(dnorm(x, beta[j], sqrt(V_true[j, j])), from = lim[1], to = lim[2],
          n = 400, add = TRUE, lwd = 1.6, col = col_dark)
    segments(beta[j], 0, beta[j], dnorm(0, 0, sqrt(V_true[j, j])),
             lty = 2, col = col_grey)
    title(main = main, font.main = 1, cex.main = 0.95)
  }
  marg(est[, "b0"], lim0, 1, lab_b0,
       expression("Sampling distribution of " * hat(beta)[0]))
  marg(est[, "b1"], lim1, 2, lab_b1,
       expression("Sampling distribution of " * hat(beta)[1]))
  legend("topleft", legend = c("normal density", "true value"), bty = "n",
         col = c(col_dark, col_grey), lwd = c(1.6, 1), lty = c(1, 2),
         seg.len = 1.6, cex = 0.8)
})

# ---- slide 3: joint scatter with density contours, and the density surface ----
g0 <- seq(lim0[1], lim0[2], length.out = 81)
g1 <- seq(lim1[1], lim1[2], length.out = 81)
dz <- outer(g0, g1, dens_bvn)
# the surface uses a tighter grid (true value +/- 3.3 sd) so the bell fills the box
sd_true <- sqrt(diag(V_true))
s0 <- seq(beta[1] - 3.3 * sd_true[1], beta[1] + 3.3 * sd_true[1], length.out = 51)
s1 <- seq(beta[2] - 3.3 * sd_true[2], beta[2] + 3.3 * sd_true[2], length.out = 51)
sz <- outer(s0, s1, dens_bvn)

fig_pdf("sim_joint_density", width = fig_w, height = fig_h, mar = two_panel_mar, {
  par(mfrow = c(1, 2))
  draw_scatter()
  # contours enclosing 50%, 90% and 99% of the joint distribution
  levs <- exp(-qchisq(c(0.5, 0.9, 0.99), 2) / 2) / (2 * pi * sqrt(det(V_true)))
  contour(g0, g1, dz, levels = levs, drawlabels = FALSE, add = TRUE,
          col = col_dark, lwd = 1)
  title(main = "Draws and density contours", font.main = 1, cex.main = 0.95)

  par(mar = c(1.3, 0.2, 1.6, 1.2), xpd = NA)
  nz  <- length(s0)
  fz  <- (sz[-1, -1] + sz[-1, -nz] + sz[-nz, -1] + sz[-nz, -nz]) / 4
  ramp <- colorRampPalette(c("#F3F3F3", "#E7B3C4", col_cardinal))(100)
  fcol <- ramp[cut(fz, 100, labels = FALSE)]
  pm <- persp(s0, s1, sz, theta = 5, phi = 25, expand = 0.55, r = 4,
              col = fcol, border = alpha_col(col_dark, 0.10), lwd = 0.2,
              box = FALSE, axes = FALSE)
  # the two front edges of the floor and the axis labels, drawn by hand so the
  # labels can use plotmath (the back edges would be drawn over the surface)
  r0 <- range(s0)
  r1 <- range(s1)
  fl <- rbind(c(r0[1], r1[1]), c(r0[2], r1[1]), c(r0[2], r1[2]))
  lines(trans3d(fl[, 1], fl[, 2], 0, pm), col = col_grey, lwd = 0.8)
  text(trans3d(mean(r0), r1[1], 0, pm), labels = lab_b0, pos = 1, offset = 0.4)
  text(trans3d(r0[2], mean(r1), 0, pm), labels = lab_b1, pos = 4, offset = 0.4)
  title(main = "Bivariate normal density", font.main = 1, cex.main = 0.95)
})
