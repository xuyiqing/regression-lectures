# fig_cluster_coverage.R --- coverage of four 95% confidence intervals for a
# mean when the data come in clusters (lecture 2.4, "Relative Performance of
# Different Inference Procedures", four slides: rho = 0, 0.05, 0.25, 0.75).
#
# Run from the deck folder:   Rscript code/fig_cluster_coverage.R
# Writes figs/cluster_coverage_rho000.pdf, _rho005, _rho025, _rho075.
#
# Replaces the 2015 figures cluster_example1-4.pdf, which came from
# CIcoverage.clusters.plot.plus() in code/legacy/Clusters_and_CIs_v2.R. Same
# design and the same four procedures; changes:
#   * data are drawn from the one-way random-effects model on the slides,
#     X_gi = mu + u_g + e_gi, so the true mean is mu itself (the old code drew
#     a finite population of 10 million values and used its mean);
#   * the cluster-adjusted CI estimates rho from the ANOVA mean squares, as the
#     slides teach, and uses G - 1 degrees of freedom (the old code plugged in
#     the TRUE rho and used n - 1);
#   * coverage is computed over 1,000 simulated samples; each panel draws the
#     first 100 (the old figures reported coverage from the 100 drawn);
#   * no seed was set before; this script fixes one.
#
# Design (unchanged): G = 100 sampled clusters, m = 10 units per cluster
# (n = 1,000), normal data with total variance 100, alpha = 0.05, B = 1,000
# bootstrap resamples. All four procedures use the same simulated samples.

source("../course_figures.R")
set.seed(20260918)

G     <- 100        # clusters per sample
m     <- 10         # units per cluster
n     <- m * G
mu    <- 0          # true mean
s2    <- 100        # total variance sigma^2 = sigma_B^2 + sigma_W^2
alpha <- 0.05
S     <- 1000       # simulated samples per rho
B     <- 1000       # bootstrap resamples per sample
show  <- 100        # intervals drawn per panel
rhos  <- c(0, 0.05, 0.25, 0.75)

one_sample <- function(rho) {
  u <- rnorm(G, 0, sqrt(rho * s2))                      # cluster effects
  x <- matrix(rnorm(n, 0, sqrt((1 - rho) * s2)), m, G)  # columns = clusters
  x <- mu + sweep(x, 2, u, "+")
  xv   <- as.vector(x)
  xbar <- mean(xv)
  cm   <- colMeans(x)                                   # cluster means

  # 1. conventional i.i.d. interval
  se1 <- sd(xv) / sqrt(n)
  ci1 <- xbar + c(-1, 1) * qt(1 - alpha / 2, n - 1) * se1

  # 2. cluster-adjusted interval: rho-hat from the one-way ANOVA mean squares
  msb  <- m * sum((cm - xbar)^2) / (G - 1)
  msw  <- sum(sweep(x, 2, cm)^2) / (G * (m - 1))
  rhat <- (msb - msw) / (msb + (m - 1) * msw)
  deff <- 1 + (m - 1) * rhat
  stopifnot(deff > 0)
  se2 <- sqrt(var(xv) / n * deff)
  ci2 <- xbar + c(-1, 1) * qt(1 - alpha / 2, G - 1) * se2

  # 3. bootstrap percentile interval, resampling units
  idx <- sample.int(n, n * B, replace = TRUE)
  bm  <- rowMeans(matrix(xv[idx], B, n))
  ci3 <- unname(quantile(bm, c(alpha / 2, 1 - alpha / 2)))

  # 4. cluster bootstrap percentile interval, resampling whole clusters.
  # With equal cluster sizes the mean of the resampled data equals the mean
  # of the resampled cluster means.
  cidx <- sample.int(G, G * B, replace = TRUE)
  cbm  <- rowMeans(matrix(cm[cidx], B, G))
  ci4  <- unname(quantile(cbm, c(alpha / 2, 1 - alpha / 2)))

  c(est = xbar, lo1 = ci1[1], hi1 = ci1[2], lo2 = ci2[1], hi2 = ci2[2],
    lo3 = ci3[1], hi3 = ci3[2], lo4 = ci4[1], hi4 = ci4[2], rhat = rhat)
}

res <- lapply(rhos, function(r) t(replicate(S, one_sample(r))))
names(res) <- rhos

methods <- c("Conventional", "Cluster-adjusted", "Bootstrap", "Cluster bootstrap")
covers <- function(r, k) r[, paste0("lo", k)] <= mu & mu <= r[, paste0("hi", k)]

cat("Coverage over", S, "samples (share of 95% CIs that contain mu):\n")
tab <- t(sapply(res, function(r) sapply(1:4, function(k) mean(covers(r, k)))))
dimnames(tab) <- list(paste0("rho=", rhos), methods)
print(round(tab, 3))
cat("\nMisses among the", show, "intervals drawn:\n")
tab_show <- t(sapply(res, function(r) sapply(1:4, function(k) sum(!covers(r[1:show, ], k)))))
dimnames(tab_show) <- dimnames(tab)
print(tab_show)
cat("\nMean rho-hat:", round(sapply(res, function(r) mean(r[, "rhat"])), 3), "\n")

# Identical horizontal axis on all four slides.
lim <- ceiling(max(abs(unlist(lapply(res, function(r) r[1:show, -c(1, 10)])))))
xlim <- c(-lim, lim)
cat("x-axis limits:", xlim, "\n")

draw_panel <- function(r, k, title) {
  i    <- seq_len(show)
  lo   <- r[i, paste0("lo", k)]
  hi   <- r[i, paste0("hi", k)]
  miss <- lo > mu | hi < mu
  cov  <- mean(covers(r, k))
  plot(NA, xlim = xlim, ylim = c(0.5, show + 0.5), axes = FALSE,
       xlab = "", ylab = "", xaxs = "i")
  segments(lo, i, hi, i, lwd = ifelse(miss, 1.1, 0.7),
           col = ifelse(miss, col_cardinal, alpha_col(col_dark, 0.45)))
  points(r[i, "est"], i, pch = 16, cex = 0.22,
         col = ifelse(miss, col_cardinal, col_dark))
  abline(v = mu, col = col_dark, lwd = 1, lty = 2)
  axis(1, at = pretty(xlim, 4), cex.axis = 0.85, lwd = 0.6, lwd.ticks = 0.6)
  mtext(title, side = 3, line = 1.05, font = 2, cex = 0.82, col = col_dark)
  mtext(sprintf("coverage %d%%", round(100 * cov)), side = 3, line = 0.1,
        cex = 0.82, col = if (cov < 0.9) col_cardinal else col_dark)
}

for (j in seq_along(rhos)) {
  r <- res[[j]]
  nm <- sprintf("cluster_coverage_rho%03d", round(100 * rhos[j]))
  fig_pdf(nm, width = 7, height = 2.8, mar = c(1.9, 0.6, 2.3, 0.6), {
    par(mfrow = c(1, 4), oma = c(1.1, 1.2, 0, 0.2))
    for (k in 1:4) draw_panel(r, k, methods[k])
    mtext(sprintf("sample mean and its 95%% CI (dashed line: true mean); coverage uses all %s samples",
                  format(S, big.mark = ",")),
          side = 1, outer = TRUE, line = 0.1, cex = 0.78, col = col_dark)
    mtext(sprintf("first %d of %s simulated samples", show, format(S, big.mark = ",")),
          side = 2, outer = TRUE, line = 0.1, cex = 0.78, col = col_dark, las = 0)
  })
}
