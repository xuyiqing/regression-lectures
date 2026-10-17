# fig_boot_refine.R --- when the reversed (basic) percentile interval and the
# bootstrap-t help (lecture 2.4, frames "Percentile Interval: Recenter and
# Reverse", "When Reversal Helps: A Biased Statistic", "Refinement:
# Bootstrap-t", "When Bootstrap-t Helps: Skewed Data").
#
# Run from the deck folder:   Rscript code/fig_boot_refine.R     (a few minutes;
#                             uses all but two cores, results do not depend on it)
# Writes figs/boot_gini_1.pdf ... boot_gini_5.pdf (reversal animation),
#        figs/boot_t_1.pdf ... boot_t_4.pdf (bootstrap-t animation),
#        code/fig_boot_refine.txt, the numbers typeset on the slides.
#
# Population and sample are lecture 2-1's (02-1PointEstimation/code/
# fig_sampling.R): 10,000 Gamma(4, 18) incomes with set.seed(450), and the
# sample of 20 drawn with set.seed(2026) (mean 72.76). Resamples of that sample
# use set.seed(2404), the seed of the lecture 2-4 review video, so the median's
# percentile interval [43.5, 97.4] is the one the video shows.
#
# Coverage studies draw fresh samples from the known population; replication i
# at sample size n uses set.seed(seed_base + 1000 * n + i), so the numbers do
# not depend on how the work is split across cores.

source("../course_figures.R")
library(parallel)
cores <- max(1, detectCores() - 2)

set.seed(450)
N <- 10000
incomes <- round(sample(qgamma(ppoints(N), shape = 4, scale = 18)), 1)
set.seed(2026)
x <- sample(x = incomes, size = 20, replace = TRUE)
stopifnot(abs(mean(x) - 72.76) < 1e-9)
n0 <- length(x)
mu <- mean(incomes)

gini <- function(y) {                      # plug-in Gini coefficient
  y <- sort(y); n <- length(y)
  sum((2 * seq_len(n) - n - 1) * y) / (n * sum(y))
}
G <- gini(incomes)

R_cov <- 4000      # fresh samples per coverage cell (Gini, median, correlation)
R_mean <- 10000    # fresh samples per cell for the mean (cheap; its gaps are smaller)
B_cov <- 1000      # resamples per fresh sample
B_one <- 10000     # resamples of the lecture 2-1 sample

pct_ci   <- function(b) unname(quantile(b, c(0.025, 0.975)))
basic_ci <- function(th, b) 2 * th - rev(pct_ci(b))
cover    <- function(ci, truth) c(cover = ci[1] <= truth & truth <= ci[2],
                                  below = ci[2] < truth,     # whole CI below the truth
                                  above = ci[1] > truth)     # whole CI above the truth

out <- character()
say <- function(...) out <<- c(out, sprintf(...))

## ---------------------------------------------------------------------------
## 1. The lecture 2-1 sample: median, mean, Gini
## ---------------------------------------------------------------------------
set.seed(2404)
bs <- t(vapply(seq_len(B_one), function(b) {
  xs <- sample(x, n0, replace = TRUE)
  c(mean = mean(xs), median = median(xs), se = sd(xs) / sqrt(n0), gini = gini(xs))
}, numeric(4)))

se0 <- sd(x) / sqrt(n0)
tstar <- (bs[, "mean"] - mean(x)) / bs[, "se"]
tq <- pct_ci(tstar)

say("Lecture 2-1 sample of 20 incomes; %s resamples, set.seed(2404)", format(B_one, big.mark = ","))
say("population: mean %.2f, median %.2f, Gini %.3f", mu, median(incomes), G)
say("")
say("MEDIAN  %.2f, mean of replications %.2f", median(x), mean(bs[, "median"]))
say("  percentile [%.1f, %.1f]; reversed [%.1f, %.1f]",
    pct_ci(bs[, "median"])[1], pct_ci(bs[, "median"])[2],
    basic_ci(median(x), bs[, "median"])[1], basic_ci(median(x), bs[, "median"])[2])
say("MEAN  %.2f, SE %.2f; t* quantiles %.2f, %.2f (t19: +/- %.2f)",
    mean(x), se0, tq[1], tq[2], qt(0.975, n0 - 1))
say("  percentile [%.1f, %.1f]; reversed [%.1f, %.1f]; bootstrap-t [%.1f, %.1f]; t [%.1f, %.1f]",
    pct_ci(bs[, "mean"])[1], pct_ci(bs[, "mean"])[2],
    basic_ci(mean(x), bs[, "mean"])[1], basic_ci(mean(x), bs[, "mean"])[2],
    mean(x) - tq[2] * se0, mean(x) - tq[1] * se0,
    mean(x) - qt(0.975, n0 - 1) * se0, mean(x) + qt(0.975, n0 - 1) * se0)

g0 <- gini(x)
gpct <- pct_ci(bs[, "gini"])
grev <- basic_ci(g0, bs[, "gini"])
say("GINI  sample %.3f, mean of replications %.3f (bootstrap bias %.3f)",
    g0, mean(bs[, "gini"]), mean(bs[, "gini"]) - g0)
say("  percentile [%.3f, %.3f]; reversed [%.3f, %.3f]", gpct[1], gpct[2], grev[1], grev[2])

## the true sampling distributions (possible only because the population is known)
set.seed(2405)
true_gini <- vapply(seq_len(20000), function(j) gini(sample(incomes, n0, replace = TRUE)), numeric(1))
say("  true sampling distribution of the Gini, n = 20 (20,000 samples): mean %.3f (bias %.3f), SD %.3f",
    mean(true_gini), mean(true_gini) - G, sd(true_gini))

set.seed(2406)
ms <- t(vapply(seq_len(50000), function(j) {
  y <- sample(incomes, n0, replace = TRUE); c(mean(y), sd(y))
}, numeric(2)))
true_t <- (ms[, 1] - mu) / (ms[, 2] / sqrt(n0))
say("  true t-ratio, n = 20 (50,000 samples): 2.5%% %.2f, 97.5%% %.2f; corr(mean, S) = %.2f",
    quantile(true_t, 0.025), quantile(true_t, 0.975), cor(ms[, 1], ms[, 2]))

## ---------------------------------------------------------------------------
## 2. Coverage studies
## ---------------------------------------------------------------------------
cov_study <- function(n, seed_base, one, R = R_cov) {
  r <- mclapply(seq_len(R), function(i) {
    set.seed(seed_base + 1000 * n + i)
    one(n)
  }, mc.cores = cores)
  colMeans(do.call(rbind, r))
}

gini_one <- function(n) {
  y <- sample(incomes, n, replace = TRUE); th <- gini(y)
  b <- vapply(seq_len(B_cov), function(k) gini(sample(y, n, replace = TRUE)), numeric(1))
  c(pct = cover(pct_ci(b), G), rev = cover(basic_ci(th, b), G))
}
mean_one <- function(n) {
  y <- sample(incomes, n, replace = TRUE); th <- mean(y); se <- sd(y) / sqrt(n)
  b <- t(vapply(seq_len(B_cov), function(k) {
    ys <- sample(y, n, replace = TRUE); c(mean(ys), sd(ys) / sqrt(n))
  }, numeric(2)))
  q <- pct_ci((b[, 1] - th) / b[, 2])
  c(pct = cover(pct_ci(b[, 1]), mu), rev = cover(basic_ci(th, b[, 1]), mu),
    boott = cover(c(th - q[2] * se, th - q[1] * se), mu),
    t = cover(th + c(-1, 1) * qt(0.975, n - 1) * se, mu))
}
median_one <- function(n) {
  y <- sample(incomes, n, replace = TRUE); th <- median(y)
  b <- vapply(seq_len(B_cov), function(k) median(sample(y, n, replace = TRUE)), numeric(1))
  c(pct = cover(pct_ci(b), median(incomes)), rev = cover(basic_ci(th, b), median(incomes)))
}
cor_one <- function(n) {                    # contrast: correlation 0.8, bivariate normal
  z1 <- rnorm(n); z2 <- 0.8 * z1 + 0.6 * rnorm(n); th <- cor(z1, z2)
  b <- vapply(seq_len(B_cov), function(k) {
    i <- sample(n, n, replace = TRUE); cor(z1[i], z2[i])
  }, numeric(1))
  c(pct = cover(pct_ci(b), 0.8), rev = cover(basic_ci(th, b), 0.8))
}

ns_gini <- c(10, 20, 50, 100)
ns_mean <- c(10, 20, 50)
cov_gini <- sapply(ns_gini, cov_study, seed_base = 3e6, one = gini_one)
cov_mean <- sapply(ns_mean, cov_study, seed_base = 4e6, one = mean_one, R = R_mean)
cov_med  <- cov_study(20, 5e6, median_one)
cov_cor  <- cov_study(20, 6e6, cor_one)
colnames(cov_gini) <- ns_gini
colnames(cov_mean) <- ns_mean

say("")
say("COVERAGE (%s fresh samples per cell, %s for the mean; %s resamples each)", format(R_cov, big.mark = ","),
    format(R_mean, big.mark = ","), format(B_cov, big.mark = ","))
for (j in seq_along(ns_gini))
  say("  Gini n = %3d: percentile %.3f (below %.3f, above %.3f); reversed %.3f (below %.3f, above %.3f)",
      ns_gini[j], cov_gini["pct.cover", j], cov_gini["pct.below", j], cov_gini["pct.above", j],
      cov_gini["rev.cover", j], cov_gini["rev.below", j], cov_gini["rev.above", j])
say("  median n = 20: percentile %.3f, reversed %.3f", cov_med["pct.cover"], cov_med["rev.cover"])
say("  correlation 0.8, normal data, n = 20: percentile %.3f, reversed %.3f", cov_cor["pct.cover"], cov_cor["rev.cover"])
for (j in seq_along(ns_mean))
  say("  mean n = %3d: percentile %.3f, reversed %.3f, bootstrap-t %.3f, t %.3f",
      ns_mean[j], cov_mean["pct.cover", j], cov_mean["rev.cover", j], cov_mean["boott.cover", j], cov_mean["t.cover", j])
for (k in c("pct", "rev", "boott", "t"))
  say("  mean n = 20, %-5s: whole CI below the mean %.3f, above %.3f", k,
      cov_mean[paste0(k, ".below"), "20"], cov_mean[paste0(k, ".above"), "20"])

writeLines(out, "code/fig_boot_refine.txt")
cat(out, sep = "\n")

## ---------------------------------------------------------------------------
## 3. Figures: reversal animation (Gini), five steps on identical axes
## ---------------------------------------------------------------------------
xlim_g <- c(0.10, 0.46)
brk_g  <- seq(0.05, 0.55, by = 0.01)
h_true <- hist(true_gini, breaks = brk_g, plot = FALSE)
h_boot <- hist(bs[, "gini"], breaks = brk_g, plot = FALSE)
ymax_g <- max(h_true$density, h_boot$density) * 1.30
bar_y  <- c(-0.16, -0.36) * ymax_g                   # rows for the two intervals

draw_h <- function(h, fill, border) rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$density,
                                         col = fill, border = border, lwd = 0.4)
ci_bar <- function(ci, y, col, label) {
  segments(ci[1], y, ci[2], y, lwd = 3, col = col, lend = 1)
  segments(ci, y - 0.025 * ymax_g, ci, y + 0.025 * ymax_g, lwd = 1.5, col = col)
  text(ci[1], y, sprintf("%.3f", ci[1]), pos = 2, cex = 0.72, col = col, offset = 0.3)
  text(ci[2], y, sprintf("%.3f", ci[2]), pos = 4, cex = 0.72, col = col, offset = 0.3)
  text(xlim_g[1], y, label, adj = c(0, 0.5), cex = 0.78, col = col)
}
gini_step <- function(step) {
  plot(NA, xlim = xlim_g, ylim = c(-0.44 * ymax_g, ymax_g), axes = FALSE,
       xlab = "Gini coefficient of 20 incomes", ylab = "", xaxs = "i")
  axis(1, at = seq(0.10, 0.45, by = 0.05), cex.axis = 0.85, lwd = 0.6, lwd.ticks = 0.6)
  abline(h = 0, col = col_grey, lwd = 0.5)
  draw_h(h_true, if (step == 1) alpha_col(col_dark, 0.30) else alpha_col(col_dark, 0.12),
         if (step == 1) "white" else NA)
  segments(G, 0, G, ymax_g * 0.97, lty = 2, lwd = 1.2, col = col_dark)
  text(G, ymax_g, sprintf("true Gini %.3f", G), cex = 0.75, col = col_dark, pos = 2, offset = 0.2)
  if (step == 1) {
    segments(mean(true_gini), 0, mean(true_gini), ymax_g * 0.88, lwd = 1.5, col = col_dark)
    text(mean(true_gini), ymax_g * 0.88, sprintf("average estimate %.3f", mean(true_gini)),
         cex = 0.75, col = col_dark, pos = 2, offset = 0.2)
    text(0.36, ymax_g * 0.55, "true sampling distribution\n(20,000 samples of 20)", cex = 0.75, col = col_dark)
  } else {
    draw_h(h_boot, alpha_col(col_cardinal, 0.45), "white")
    segments(g0, 0, g0, ymax_g * 0.97, lwd = 1.6, col = col_cardinal)
    text(g0, ymax_g, sprintf("our sample %.3f", g0), cex = 0.75, col = col_cardinal, pos = 4, offset = 0.2)
    segments(mean(bs[, "gini"]), 0, mean(bs[, "gini"]), ymax_g * 0.80, lwd = 1.2, lty = 3, col = col_cardinal)
    text(0.165, ymax_g * 0.45, "true sampling\ndistribution", cex = 0.75, col = col_grey)
    text(0.405, ymax_g * 0.50, "bootstrap\nreplications", cex = 0.75, col = col_cardinal)
  }
  if (step >= 3) ci_bar(gpct, bar_y[1], col_dark, "percentile")
  if (step >= 4) {
    ci_bar(grev, bar_y[2], col_cardinal, "reversed")
    arrows(gpct[1], bar_y[1] - 0.05 * ymax_g, grev[2] - 0.004, bar_y[2] + 0.05 * ymax_g,
           length = 0.06, lwd = 0.8, col = col_grey)
    arrows(gpct[2], bar_y[1] - 0.05 * ymax_g, grev[1] + 0.004, bar_y[2] + 0.05 * ymax_g,
           length = 0.06, lwd = 0.8, col = col_grey)
  }
}
for (s in 1:4) fig_pdf(sprintf("boot_gini_%d", s), width = 7, height = 2.7, pointsize = 13,
                       mar = c(3.0, 0.6, 0.6, 0.6), gini_step(s))

coverage_plot <- function(ns, rows, cols, labels, ylim, xlab, legend_pos = "bottomright") {
  plot(NA, xlim = range(log(ns)) + c(-0.15, 0.15), ylim = ylim, axes = FALSE,
       xlab = xlab, ylab = "coverage of 95% CI")
  axis(1, at = log(ns), labels = ns, cex.axis = 0.85, lwd = 0.6, lwd.ticks = 0.6)
  axis(2, at = seq(0.6, 1, 0.05), labels = paste0(seq(60, 100, 5), "%"), cex.axis = 0.8,
       lwd = 0.6, lwd.ticks = 0.6)
  abline(h = 0.95, lty = 2, col = col_grey)
  for (k in seq_along(rows)) {
    v <- rows[[k]]
    lines(log(ns), v, col = cols[k], lwd = 2)
    points(log(ns), v, col = cols[k], pch = 16, cex = 0.8)
  }
  legend(legend_pos, bty = "n", cex = 0.75, legend = labels, col = cols, lwd = 2, pch = 16)
}
fig_pdf("boot_gini_5", width = 7, height = 2.7, pointsize = 13, mar = c(3.0, 3.6, 0.6, 0.6), {
  coverage_plot(ns_gini, list(cov_gini["pct.cover", ], cov_gini["rev.cover", ]),
                c(col_dark, col_cardinal), c("percentile", "reversed"), c(0.62, 1.0),
                "sample size n (incomes drawn from the lecture 2-1 population)")
  for (j in seq_along(ns_gini)) for (k in c("pct", "rev"))
    text(log(ns_gini[j]), cov_gini[paste0(k, ".cover"), j], sprintf("%.0f%%", 100 * cov_gini[paste0(k, ".cover"), j]),
         pos = if (k == "rev") 3 else 1, cex = 0.68, col = if (k == "rev") col_cardinal else col_dark)
})

## ---------------------------------------------------------------------------
## 4. Figures: bootstrap-t animation (mean of skewed incomes)
## ---------------------------------------------------------------------------
fig_pdf("boot_t_1", width = 7, height = 2.7, pointsize = 13, mar = c(3.0, 3.6, 0.6, 0.6), {
  k <- 1:1000
  lo <- ms[k, 1] - qt(0.975, n0 - 1) * ms[k, 2] / sqrt(n0)
  hi <- ms[k, 1] + qt(0.975, n0 - 1) * ms[k, 2] / sqrt(n0)
  colp <- ifelse(hi < mu, col_cardinal, ifelse(lo > mu, pal["blue"], alpha_col(col_dark, 0.30)))
  plot(ms[k, 1], ms[k, 2], pch = 16, cex = 0.5, col = colp, xlab = "sample mean",
       ylab = "sample SD  S", xlim = c(35, 115), ylim = c(10, 70), bty = "l")
  abline(v = mu, lty = 2, col = col_dark)
  text(mu, 69, sprintf("true mean %.1f", mu), pos = 4, cex = 0.75, col = col_dark, offset = 0.2)
  text(38, 63, sprintf("corr(mean, S) = %.2f", cor(ms[, 1], ms[, 2])), adj = 0, cex = 0.8, col = col_dark)
  legend("bottomright", bty = "n", cex = 0.75, pch = 16, col = c(col_cardinal, pal["blue"]),
         legend = c(sprintf("t interval entirely below the truth (%d of 1,000)", sum(hi < mu)),
                    sprintf("t interval entirely above the truth (%d of 1,000)", sum(lo > mu))))
})

xlim_t <- c(-5, 3.5)
brk_t  <- seq(-40, 10, by = 0.2)
h_tt   <- hist(true_t, breaks = brk_t, plot = FALSE)
d_bt   <- density(tstar, adjust = 1.2)
ymax_t <- max(h_tt$density, d_bt$y) * 1.45
t_step <- function(step) {
  plot(NA, xlim = xlim_t, ylim = c(0, ymax_t), axes = FALSE, xaxs = "i",
       xlab = "t-ratio   (mean - true mean) / (S / sqrt(n)),   n = 20", ylab = "")
  axis(1, at = -5:3, cex.axis = 0.85, lwd = 0.6, lwd.ticks = 0.6)
  draw_h(h_tt, alpha_col(col_dark, if (step == 2) 0.30 else 0.15), "white")
  curve(dt(x, n0 - 1), from = xlim_t[1], to = xlim_t[2], add = TRUE, lwd = 1.5, lty = 2, col = pal["blue"])
  qtrue <- quantile(true_t, c(0.025, 0.975))
  tt <- qt(0.975, n0 - 1)
  segments(c(-tt, tt), 0, c(-tt, tt), ymax_t * 0.13, col = pal["blue"], lwd = 1.2, lty = 2)
  segments(qtrue, 0, qtrue, ymax_t * 0.36, col = col_dark, lwd = 1.5)
  text(qtrue, ymax_t * 0.40, sprintf("%.2f", qtrue), cex = 0.72, col = col_dark)
  text(c(-tt, tt), ymax_t * 0.17, sprintf("%.2f", c(-tt, tt)), cex = 0.72, col = pal["blue"])
  legend_items <- c("true t-ratio (50,000 samples)", "t table, 19 df")
  legend_cols  <- c(alpha_col(col_dark, 0.5), pal["blue"])
  legend_lty   <- c(1, 2); legend_lwd <- c(8, 1.5)
  if (step == 3) {
    lines(d_bt, col = col_cardinal, lwd = 2)
    segments(tq, 0, tq, ymax_t * 0.56, col = col_cardinal, lwd = 1.5)
    text(tq, ymax_t * 0.60, sprintf("%.2f", tq), cex = 0.72, col = col_cardinal)
    legend_items <- c(legend_items, "bootstrap t* from our sample")
    legend_cols  <- c(legend_cols, col_cardinal); legend_lty <- c(legend_lty, 1); legend_lwd <- c(legend_lwd, 2)
  }
  legend("topleft", bty = "n", cex = 0.75, legend = legend_items, col = legend_cols,
         lty = legend_lty, lwd = legend_lwd)
}
for (s in 2:3) fig_pdf(sprintf("boot_t_%d", s), width = 7, height = 2.7, pointsize = 13,
                       mar = c(3.0, 0.6, 0.6, 0.6), t_step(s))

fig_pdf("boot_t_4", width = 7, height = 2.7, pointsize = 13, mar = c(3.0, 3.6, 1.4, 0.6), {
  par(mfrow = c(1, 2))
  coverage_plot(ns_mean, list(cov_mean["pct.cover", ], cov_mean["rev.cover", ],
                              cov_mean["t.cover", ], cov_mean["boott.cover", ]),
                c(col_grey, col_dark, pal["blue"], col_cardinal),
                c("percentile", "reversed", "t", "bootstrap-t"), c(0.85, 0.97), "sample size n")
  mtext("coverage of the mean", side = 3, line = 0.3, cex = 0.8, font = 2)
  m <- rbind(below = cov_mean[c("pct.below", "rev.below", "t.below", "boott.below"), "20"],
             above = cov_mean[c("pct.above", "rev.above", "t.above", "boott.above"), "20"])
  colnames(m) <- c("percentile", "reversed", "t", "bootstrap-t")
  par(mar = c(3.0, 3.0, 1.4, 0.6))
  bp <- barplot(100 * m, beside = TRUE, col = c(col_cardinal, pal["blue"]), border = NA,
                ylim = c(0, 8), las = 1, cex.names = 0.72, cex.axis = 0.8, ylab = "% of samples")
  abline(h = 2.5, lty = 2, col = col_grey)
  text(bp, 100 * m, sprintf("%.1f", 100 * m), pos = 3, cex = 0.62, offset = 0.2)
  mtext("n = 20: misses by side", side = 3, line = 0.3, cex = 0.8, font = 2)
  legend("topright", bty = "n", cex = 0.7, fill = c(col_cardinal, pal["blue"]), border = NA,
         legend = c("CI entirely below", "CI entirely above"))
})
