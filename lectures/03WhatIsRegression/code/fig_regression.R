# fig_regression.R --- Lecture 3, sections "Linear Regression" and
# "Combining Linear Regression with Nonparametric Regression", on US counties
# in the 2024 presidential election.
#
#   Y  rep24        Republican (Trump) % of the two-party vote, 2024
#   X  pct_ba       % of adults 25+ with a BA or higher (ACS 2019-23)   [linear regression]
#   X  log_density  log of people per square mile                      [LOESS and GAM]
#
#   figs/county_ols.pdf          the least-squares line, rep24 on pct_ba, all 3,034 counties
#   figs/county_loess_base.pdf   loess() fit drawn with base graphics, sample of 200
#   figs/county_loess_gg.pdf     ggplot2 geom_smooth(method = "loess"), sample of 200
#   figs/county_loess_spans.pdf  loess with span = 0.25, 0.75 and 1, sample of 200
#   figs/county_gam.pdf          ggplot2 geom_smooth(method = "gam"), all 3,034 counties
#
# The sample is drawn with set.seed(450), the same 200 counties as in
# fig_kernel.R. The loess and GAM figures reproduce the R code printed on the
# slides; this script only adds the course look (theme and colors). The R
# output printed on the slides, and every other number they cite, is written
# to code/regression_output.txt.
#
# Data: code/data/county_pres.csv, copied from lectures/_data/us_elections/
# (README there: MEDSL precinct returns aggregated to counties, MEDSL state
# totals for checks, ACS 2019-23, 2023 Census Gazetteer). 3,105 counties;
# 3,034 with a 2024 share (Alaska and Connecticut are not in the file;
# Louisiana's 64 parishes, 3 Indiana and 3 New Jersey counties and Kalawao, HI
# have no usable 2024 returns).
#
# Run from the deck folder:  Rscript --vanilla code/fig_regression.R

source("../course_figures.R")
library(ggplot2)
sink("code/regression_output.txt", split = TRUE)
options(width = 90)

# Data, as on the slide "Parametric Approach: Linear Regression in R" -----------
cat("> counties <- read.csv(\"county_pres.csv\")\n")
cat("> counties$rep24 <- 100 * counties$rep2p_2024   # Republican % of two-party vote\n")
cat("> counties <- subset(counties, !is.na(rep24))    # counties with 2024 returns\n")
counties <- read.csv("code/data/county_pres.csv")   # the slide reads it from the working folder
counties$rep24 <- 100 * counties$rep2p_2024     # Republican % of two-party vote
miss <- counties[is.na(counties$rep24), ]
cat(sprintf("[not on the slide] %d counties in the file (no AK or CT rows: %s); %d without a 2024 share: %s\n",
            nrow(counties), !any(counties$state %in% c("AK", "CT")), nrow(miss),
            paste(names(table(miss$state)), table(miss$state), collapse = ", ")))
counties <- subset(counties, !is.na(rep24))
cat("> nrow(counties)\n"); print(nrow(counties))
stopifnot(nrow(counties) == 3034)
cat("> head(counties[, c(\"county\", \"state\", \"rep24\", \"pct_ba\", \"log_density\")])\n")
print(head(counties[, c("county", "state", "rep24", "pct_ba", "log_density")]))

# Least squares ------------------------------------------------------------------
cat("\n> lm.out <- lm(rep24 ~ pct_ba, data = counties)\n> summary(lm.out)\n")
lm.out <- lm(rep24 ~ pct_ba, data = counties)
print(summary(lm.out))
b <- coef(lm.out)
cat(sprintf("\nintercept %.1f; slope %.3f per point of BA = %.1f per 10 points; t = %.1f; R2 = %.2f\n",
            b[[1]], b[[2]], 10 * b[[2]], summary(lm.out)$coefficients[2, 3],
            summary(lm.out)$r.squared))
o <- order(counties$pct_ba)
lowest <- counties[o[1:3], c("county", "state", "pct_ba", "pop")]
cat("lowest % BA:\n"); print(lowest, row.names = FALSE)
cat(sprintf("%% BA ranges from %.1f to %.1f\n", min(counties$pct_ba), max(counties$pct_ba)))
stopifnot(b[[2]] < 0, lowest$pct_ba[1] == 0, lowest$pct_ba[2] > 4)

xlab_ba <- "% of adults 25+ with a BA or higher"
ylab_r  <- "Republican % of two-party vote, 2024"
fig_pdf("county_ols", {
  plot(NA, xlim = c(0, 82), ylim = c(0, 100), xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = xlab_ba, ylab = ylab_r)
  axis(1, at = seq(0, 80, 20))
  axis(2, at = seq(0, 100, 20))
  points(counties$pct_ba, counties$rep24, pch = 16, cex = 0.35,
         col = alpha_col(col_dark, 0.3))
  abline(lm.out, col = col_cardinal, lwd = 2.2)
  text(30, 95, adj = c(0, 0.5), col = col_cardinal,
       bquote("fitted line:" ~~ rep24 == .(sprintf("%.2f", b[[1]])) -
                .(sprintf("%.2f", -b[[2]])) %*% "pct_ba"))
})

# The random sample of 200 counties, as on the slide "LOESS Characterization" ------
cat("\n> set.seed(450)\n> cty200 <- counties[sample(nrow(counties), 200), ]   # a random sample of 200\n")
set.seed(450)
cty200 <- counties[sample(nrow(counties), 200), ]
cty200 <- cty200[order(cty200$log_density), ]
lo.out <- loess(rep24 ~ log_density, data = cty200)
print(summary(lo.out))

xlab_d <- "log population density (people per sq. mile)"
fig_pdf("county_loess_base", {
  plot(NA, xlim = c(-1.3, 8.2), ylim = c(15, 100), xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = xlab_d, ylab = ylab_r)
  axis(1, at = seq(0, 8, 2))
  axis(2, at = seq(20, 100, 20))
  points(cty200$log_density, cty200$rep24, pch = 1, cex = 0.95, col = col_dark)
  lines(lo.out$fitted, x = lo.out$x, col = col_cardinal, lwd = 2.2)
})

# ggplot2 versions ---------------------------------------------------------------------
course_look <- list(theme_450a() +
                      theme(geom = element_geom(ink = col_dark, accent = col_cardinal)),
                    labs(x = xlab_d, y = "Republican % of two-party vote"))

p1 <- ggplot(cty200, aes(x = log_density, y = rep24)) +
  geom_point() +
  geom_smooth(method = "loess")
fig_gg("county_loess_gg", p1 + course_look)

p2 <- ggplot(cty200, aes(x = log_density, y = rep24)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, aes(colour = "0.25")) +
  geom_smooth(method = "loess", span = 0.75, se = FALSE, aes(colour = "0.75")) +
  geom_smooth(method = "loess", span = 1,    se = FALSE, aes(colour = "1")) +
  labs(colour = "span")
fig_gg("county_loess_spans", p2 + course_look +
  scale_colour_manual(values = c("0.25" = pal[["blue"]],
                                 "0.75" = col_cardinal,
                                 "1" = pal[["orange"]])))

# How much the span matters: largest gap between the span 0.25 and span 0.75
# curves, over the middle 96% of log density (2nd to 98th percentile), in the
# sample of 200 and in all 3,034 counties.
span_gap <- function(d) {
  g <- seq(quantile(d$log_density, 0.02), quantile(d$log_density, 0.98), length.out = 400)
  p <- sapply(c(0.25, 0.75), function(s)
    predict(loess(rep24 ~ log_density, data = d, span = s), data.frame(log_density = g)))
  max(abs(p[, 1] - p[, 2]))
}
gap200 <- span_gap(cty200)
gap_all <- span_gap(counties)
cat(sprintf("\nlargest gap between span 0.25 and span 0.75 (2nd-98th pct of log density): sample of 200 %.1f points; all 3,034 counties %.1f points\n",
            gap200, gap_all))
stopifnot(gap_all < gap200)

# GAM on all counties, as on the slide "GAM Characterization of E[Y|X]" ---------------
stopifnot(requireNamespace("mgcv", quietly = TRUE))
p3 <- ggplot(counties, aes(x = log_density, y = rep24)) +
  geom_point(size = 0.5, alpha = 0.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))
fig_gg("county_gam", p3 + course_look)

gam_curve <- layer_data(p3, 2)
gam_fit <- mgcv::gam(rep24 ~ s(log_density, bs = "cs"), data = counties, method = "REML")
gap_mgcv <- max(abs(gam_curve$y - predict(gam_fit, data.frame(log_density = gam_curve$x))))
edf <- sum(gam_fit$edf[-1])
lin <- lm(rep24 ~ log_density, data = counties)
cat(sprintf("GAM on all %d counties: edf of s(log_density) = %.2f; deviance explained %.3f (straight line R2 %.3f)\n",
            nrow(counties), edf, summary(gam_fit)$dev.expl, summary(lin)$r.squared))
slope_at <- function(a, b) diff(predict(gam_fit, data.frame(log_density = c(a, b)))) / (b - a)
cat(sprintf("GAM average slope: %.1f points per log unit on [0, 4.5]; %.1f on [4.5, 8] (log density 4.5 = %.0f people per sq. mile)\n",
            slope_at(0, 4.5), slope_at(4.5, 8), exp(4.5)))
stopifnot(gap_mgcv < 1e-6, edf > 3)
sink()
