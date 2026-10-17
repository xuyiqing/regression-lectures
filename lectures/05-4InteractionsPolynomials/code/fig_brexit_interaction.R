# fig_brexit_interaction.R --- Lecture 5-4, interaction between two continuous variables: the design of Becker,
# Fetzer and Novy (2017, Economic Policy 32(92), Table 6, column 1), rebuilt from official data. Replaces
# fig_boix.R (Boix 1999, 22 countries) in v2, 2026-10-11. Continues Lecture 5-3's Brexit example.
#
# Data: code/data/brexit_la.csv, built in lectures/_data/brexit/ (Electoral Commission results; 2001 and 2011
# Censuses via Nomis and Scotland's Census; Open Government Licence; see that folder's README).
# Sample: the 380 counting areas of Great Britain (England 326, Wales 22, Scotland 32).
#   pct_leave        Leave, % of valid votes, 2016
#   pct_noqual_2001  % of residents aged 16-74 with no qualifications (or only "other" qualifications), 2001
#   acc_growth       growth in residents born in the 12 countries that joined the EU in 2004 and 2007,
#                    2001 to 2011, in percentage points of the 2001 population (Becker et al., footnote 15).
#                    Cyprus is counted in both years (the decisions log's "consistent" measure).
# Raw units (Becker et al. standardize both variables; same t on the interaction). Robust (HC1) SEs, as theirs.
#
# Figures (column figures):
#   bx_scatter_1, _2   Leave vs no qualifications, Leave vs migrant growth, each with its bivariate OLS line
#   bx_3d_1..6         3-D scatter; with the fitted surface; another angle; the shaded surface from three angles
#   bx_lines_1..4      fitted Leave-no-qualifications line at migrant growth 0, 1, 3, 6 pp, one at a time
# Numbers printed on the slides: code/brexit_interaction_output.txt
#
# Run from the deck folder (v2/):  Rscript code/fig_brexit_interaction.R

source("../course_figures.R")
suppressPackageStartupMessages({ library(sandwich); library(lmtest) })

bx <- read.csv("code/data/brexit_la.csv")
bx <- bx[bx$nation %in% c("England", "Wales", "Scotland"), ]
stopifnot(nrow(bx) == 380)
bx$leave <- bx$pct_leave; bx$noqual <- bx$pct_noqual_2001; bx$growth <- bx$acc_growth

fit <- lm(leave ~ noqual * growth, data = bx)
V <- vcovHC(fit, type = "HC1")
b <- unname(coef(fit))
s1 <- lm(leave ~ noqual, data = bx); s2 <- lm(leave ~ growth, data = bx)
nm_g <- lm(leave ~ noqual + noqual:growth, data = bx)   # migrant growth main effect dropped
nm_q <- lm(leave ~ growth + noqual:growth, data = bx)   # no-qualifications main effect dropped
zs <- function(v) (v - mean(v)) / sd(v)
fz <- lm(leave ~ zs(noqual) * zs(growth), data = bx)
g_levels <- c(0, 1, 3, 6)
slope_at <- function(g) b[2] + b[4] * g
slope_se <- function(g) sqrt(V[2, 2] + g^2 * V[4, 4] + 2 * g * V[2, 4])

out <- "code/brexit_interaction_output.txt"
sink(out)
cat("Brexit, 380 counting areas of Great Britain. Leave % ~ no qualifications 2001 (%) x EU-accession migrant growth 2001-11 (pp)\n\n")
cat("Distributions:\n")
cat("  noqual: "); print(round(quantile(bx$noqual, c(0, .1, .5, .9, 1)), 2))
cat("  growth: "); print(round(quantile(bx$growth, c(0, .1, .25, .5, .75, .9, .95, .99, 1)), 2))
cat(sprintf("  growth: mean %.2f, sd %.2f, skewness %.2f; areas with growth >= 3: %d, >= 6: %d\n", mean(bx$growth), sd(bx$growth),
            mean(zs(bx$growth)^3), sum(bx$growth >= 3), sum(bx$growth >= 6)))
cat(sprintf("  cor(noqual, growth) = %.3f\n", cor(bx$noqual, bx$growth)))
cat("  noqual among areas with growth >= 4:", round(range(bx$noqual[bx$growth >= 4]), 1), "\n")
cat("  top 5 by growth:\n"); print(head(bx[order(-bx$growth), c("area", "region", "growth", "noqual", "leave")], 5), row.names = FALSE)
cat("\nBivariate (classical SEs):\n")
cat(sprintf("  leave ~ noqual: intercept %.2f, slope %.3f (t %.1f), R2 %.3f\n", coef(s1)[1], coef(s1)[2], summary(s1)$coef[2, 3], summary(s1)$r.squared))
cat(sprintf("  leave ~ growth: intercept %.2f, slope %.3f (t %.2f), R2 %.3f\n", coef(s2)[1], coef(s2)[2], summary(s2)$coef[2, 3], summary(s2)$r.squared))
cat("\nInteraction model, HC1 SEs:\n"); print(round(coeftest(fit, vcov. = V)[, ], 4))
cat(sprintf("R2 %.3f; classical SE of interaction %.4f (t %.2f)\n", summary(fit)$r.squared, summary(fit)$coef[4, 2], summary(fit)$coef[4, 3]))
cat("\nSlope of no qualifications at several levels of migrant growth (b1 + b3 g, HC1 SE):\n")
for (g in c(g_levels, median(bx$growth), quantile(bx$growth, .9)))
  cat(sprintf("  growth %5.2f: slope %.3f (SE %.3f); line: leave = %.2f + %.3f noqual\n", g, slope_at(g), slope_se(g), b[1] + b[3] * g, slope_at(g)))
cat("\nSlope of migrant growth at no qualifications 0 (b2), and at observed values (b2 + b3 q):\n")
for (qq in c(0, 12.6, 26.7, 35.5, 44.2, 52.6))
  cat(sprintf("  noqual %4.1f: %.2f\n", qq, b[3] + b[4] * qq))
cat(sprintf("  zero at noqual = %.1f\n", -b[3] / b[4]))
cat("\nStandardized (z-scores, as Becker et al. Table 6), HC1:\n"); print(round(coeftest(fz, vcov. = vcovHC(fz, "HC1"))[, ], 3))
cat("\nLower-order term dropped (HC1):\n")
cat("  leave ~ noqual + noqual:growth\n"); print(round(coeftest(nm_g, vcov. = vcovHC(nm_g, "HC1"))[, ], 4))
cat("  leave ~ growth + noqual:growth\n"); print(round(coeftest(nm_q, vcov. = vcovHC(nm_q, "HC1"))[, ], 4))
cat("\nLeverage and influence (raw-unit model):\n")
h <- hatvalues(fit); cd <- cooks.distance(fit)
cat(sprintf("  max leverage %.3f (%s); max Cook's D %.3f (%s)\n", max(h), bx$area[which.max(h)], max(cd), bx$area[which.max(cd)]))
d1 <- bx[bx$growth < quantile(bx$growth, .99), ]; f1 <- lm(leave ~ noqual * growth, data = d1)
cat(sprintf("  drop top 1%% of growth (%d areas): interaction %.4f (HC1 t %.2f)\n", nrow(bx) - nrow(d1), coef(f1)[4],
            coeftest(f1, vcov. = vcovHC(f1, "HC1"))[4, 3]))
sink()

# ---- figures ---------------------------------------------------------------------------------------
col_w <- 2.7; col_h <- 2.5
col_mar <- c(3.0, 3.0, 1.0, 0.5)
yl <- c(15, 80)
lab_leave <- "Leave (%)"
lab_q <- "No qualifications, 2001 (%)"
lab_g <- "EU-accession migrant growth (pp)"

scatter_fit <- function(xv, xlab, xlim) {
  plot(xv, bx$leave, xlab = xlab, ylab = lab_leave, ylim = yl, xlim = xlim,
       pch = 16, cex = 0.45, col = alpha_col(col_dark, 0.5))
  f <- lm(bx$leave ~ xv)
  segments(min(xv), coef(f)[1] + coef(f)[2] * min(xv), max(xv), coef(f)[1] + coef(f)[2] * max(xv), col = col_cardinal, lwd = 2)
}
fig_pdf("bx_scatter_1", width = col_w, height = col_h, mar = col_mar, scatter_fit(bx$noqual, lab_q, c(10, 55)))
fig_pdf("bx_scatter_2", width = col_w, height = col_h, mar = col_mar, scatter_fit(bx$growth, lab_g, c(0, 12.5)))

# ---- 3-D: data, fitted surface, shading -------------------------------------------------------------
gq <- seq(10, 55, length.out = 10)
gg <- seq(0, 12.5, length.out = 10)
surf <- outer(gq, gg, function(q, g) b[1] + b[2] * q + b[3] * g + b[4] * q * g)
zl <- c(0, 100)
zlab_left <- function(pm, xr, yr, zr, label, widest = "000") {
  cn <- expand.grid(x = xr, y = yr)
  p <- trans3d(cn$x, cn$y, mean(zr), pm)
  k <- which.min(p$x)
  off <- strwidth(widest, cex = 0.72) + strheight("M", cex = 0.85) * 1.6
  text(p$x[k] - off, p$y[k], label, srt = 90, cex = 0.85, xpd = NA)
}
view3d <- function(theta, phi, show = c("points", "wire", "shade")) {
  par(xpd = NA)
  shade <- "shade" %in% show
  pm <- persp(gq, gg, pmin(pmax(surf, zl[1]), zl[2]), zlim = zl, theta = theta, phi = phi,
              r = 3, expand = 0.75,
              col = if (shade) alpha_col(col_cardinal, 0.55) else NA,
              shade = if (shade) 0.45 else NA,
              border = if ("wire" %in% show) alpha_col(col_cardinal, 0.55) else if (shade) "white" else NA,
              lwd = 0.6, ticktype = "detailed", nticks = 4, cex.axis = 0.72, cex.lab = 0.85,
              xlab = "no qual.", ylab = "growth", zlab = "")
  zlab_left(pm, range(gq), range(gg), zl, "Leave")
  if ("points" %in% show) {
    if ("wire" %in% show) {   # a short grey stem from each point to the fitted surface
      from <- trans3d(bx$noqual, bx$growth, bx$leave, pm)
      to <- trans3d(bx$noqual, bx$growth, fitted(fit), pm)
      segments(from$x, from$y, to$x, to$y, col = col_grey, lwd = 0.4)
    }
    points(trans3d(bx$noqual, bx$growth, bx$leave, pm), pch = 16, cex = 0.35, col = alpha_col(col_dark, 0.7))
  }
}
sf_mar <- c(0.9, 1.3, 0.2, 0.6)
fig_pdf("bx_3d_1", width = col_w, height = col_h, mar = sf_mar, view3d(-30, 25, "points"))
fig_pdf("bx_3d_2", width = col_w, height = col_h, mar = sf_mar, view3d(-30, 25, c("points", "wire")))
fig_pdf("bx_3d_3", width = col_w, height = col_h, mar = sf_mar, view3d(35, 25, c("points", "wire")))
fig_pdf("bx_3d_4", width = col_w, height = col_h, mar = sf_mar, view3d(-30, 25, "shade"))
fig_pdf("bx_3d_5", width = col_w, height = col_h, mar = sf_mar, view3d(35, 25, "shade"))
fig_pdf("bx_3d_6", width = col_w, height = col_h, mar = sf_mar, view3d(125, 25, "shade"))

# ---- lines for no qualifications at several levels of migrant growth ----------------------------------
cols_g <- c(pal[["blue"]], pal[["green"]], pal[["orange"]], col_cardinal)
# Each line is drawn only over the no-qualifications range of the areas whose migrant growth is near that level
# (windows 0-0.5, 0.5-1.5, 2-4, 4-8 pp), so no line runs where there are no such areas.
windows <- list(c(0, 0.5), c(0.5, 1.5), c(2, 4), c(4, 8))
line_range <- lapply(windows, function(w) range(bx$noqual[bx$growth >= w[1] & bx$growth <= w[2]]))
cat("Line ranges (noqual) by growth window:\n",
    paste(sapply(seq_along(windows), function(k) sprintf("  growth %g-%g pp: %d areas, noqual %.1f-%.1f", windows[[k]][1], windows[[k]][2],
      sum(bx$growth >= windows[[k]][1] & bx$growth <= windows[[k]][2]), line_range[[k]][1], line_range[[k]][2])), collapse = "\n"),
    "\n", file = out, append = TRUE)
lines_plot <- function(n) {
  plot(bx$noqual, bx$leave, xlim = c(10, 55), ylim = yl, xlab = lab_q, ylab = lab_leave,
       pch = 16, cex = 0.35, col = alpha_col(col_grey, 0.35))
  for (k in seq_len(n)) {
    g <- g_levels[k]
    r <- line_range[[k]]
    segments(r[1], b[1] + b[3] * g + slope_at(g) * r[1], r[2], b[1] + b[3] * g + slope_at(g) * r[2], col = cols_g[k], lwd = 2)
  }
  on <- seq_along(g_levels) <= n   # all four legend entries keep their place
  legend("bottomright", bty = "n", cex = 0.8, seg.len = 1.4, lwd = 2, title = "growth (pp)", title.adj = 0.1,
         legend = paste("=", g_levels), col = ifelse(on, cols_g, NA),
         text.col = ifelse(on, col_dark, alpha_col("white", 0)))
}
for (n in 1:4) fig_pdf(paste0("bx_lines_", n), width = col_w, height = col_h, mar = col_mar, lines_plot(n))
cat("fig_brexit_interaction: done; numbers in", out, "\n")
