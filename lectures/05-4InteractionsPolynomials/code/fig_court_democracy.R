# fig_court_democracy.R --- Lecture 5-4, interaction between a dummy and a continuous variable, on the
# running example of Lectures 5-1 and 5-2: executive corruption on high court independence, by regime type.
# Replaces fig_democracy_islam.R (Fish 2002) in v2 (2026-10-11, Yiqing's choice of example).
#
# Data: code/data/countries_xsec.csv, one row per country in 2024, built in lectures/_data/countries/
# (V-Dem v16, CC BY-SA 4.0; see its README).
#   execorr100 100 x V-Dem v2x_execorr, executive corruption (0 = clean, 100 = most corrupt)
#   courtind   V-Dem v2juhcind (original 0-4 scale), high court independence: how often the top court rules
#              independently of the government, 0 = never ... 4 = always (country-expert ratings)
#   democracy  1 if V-Dem polyarchy > 0.5
# Sample: the 179 countries with all three. (Lecture 5-1 used the 168 that also have WDI GDP; no frame here
# shows GDP, so 5-4 keeps the 11 countries without it: Cuba, Eritrea, North Korea, Palestine/Gaza,
# Somaliland, South Sudan, Syria, Taiwan, Venezuela, Yemen, Zanzibar.)
#
# Figures (column figures unless noted):
#   cd_scatter_1..4  scatter; democracies highlighted; both groups; the additive fit (parallel lines)
#   cd_inter_1..3    scatter, then the interaction fit, one group at a time
#   cd_coef_1..4     where each coefficient appears in the two lines
#   cd_surface_1..3  the fitted surface in 3-D from three angles
#   cd_nomain        fit without the democracy main effect (lines forced to meet at court independence 0)
#   cd_me            full width, two panels: (a) marginal effect of court independence in each regime;
#                    (b) marginal effect of democracy across court independence, drawn only where both
#                    groups have data, with a rug for each group
# Numbers printed on the slides: code/court_democracy_output.txt
#
# Run from the deck folder (v2/):  Rscript code/fig_court_democracy.R

source("../course_figures.R")

x <- read.csv("code/data/countries_xsec.csv")
d <- x[complete.cases(x[, c("execorr100", "courtind", "democracy")]), ]
stopifnot(nrow(d) == 179, sum(d$democracy) == 86)
aut <- d$democracy == 0

fit_add <- lm(execorr100 ~ courtind + democracy, data = d)
fit_int <- lm(execorr100 ~ courtind * democracy, data = d)
fit_nm  <- lm(execorr100 ~ courtind + courtind:democracy, data = d)
b <- unname(coef(fit_int))
V <- vcov(fit_int)
a <- unname(coef(fit_add))
c0 <- unname(coef(fit_nm))
q <- qt(0.975, df = fit_int$df.residual)

# marginal effect of democracy at court independence x: b2 + b3 x
me_dem <- function(x) b[3] + b[4] * x
se_dem <- function(x) sqrt(V[3, 3] + x^2 * V[4, 4] + 2 * x * V[3, 4])
# marginal effect of court independence in each regime: b1 (autocracies), b1 + b3 (democracies)
me_court <- c(b[2], b[2] + b[4])
se_court <- c(sqrt(V[2, 2]), sqrt(V[2, 2] + V[4, 4] + 2 * V[2, 4]))
lo_common <- min(d$courtind[!aut]); hi_common <- max(d$courtind[aut])   # where both groups have data
xg_all <- seq(0, 4, by = 0.001)
sig <- (me_dem(xg_all) + q * se_dem(xg_all) < 0) | (me_dem(xg_all) - q * se_dem(xg_all) > 0)

out <- "code/court_democracy_output.txt"
sink(out)
cat("Lecture 5-4 v2: dummy x continuous. V-Dem v16, 2024, N =", nrow(d),
    "| democracies (polyarchy > 0.5):", sum(!aut), "| autocracies:", sum(aut), "\n\n")
cat("Group means:\n"); print(round(aggregate(cbind(courtind, execorr100) ~ democracy, data = d, FUN = mean), 2))
cat("Range of court independence: autocracies", range(d$courtind[aut]), "| democracies", range(d$courtind[!aut]), "\n")
cat("Quantiles of court independence (5, 25, 50, 75, 95%):\n  autocracies",
    round(quantile(d$courtind[aut], c(.05, .25, .5, .75, .95)), 2), "\n  democracies",
    round(quantile(d$courtind[!aut], c(.05, .25, .5, .75, .95)), 2), "\n")
cat("Democracies with court independence < 1.25:", sum(!aut & d$courtind < 1.25),
    "| autocracies with court independence > 2.8:", sum(aut & d$courtind > 2.8), "\n")
print(d[(!aut & d$courtind < 1.3) | (aut & d$courtind > 2.7), c("country", "courtind", "execorr100", "democracy")], row.names = FALSE)

cat("\n-- additive: execorr100 ~ courtind + democracy (N 179)\n"); print(round(summary(fit_add)$coef, 4))
cat("\n-- interaction: execorr100 ~ courtind * democracy\n"); print(round(summary(fit_int)$coef, 4))
cat(sprintf("R2 = %.4f, residual df %d\n", summary(fit_int)$r.squared, fit_int$df.residual))
cat(sprintf("autocracies: yhat = %.4f + (%.4f) court\n", b[1], b[2]))
cat(sprintf("democracies: yhat = %.4f + (%.4f) court   [b0 + b2, b1 + b3]\n", b[1] + b[3], b[2] + b[4]))
cat("\nMarginal effect of court independence (pooled interaction model, classical SEs):\n")
cat(sprintf("  autocracies: %.3f (SE %.3f, t %.2f)\n  democracies: %.3f (SE %.3f, t %.2f)\n",
            me_court[1], se_court[1], me_court[1] / se_court[1], me_court[2], se_court[2], me_court[2] / se_court[2]))
cat("Separate regressions in each group (the leader's check):\n")
for (g in 0:1) {
  s <- summary(lm(execorr100 ~ courtind, data = d[d$democracy == g, ]))$coef
  cat(sprintf("  democracy = %d: slope %.3f (SE %.3f), n %d\n", g, s[2, 1], s[2, 2], sum(d$democracy == g)))
}
cat(sprintf("Interaction b3 %.3f (SE %.3f, t %.2f)\n", b[4], sqrt(V[4, 4]), b[4] / sqrt(V[4, 4])))
cat("\nMarginal effect of democracy = b2 + b3 * court:\n")
cat(sprintf("  zero at court = %.3f (democracies start at %.3f)\n", -b[3] / b[4], lo_common))
cat(sprintf("  drawn over the common range %.3f to %.3f (lowest democracy, highest autocracy)\n", lo_common, hi_common))
for (v in c(lo_common, 1, 1.25, 1.5, 2, 2.5, 2.8, 3, hi_common))
  cat(sprintf("  court %.3f: ME %.2f, 95%% CI [%.2f, %.2f]\n", v, me_dem(v), me_dem(v) - q * se_dem(v), me_dem(v) + q * se_dem(v)))
cat(sprintf("  95%% CI excludes zero for court >= %.3f (grid of 0.001 on 0-4)\n", min(xg_all[sig & xg_all > 0.5])))
cat("\n-- no democracy main effect: execorr100 ~ courtind + courtind:democracy\n"); print(round(summary(fit_nm)$coef, 4))
cat(sprintf("autocracies: %.2f + (%.2f) court; democracies: %.2f + (%.2f) court\n", c0[1], c0[2], c0[1], c0[2] + c0[3]))
# For comparison only (not on the slides): Lecture 5-1's 168-country sample (countries that also have WDI GDP)
d168 <- d[!is.na(d$gdppc), ]
f168 <- summary(lm(execorr100 ~ courtind * democracy, data = d168))$coef
cat(sprintf("\n5-1 sample (N %d): court %.3f, dem %.3f, court:dem %.3f (t %.2f); democracies' slope %.3f\n", nrow(d168),
            f168[2, 1], f168[3, 1], f168[4, 1], f168[4, 3], f168[2, 1] + f168[4, 1]))
sink()

# ---- figures --------------------------------------------------------------------------------------
col_d <- pal[["blue"]]          # democracies (as in Lecture 5-1)
col_a <- col_cardinal           # autocracies
col_w <- 2.7; col_h <- 2.5
col_mar <- c(3.0, 3.0, 1.9, 0.5)
xlab_c <- "Court independence (0–4)"
ylab_c <- "Executive corruption"
xl <- c(-0.15, 4.1); yl <- c(-4, 100)
minus <- function(v) sub("^-", "−", v)

# Both entries always take their place, so the legend does not move between steps.
group_legend <- function(which = c("d", "a"), lines = FALSE) {
  on <- c("d", "a") %in% which
  legend("top", inset = c(0, -0.19), xpd = NA, horiz = TRUE, bty = "n",
         legend = c("Democracies", "Autocracies"),
         col = ifelse(on, c(col_d, col_a), NA),
         text.col = ifelse(on, col_dark, alpha_col("white", 0)),
         pch = if (lines) NA else 16, lwd = if (lines) 2 else NA, seg.len = 1.2,
         pt.cex = 0.9, cex = 0.9, x.intersp = 0.5, text.width = strwidth("Democracies  ") * 0.9)
}
frame_c <- function(xlim = xl, ylim = yl) {
  plot(NA, xlim = xlim, ylim = ylim, xlab = xlab_c, ylab = ylab_c, axes = FALSE, xaxs = "i", yaxs = "i")
  axis(1, at = 0:4); axis(2, at = seq(0, 100, 25))
}
scatter <- function(groups = c("d", "a")) {
  frame_c()
  for (g in c("a", "d")) {
    s <- if (g == "d") !aut else aut
    if (g %in% groups) {
      points(d$courtind[s], d$execorr100[s], pch = 16, cex = 0.6,
             col = alpha_col(if (g == "d") col_d else col_a, 0.75))
    } else {
      points(d$courtind[s], d$execorr100[s], pch = 1, cex = 0.55, col = col_grey)
    }
  }
}
fit_line <- function(a0, s, col, from = 0, to = 4, ...) segments(from, a0 + s * from, to, a0 + s * to, col = col, lwd = 2, ...)
# each group's line over that group's own range of court independence
grp_line <- function(a0, s, dem, ...) {
  r <- range(d$courtind[d$democracy == dem])
  fit_line(a0, s, if (dem == 1) col_d else col_a, from = r[1], to = r[2], ...)
}

fig_pdf("cd_scatter_1", width = col_w, height = col_h, mar = col_mar, scatter(character()))
fig_pdf("cd_scatter_2", width = col_w, height = col_h, mar = col_mar, { scatter("d"); group_legend("d") })
fig_pdf("cd_scatter_3", width = col_w, height = col_h, mar = col_mar, { scatter(); group_legend() })
fig_pdf("cd_scatter_4", width = col_w, height = col_h, mar = col_mar, {
  scatter(); group_legend()
  grp_line(a[1], a[2], 0); grp_line(a[1] + a[3], a[2], 1)
})
fig_pdf("cd_inter_1", width = col_w, height = col_h, mar = col_mar, { scatter(); group_legend() })
fig_pdf("cd_inter_2", width = col_w, height = col_h, mar = col_mar, {
  scatter(); group_legend(); grp_line(b[1], b[2], 0)
})
fig_pdf("cd_inter_3", width = col_w, height = col_h, mar = col_mar, {
  scatter(); group_legend(); grp_line(b[1], b[2], 0); grp_line(b[1] + b[3], b[2] + b[4], 1)
})

# ---- where the coefficients appear: both lines drawn from court independence 0 to 4 -------------------
coef_base <- function() {
  frame_c(ylim = c(0, 100))
  fit_line(b[1], b[2], col_a)
  fit_line(b[1] + b[3], b[2] + b[4], col_d)
  group_legend(lines = TRUE)
}
bracket <- function(xx, y0, y1) arrows(xx, y0, xx, y1, length = 0.03, angle = 90, code = 3, col = col_dark, lwd = 1.2)
lab <- function(xx, yy, e, pos = 4, cex = 0.95) text(xx, yy, e, pos = pos, offset = 0.25, cex = cex)
ya1 <- b[1] + b[2] * 1        # autocracy line at court independence 1
slope_triangle <- function() {
  segments(1, ya1, 2, ya1, col = col_dark, lwd = 1.2)
  segments(2, ya1, 2, ya1 + b[2], col = col_dark, lwd = 1.2)
  text(1.5, ya1, "1", pos = 3, offset = 0.2, cex = 0.85)
  lab(2, ya1 + b[2] / 2, expression(hat(beta)[1]))
}
fig_pdf("cd_coef_1", width = col_w, height = col_h, mar = col_mar, {
  coef_base()
  bracket(0.12, 0, b[1]); points(0, b[1], pch = 16, col = col_a, xpd = NA)
  lab(0.12, b[1] / 2, expression(hat(beta)[0]))
})
fig_pdf("cd_coef_2", width = col_w, height = col_h, mar = col_mar, { coef_base(); slope_triangle() })
fig_pdf("cd_coef_3", width = col_w, height = col_h, mar = col_mar, {
  coef_base()
  points(c(0, 0), c(b[1], b[1] + b[3]), pch = 16, col = c(col_a, col_d), xpd = NA)
  segments(0, c(b[1], b[1] + b[3]), 0.55, col = col_grey, lty = 3)
  bracket(0.55, b[1], b[1] + b[3])
  lab(0.55, b[1] + 8, expression(hat(beta)[2]))
})
fig_pdf("cd_coef_4", width = col_w, height = col_h, mar = col_mar, {
  coef_base(); slope_triangle()
  # from the same starting point, a line with the democracies' slope (b1 + b3) ends b3 below:
  # b3 is the difference in slopes
  segments(1, ya1, 2, ya1 + b[2] + b[4], col = col_d, lwd = 1.5, lty = 2)
  segments(2, c(ya1 + b[2] + b[4], ya1 + b[2]), 3.0, col = col_grey, lty = 3)
  bracket(3.0, ya1 + b[2] + b[4], ya1 + b[2])
  lab(3.0, ya1 + b[2] + b[4], expression(hat(beta)[3]), pos = 1)   # below the bracket, clear of both lines
})

# ---- the fitted surface in 3-D ---------------------------------------------------------------------
gx <- seq(0, 4, length.out = 9)
gz <- seq(0, 1, length.out = 6)
surf <- outer(gx, gz, function(xx, zz) b[1] + b[2] * xx + b[3] * zz + b[4] * xx * zz)
zl <- c(0, 100)
zlab_left <- function(pm, xr, yr, zr, label, widest = "000") {
  cn <- expand.grid(x = xr, y = yr)
  p <- trans3d(cn$x, cn$y, mean(zr), pm)
  k <- which.min(p$x)
  off <- strwidth(widest, cex = 0.72) + strheight("M", cex = 0.85) * 1.6
  text(p$x[k] - off, p$y[k], label, srt = 90, cex = 0.85, xpd = NA)
}
surface_view <- function(theta, phi) {
  par(xpd = NA)
  pm <- persp(gx, gz, surf, xlim = c(0, 4), ylim = c(0, 1), zlim = zl,
              theta = theta, phi = phi, r = 3, expand = 0.75,
              col = NA, border = alpha_col(col_grey, 0.8), lwd = 0.6,
              ticktype = "detailed", nticks = 3, cex.axis = 0.72, cex.lab = 0.85,
              xlab = "Court", ylab = "Democracy", zlab = "")
  zlab_left(pm, c(0, 4), c(0, 1), zl, "Corruption")
  lines(trans3d(c(0, 4), c(0, 0), b[1] + b[2] * c(0, 4), pm), col = col_a, lwd = 2)
  lines(trans3d(c(0, 4), c(1, 1), b[1] + b[3] + (b[2] + b[4]) * c(0, 4), pm), col = col_d, lwd = 2)
  points(trans3d(d$courtind[aut], 0, d$execorr100[aut], pm), pch = 16, cex = 0.45, col = alpha_col(col_a, 0.7))
  points(trans3d(d$courtind[!aut], 1, d$execorr100[!aut], pm), pch = 16, cex = 0.45, col = alpha_col(col_d, 0.7))
}
sf_mar <- c(0.9, 1.3, 0.2, 0.6)
fig_pdf("cd_surface_1", width = col_w, height = col_h, mar = sf_mar, surface_view(30, 25))
fig_pdf("cd_surface_2", width = col_w, height = col_h, mar = sf_mar, surface_view(60, 20))
fig_pdf("cd_surface_3", width = col_w, height = col_h, mar = sf_mar, surface_view(130, 25))

# ---- no democracy main effect: both lines start at the same point at court independence 0 -----------
fig_pdf("cd_nomain", width = col_w, height = col_h, mar = col_mar, {
  scatter(); group_legend()
  fit_line(c0[1], c0[2], col_a)
  fit_line(c0[1], c0[2] + c0[3], col_d)
  points(0, c0[1], pch = 16, cex = 1.1, col = col_dark, xpd = NA)
})

# ---- marginal effects ---------------------------------------------------------------------------------
# 6.4 x 3.0 in fills the slide width at 0.82\textheight; small right margin on (a) and left margin on (b) keep
# the two panels close (Yiqing, 2026-10-11: the gap between them was too wide)
fig_pdf("cd_me", width = 6.4, height = 3.0, mar = c(3.2, 3.6, 1.7, 0.1), {
  layout(matrix(1:2, 1), widths = c(1.4, 2.4))
  # (a) court independence, by regime
  par(mar = c(3.2, 3.6, 1.7, 0.1))
  lo <- me_court - q * se_court; hi <- me_court + q * se_court
  plot(NA, xlim = c(0.45, 2.55), ylim = c(-30, 5), axes = FALSE, xlab = "",
       ylab = "Change in corruption per point")
  axis(1, at = 1:2, labels = c("Autocracies", "Democracies"), tick = FALSE, cex.axis = 0.8, gap.axis = -1)
  axis(2, at = seq(-30, 0, 10), labels = minus(seq(-30, 0, 10)))
  abline(h = 0, col = col_grey, lty = 2)
  arrows(1:2, lo, 1:2, hi, length = 0.04, angle = 90, code = 3, col = c(col_a, col_d), lwd = 1.6)
  points(1:2, me_court, pch = 16, cex = 1.2, col = c(col_a, col_d))
  text(1:2 + 0.08, me_court, minus(sprintf("%.1f", me_court)), pos = 4, cex = 0.8, offset = 0.2, xpd = NA)
  mtext("(a) Effect of court independence", side = 3, line = 0.5, cex = 0.8, adj = 0)
  # (b) democracy, across court independence, only where both groups have data
  par(mar = c(3.2, 3.2, 1.7, 0.6))
  xg <- seq(lo_common, hi_common, length.out = 200)
  me <- me_dem(xg); se <- se_dem(xg)
  plot(NA, xlim = c(0, 4), ylim = c(-60, 30), xaxs = "i", axes = FALSE,
       xlab = xlab_c, ylab = "Democracies − autocracies")
  axis(1, at = 0:4); axis(2, at = seq(-60, 30, 15), labels = minus(seq(-60, 30, 15)))
  polygon(c(xg, rev(xg)), c(me - q * se, rev(me + q * se)), col = alpha_col(col_cardinal, 0.18), border = NA)
  abline(h = 0, col = col_grey, lty = 2)
  lines(xg, me, col = col_cardinal, lwd = 2.2)
  rug(d$courtind[!aut], side = 3, ticksize = 0.04, col = col_d, lwd = 0.7)
  rug(d$courtind[aut], side = 1, ticksize = 0.04, col = col_a, lwd = 0.7)
  text(0.05, 26, "democracies", adj = c(0, 1), cex = 0.75, col = col_d)
  text(0.05, -56, "autocracies", adj = c(0, 0), cex = 0.75, col = col_a)
  mtext("(b) Effect of democracy", side = 3, line = 0.5, cex = 0.8, adj = 0)
})
cat("fig_court_democracy: done; numbers in", out, "\n")
