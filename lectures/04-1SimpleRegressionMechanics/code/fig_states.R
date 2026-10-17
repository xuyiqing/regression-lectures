# fig_states.R --- running example of Lecture 4 (deck 4-1; decks 4-2 and 4-3 quote its numbers):
# Trump's share of the two-party presidential vote in 2024 (Y, percent) and the share of adults 25+
# with a bachelor's degree or higher (X, percent, ACS 2019-23), 50 U.S. states. DC is excluded.
#
# Replaces fig_beo.R (Black elected officials, 41 states, source unknown) in the 2026-10-11 examples
# rebuild. The slope compares states; it is not the effect of education on anyone's vote.
#
# Data: code/data/state_pres.csv, a copy of lectures/_data/us_elections/state_pres.csv (built by
# _data/us_elections/build.R; sources and license in that folder's README.md):
#   rep2p_2024  Trump / (Trump + Harris), from MEDSL "U.S. President 1976-2024" (doi:10.7910/DVN/42MVDX,
#               CC0), official state totals
#   pct_ba      100 x (B15003_022 + 023 + 024 + 025) / B15003_001, ACS 2019-2023 5-year (public domain)
#
# Run from the deck folder:  Rscript code/fig_states.R
#
# Writes: code/states_output.txt (every number the 4-1, 4-2 and 4-3 slides print about this example)
# and figs/states_*.pdf.

source("../course_figures.R")
suppressPackageStartupMessages(library(lmtest))

# ------------------------------------------------------------------- data
raw <- read.csv("code/data/state_pres.csv", colClasses = c(state_fips = "character"))
stopifnot(nrow(raw) == 51, sum(raw$is_dc) == 1)
states <- data.frame(state = raw$state, st = raw$state_po,
                     trump = 100 * raw$rep2p_2024, ba = raw$pct_ba)[raw$is_dc == 0, ]
rownames(states) <- states$st
stopifnot(nrow(states) == 50, !anyNA(states))

# ------------------------------------------------- regression and slide text
options(useFancyQuotes = FALSE, width = 80, digits = 7)
lm.out <- lm(trump ~ ba, data = states)
s <- summary(lm.out)
cf <- coef(s)
b0 <- cf[1, 1]; b1 <- cf[2, 1]
n <- nrow(states); dfr <- s$df[2]
# the gate numbers (lectures/_data/us_elections/gates_output.txt)
stopifnot(round(b1, 4) == -1.4537, round(cf[2, 2], 4) == 0.1738, round(b0, 4) == 102.8376,
          round(s$r.squared, 3) == 0.593, dfr == 48)

fit <- fitted(lm.out); res <- resid(lm.out)
x <- states$ba; y <- states$trump
ybar <- mean(y); xbar <- mean(x)
sst <- sum((y - ybar)^2); sse <- sum((fit - ybar)^2); ssr <- sum(res^2)
sxx <- sum((x - xbar)^2); sxy <- sum((x - xbar) * (y - ybar))
stopifnot(abs(sst - sse - ssr) < 1e-8, abs(sxy / sxx - b1) < 1e-10,
          abs(cov(x, y) / var(x) - b1) < 1e-10, abs(sum(x * res)) < 1e-8)
tcrit <- qt(0.975, df = dfr); tcrit1 <- qt(0.95, df = dfr)
rs <- resettest(lm.out, power = 2:3, type = "fitted")
bp <- bptest(lm.out)
cd <- cooks.distance(lm.out)
ord <- order(-abs(res))
wy <- which(states$st == "WY")

# with DC, for reference only (not on the slides)
dc <- lm(I(100 * rep2p_2024) ~ pct_ba, data = raw)

f4 <- function(v) sprintf("%.4f", v)
out <- c(
  "# fig_states.R output: the running example of Lecture 4 (4-1, 4-2, 4-3)",
  "# Y = trump: Trump's % of the two-party vote, 2024 (MEDSL state totals)",
  "# X = ba: % of adults 25+ with a bachelor's degree or higher (ACS 2019-23 5-year)",
  "# 50 states; DC excluded",
  "",
  "> lm.out <- lm(trump ~ ba, data = states)",
  "> summary(lm.out)",
  capture.output(s),
  "",
  "> confint(lm.out)",
  capture.output(confint(lm.out)),
  "",
  "> vcov(lm.out)",
  capture.output(vcov(lm.out)),
  "",
  "# ---- numbers used on the slides (4-1: fit, residuals, SSR surface, ANOVA, R^2)",
  sprintf("n = %d; residual df = %d", n, dfr),
  sprintf("b0 = %.4f (SE %.4f, t %.3f); b1 = %.4f (SE %.4f, t %.3f)",
          b0, cf[1, 2], cf[1, 3], b1, cf[2, 2], cf[2, 3]),
  sprintf("b1 per 5 points of X = %.2f; per 10 points = %.2f", 5 * b1, 10 * b1),
  sprintf("mean(trump) = %.4f; mean(ba) = %.4f; min/max ba = %.2f / %.2f; min/max trump = %.2f / %.2f",
          ybar, xbar, min(x), max(x), min(y), max(y)),
  sprintf("sd(trump) = %.4f; sd(ba) = %.4f; cor(trump, ba) = %.4f; cor^2 = %.4f",
          sd(y), sd(x), cor(x, y), cor(x, y)^2),
  sprintf("Sxx = sum((x - xbar)^2) = %.4f; Sxy = %.4f; Sxy/Sxx = %.4f", sxx, sxy, sxy / sxx),
  sprintf("cov(ba, trump) = %.4f; var(ba) = %.4f; ratio = %.4f", cov(x, y), var(x), cov(x, y) / var(x)),
  sprintf("SST = %.2f; SSE = %.2f; SSR = %.2f; SSE/SST = %.4f; 1 - SSR/SST = %.4f",
          sst, sse, ssr, sse / sst, 1 - ssr / sst),
  sprintf("R2 = %.4f; adj R2 = %.4f; residual SE (sigma-hat) = %.4f; sigma-hat^2 = SSR/(n-2) = %.4f",
          s$r.squared, s$adj.r.squared, s$sigma, ssr / dfr),
  sprintf("F = %.2f on 1 and %d df", s$fstatistic[["value"]], dfr),
  sprintf("Wyoming: X = %.2f, Y = %.2f, fitted = %.2f, residual = %.2f; Y - Ybar = %.2f; fitted - Ybar = %.2f",
          x[wy], y[wy], fit[wy], res[wy], y[wy] - ybar, fit[wy] - ybar),
  paste0("largest residuals (pp): ",
         paste(sprintf("%s %+.1f", states$st[ord[1:6]], res[ord[1:6]]), collapse = ", ")),
  "",
  "# ---- for 4-2 (variance of the estimators) and 4-3 (tests and intervals)",
  sprintf("SE(b1) by hand = sqrt(sigma2 / Sxx) = sqrt(%.4f / %.4f) = %.4f", ssr / dfr, sxx, sqrt(ssr / dfr / sxx)),
  sprintf("SE(b0) by hand = sqrt(sigma2 * (1/n + xbar^2/Sxx)) = %.4f", sqrt(ssr / dfr * (1 / n + xbar^2 / sxx))),
  sprintf("t critical value qt(0.975, %d) = %.4f; one-sided qt(0.95, %d) = %.4f", dfr, tcrit, dfr, tcrit1),
  sprintf("t for H0: b1 = 0: %.4f / %.4f = %.2f; two-sided p = %.3g", b1, cf[2, 2], cf[2, 3], cf[2, 4]),
  sprintf("95%% CI by hand: %.4f +/- %.4f * %.4f = [%.4f, %.4f]", b1, tcrit, cf[2, 2],
          b1 - tcrit * cf[2, 2], b1 + tcrit * cf[2, 2]),
  sprintf("RESET (powers 2:3 of fitted): F = %.3f, p = %.3f", rs$statistic, rs$p.value),
  sprintf("Breusch-Pagan (studentized): BP = %.3f, p = %.3f", bp$statistic, bp$p.value),
  sprintf("max Cook's D = %.3f (%s); next: %s", max(cd), states$state[which.max(cd)],
          paste(sprintf("%s %.3f", states$state[order(-cd)[2:4]], sort(cd, decreasing = TRUE)[2:4]),
                collapse = ", ")),
  "",
  "# ---- reference only: with DC (N = 51)",
  sprintf("b0 = %.4f, b1 = %.4f (SE %.4f), R2 = %.4f", coef(dc)[1], coef(dc)[2],
          coef(summary(dc))[2, 2], summary(dc)$r.squared)
)
writeLines(out, "code/states_output.txt")

# ------------------------------------------------------------------ figures
xl <- c(20, 50); yl <- c(26, 80)
fmt <- function(v, d) sub("^-", "−", formatC(v, format = "f", digits = d))
xlab_st <- "X: % of adults 25+ with a bachelor's degree, 2019–23"
ylab_st <- "Y: Trump % of two-party vote, 2024"
col_pt <- col_dark
col_fit <- col_cardinal
col_res <- pal[["blue"]]
col_pred <- pal[["orange"]]

base_plot <- function(...) {
  plot(x, y, type = "n", xlim = xl, ylim = yl, xaxs = "i",
       xlab = xlab_st, ylab = ylab_st, axes = FALSE, ...)
  axis(1, at = seq(20, 50, 5)); axis(2, at = seq(30, 80, 10))
  box(bty = "l")
}
pts <- function(col = col_pt, cex = 0.85) points(x, y, pch = 16, col = col, cex = cex)
fit_line <- function(lwd = 2) {
  clip(xl[1], xl[2], yl[1], yl[2])
  abline(b0, b1, col = col_fit, lwd = lwd)
  do.call(clip, as.list(par("usr")))
}
eq_label <- function(xx = 49.5, yy = 77) {
  text(xx, yy, bquote({hat(Y) == hat(beta)[0] + hat(beta)[1] * X} ==
                        .(fmt(b0, 2)) - .(fmt(abs(b1), 2)) * X),
       adj = c(1, 0.5), col = col_fit, cex = 1)
}
# labelled states: the six largest residuals and four familiar ones
lab_pos <- c(WY = 3, ND = 4, UT = 4, HI = 1, NM = 1, NV = 1, WV = 3, MA = 1, CA = 1, TX = 1)
state_labels <- function(col = col_grey, cex = 0.78, which = names(lab_pos)) {
  k <- match(which, states$st)
  text(x[k], y[k], states$st[k], pos = lab_pos[which], offset = 0.35, col = col, cex = cex)
}

# p. 4: the data
fig_pdf("states_scatter", {
  base_plot()
  pts()
  state_labels()
  text(49.5, 29.5, "50 U.S. states (DC excluded)", adj = c(1, 0.5), col = col_grey, cex = 0.8)
})

# pp. 5, 40: the OLS fit
fig_pdf("states_fit", {
  base_plot()
  fit_line()
  pts()
  eq_label()
  text(49.5, 73.3, bquote(hat(beta)[0] == .(fmt(b0, 2)) * ": the line's value at X = 0, far left of the data"),
       adj = c(1, 0.5), col = col_fit, cex = 0.8)
  # slope triangle over 5 units of X, below the line where there are no points
  x1 <- 45; x2 <- 50
  segments(x1, b0 + b1 * x1, x1, b0 + b1 * x2, col = col_grey, lwd = 1)
  segments(x1, b0 + b1 * x2, x2, b0 + b1 * x2, col = col_grey, lwd = 1)
  text((x1 + x2) / 2, b0 + b1 * x2 - 1.3, "+5", col = col_grey, cex = 0.8)
  text(x1 + 0.25, b0 + b1 * (x1 + 0.6 * (x2 - x1)), fmt(5 * b1, 1),
       adj = c(0, 0.5), col = col_grey, cex = 0.8)
  text(x1 - 0.4, b0 + b1 * x2 + 0.5, bquote("slope " * hat(beta)[1] == .(fmt(b1, 2))),
       adj = c(1, 0.5), col = col_fit, cex = 0.85)
})

# p. 6: residuals
fig_pdf("states_residuals", {
  base_plot()
  segments(x, fit, x, y, col = col_res, lwd = 1.4)
  fit_line()
  points(x, fit, pch = 22, bg = "white", col = col_fit, cex = 0.6)
  pts()
  segments(x[wy], fit[wy], x[wy], y[wy], col = col_res, lwd = 3)
  text(x[wy], y[wy], "WY", pos = 3, offset = 0.35, col = col_grey, cex = 0.78)
  text(x[wy] - 0.4, (y[wy] + fit[wy]) / 2, expression(hat(u)[i]), adj = c(1, 0.5), col = col_res)
  text(33.5, 78, expression("residual " * hat(u)[i] == Y[i] - hat(Y)[i]), adj = c(0, 0.5), col = col_res)
  text(33.5, 74.5, expression("= part of " * Y[i] * " not predicted by the line"),
       adj = c(0, 0.5), col = col_res, cex = 0.9)
  text(33.5, 68.5, expression("OLS picks " * hat(beta)[0] * ", " * hat(beta)[1] *
                              " to minimize " * sum(hat(u)[i]^2, i == 1, n)),
       adj = c(0, 0.5), col = col_fit)
  legend("bottomleft", bty = "n", cex = 0.8,
         legend = c("observed values", "fitted values", "residuals"),
         pch = c(16, 22, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 1.4),
         col = c(col_pt, col_fit, col_res), pt.bg = "white")
})

# pp. 41-43: decomposing one observation (Wyoming); identical axes in all three
xa <- x[wy]; ya <- y[wy]; fa <- fit[wy]
halo_text <- function(xx, yy, lab, adj = c(0, 0.5), cex = 1, ...) {
  w <- strwidth(lab, cex = cex); h <- strheight(lab, cex = cex)
  x0 <- xx - adj[1] * w; y0 <- yy - adj[2] * h
  rect(x0 - 0.2, y0 - 0.5 * h, x0 + w + 0.2, y0 + 1.5 * h, col = "white", border = NA)
  text(xx, yy, lab, adj = adj, cex = cex, ...)
}
margin_label <- function(yy, sym, words, xx = 50.8, above = FALSE) {
  text(xx, yy + 0.3, sym, adj = c(0, 0.5), xpd = TRUE)
  text(xx, if (above) yy + 3.6 else yy - 3.0, words, adj = c(0, 0.5), xpd = TRUE, cex = 0.8, col = col_grey)
}
decomp <- function(step) {
  par(mar = c(3.2, 3.6, 1.0, 6.0))
  base_plot()
  abline(h = ybar, lty = 2, col = col_grey)
  fit_line(lwd = 1.6)
  pts(col = col_light)
  points(xa, ya, pch = 16, col = col_pt, cex = 1.1)
  text(xa, ya, "WY", pos = 3, offset = 0.4, col = col_grey, cex = 0.78)
  if (step == 1) {
    segments(xa, ybar, xa, ya, col = pal[["green"]], lwd = 3)
    halo_text(xa - 0.5, (ya + ybar) / 2, expression(Y[i] - bar(Y)), adj = c(1, 0.5), col = pal[["green"]])
    text(33, 78, expression(Y[i] - bar(Y) * ": the part of " * Y[i]), adj = c(0, 0.5), col = pal[["green"]], cex = 0.9)
    text(33, 74.5, "not predicted by the average", adj = c(0, 0.5), col = pal[["green"]], cex = 0.9)
  } else {
    segments(xa, fa, xa, ya, col = col_res, lwd = 3)
    segments(xa, ybar, xa, fa, col = col_pred, lwd = 3)
    points(xa, fa, pch = 22, bg = "white", col = col_fit, cex = 1.1)
    halo_text(xa - 0.5, (ya + fa) / 2, expression(hat(u)[i] == Y[i] - hat(Y)[i]),
              adj = c(1, 0.5), col = col_res)
    text(33, 78, expression("residual " * hat(u)[i] * ": the part of " * Y[i]),
         adj = c(0, 0.5), col = col_res, cex = 0.9)
    text(33, 74.5, "not predicted by the regression", adj = c(0, 0.5), col = col_res, cex = 0.9)
    if (step == 3) {
      halo_text(xa + 0.5, (fa + ybar) / 2, expression(hat(Y)[i] - bar(Y)), adj = c(0, 0.5), col = col_pred)
      text(33, 69.5, expression(hat(Y)[i] - bar(Y) * ": the part of " * Y[i] * " predicted"),
           adj = c(0, 0.5), col = col_pred, cex = 0.9)
      text(33, 66, "by the regression, beyond the average", adj = c(0, 0.5), col = col_pred, cex = 0.9)
    }
    margin_label(fa, expression(hat(Y)[i]), "fitted value", above = TRUE)
  }
  margin_label(ya, expression(Y[i]), "observed value")
  margin_label(ybar, expression(bar(Y)), "average outcome")
}
fig_pdf("states_decomp1", decomp(1))
fig_pdf("states_decomp2", decomp(2))
fig_pdf("states_decomp3", decomp(3))

# p. 45: the three sums of squares, side by side on identical axes
fig_pdf("states_sumsq", width = 6, height = 2.4, pointsize = 10,
        mar = c(2.9, 2.8, 3.0, 0.4), {
  par(mfrow = c(1, 3), mgp = c(1.7, 0.5, 0), cex = 1)
  panel <- function(y0, y1, col, title, value) {
    plot(x, y, type = "n", xlim = xl, ylim = yl, xaxs = "i",
         xlab = "X", ylab = "Y", axes = FALSE)
    axis(1, at = seq(20, 50, 10)); axis(2, at = seq(30, 70, 20))
    box(bty = "l")
    abline(h = ybar, lty = 2, col = col_grey)
    segments(x, y0, x, y1, col = col, lwd = 1.1)
    clip(xl[1], xl[2], yl[1], yl[2]); abline(b0, b1, col = col_fit, lwd = 1.4)
    do.call(clip, as.list(par("usr")))
    points(x, y, pch = 16, cex = 0.5, col = col_pt)
    mtext(title, side = 3, line = 1.25, cex = 0.9, col = col)
    mtext(sprintf("= %s", formatC(value, format = "f", digits = 0, big.mark = ",")),
          side = 3, line = 0.2, cex = 0.9, col = col)
  }
  panel(ybar, y, pal[["green"]], expression(SST == Sigma(y[i] - bar(y))^2), sst)
  panel(ybar, fit, col_pred, expression(SSE == Sigma(hat(y)[i] - bar(y))^2), sse)
  panel(fit, y, col_res, expression(SSR == Sigma(y[i] - hat(y)[i])^2), ssr)
})

# pp. 22-24: the least-squares objective S(b0, b1) for this example
S2 <- function(a, b) sum((y - a - b * x)^2)
k <- 3.2
b0g <- seq(b0 - k * cf[1, 2], b0 + k * cf[1, 2], length.out = 501)
b1g <- seq(b1 - k * cf[2, 2], b1 + k * cf[2, 2], length.out = 501)
z <- outer(b0g, b1g, Vectorize(S2))
smin <- ssr
ramp <- grDevices::colorRampPalette(c(col_cardinal, "#E7A9BE", "#F7E8EE", "#FFFFFF"))
data_note <- "50 U.S. states"

fig_pdf("states_ssr_surface", mar = c(0.2, 0.2, 0.2, 0.2), {
  g0 <- seq(min(b0g), max(b0g), length.out = 33)
  g1 <- seq(min(b1g), max(b1g), length.out = 33)
  zz <- outer(g0, g1, Vectorize(S2))
  zf <- (zz[-1, -1] + zz[-1, -ncol(zz)] + zz[-nrow(zz), -1] + zz[-nrow(zz), -ncol(zz)]) / 4
  nb <- 40
  cut_z <- cut(zf, breaks = quantile(zz, seq(0, 1, length.out = nb + 1)), include.lowest = TRUE)
  pm <- persp(g0, g1, zz / 1000, theta = 35, phi = 24, expand = 0.72, r = 4,
              col = ramp(nb)[cut_z], border = adjustcolor(col_dark, 0.18), lwd = 0.3,
              box = TRUE, axes = TRUE, ticktype = "detailed", nticks = 4,
              xlab = "", ylab = "", zlab = "", cex.axis = 0.7)
  lab3d <- function(xx, yv, zv, lab, ...) {
    p <- trans3d(xx, yv, zv, pm); text(p$x, p$y, lab, ...)
  }
  lab3d(mean(g0), min(g1) - 0.30 * diff(range(g1)), min(zz) / 1000, expression(tilde(beta)[0]), cex = 1.1)
  lab3d(max(g0) + 0.12 * diff(range(g0)), mean(g1), min(zz) / 1000, expression(tilde(beta)[1]), cex = 1.1)
  p0 <- trans3d(b0, b1, smin / 1000, pm)
  points(p0$x, p0$y, pch = 21, bg = pal[["blue"]], col = "white", cex = 1.3)
  usr <- par("usr")
  text(usr[1] + 0.02 * diff(usr[1:2]), usr[4] - 0.05 * diff(usr[3:4]),
       expression(S(tilde(beta)[0], tilde(beta)[1]) ~ "(thousands)"), adj = c(0, 1), cex = 1)
  text(usr[2], usr[3] + 0.03 * diff(usr[3:4]), data_note, adj = c(1, 0), col = col_grey, cex = 0.8)
})

# contours: raster fill in bands (no seams) with vector contour lines on top
lev_line <- c(2200, 2600, 3200, 4000, 5500, 7500, 10000, 13000, 16500, 20500)
lev_fill <- c(smin - 1, lev_line, max(z) + 1)
lev_lab  <- c(4000, 7500, 13000)
boxed_text <- function(xx, yy, lab, adj = c(0, 0.5), cex = 1, pad = 0.4, ...) {
  w <- strwidth(lab, cex = cex); h <- strheight(lab, cex = cex)
  px <- pad * strwidth("M", cex = cex); py <- pad * strheight("M", cex = cex)
  x0 <- xx - adj[1] * w; y0 <- yy - adj[2] * h
  rect(x0 - px, y0 - py, x0 + w + px, y0 + h + py, col = "white", border = col_grey, lwd = 0.5)
  text(x0, y0 + h / 2, lab, adj = c(0, 0.5), cex = cex, ...)
}
contour_frame <- function() {
  plot(NA, xlim = range(b0g), ylim = range(b1g), xaxs = "i", yaxs = "i",
       xlab = expression(tilde(beta)[0]), ylab = expression(tilde(beta)[1]))
  image(b0g, b1g, z, breaks = lev_fill, col = ramp(length(lev_fill) - 1),
        useRaster = TRUE, add = TRUE)
  contour(b0g, b1g, z, levels = setdiff(lev_line, lev_lab), add = TRUE,
          col = col_dark, lwd = 0.5, drawlabels = FALSE)
  contour(b0g, b1g, z, levels = lev_lab, add = TRUE, col = col_dark, lwd = 0.5,
          labcex = 0.75, method = "flattest")
  box(bty = "o")
  usr <- par("usr")
  boxed_text(usr[2] - 0.03 * diff(usr[1:2]), usr[4] - 0.10 * diff(usr[3:4]), data_note,
             adj = c(1, 0), cex = 0.8, col = col_dark)
}
fig_pdf("states_ssr_contour", mar = c(3.2, 3.8, 1.0, 0.8), contour_frame())
fig_pdf("states_ssr_contour_min", mar = c(3.2, 3.8, 1.0, 0.8), {
  contour_frame()
  abline(v = b0, h = b1, lty = 2, col = pal[["blue"]], lwd = 1.2)
  points(b0, b1, pch = 21, bg = pal[["blue"]], col = "white", cex = 1.5)
  boxed_text(b0 + 0.04 * diff(range(b0g)), b1 + 0.20 * diff(range(b1g)),
             bquote((list(hat(beta)[0], hat(beta)[1])) ==
                      (list(.(fmt(b0, 2)), .(fmt(b1, 2))))),
             adj = c(0, 0.5), cex = 0.95, col = col_dark)
  boxed_text(b0 + 0.04 * diff(range(b0g)), b1 + 0.10 * diff(range(b1g)),
             bquote("minimum " * S == .(formatC(smin, format = "f", digits = 0, big.mark = ","))),
             adj = c(0, 0.5), cex = 0.95, col = col_dark)
})

cat(out, sep = "\n")
