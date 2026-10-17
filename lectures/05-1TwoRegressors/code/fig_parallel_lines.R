# fig_parallel_lines.R --- Lecture 5-1, "Two Cases": executive corruption on high
# court independence with a binary covariate (democracy) and with a continuous
# covariate (log10 GDP per capita) shown as parallel lines.
#
# Data: code/data/countries_xsec.csv, one row per country in 2024, built in
# lectures/_data/countries/ (V-Dem v16, CC BY-SA 4.0; WDI, CC BY 4.0; see its README).
#   execorr100 100 x V-Dem v2x_execorr, executive corruption (0 = clean, 100 = most corrupt)
#   courtind   V-Dem v2juhcind (original scale), high court independence: how often the
#              high court's rulings in cases salient to the government merely reflect
#              government wishes, 0 = always ... 4 = never (country-expert ratings)
#   democracy  1 if V-Dem polyarchy > 0.5
#   gdppc      WDI GDP per capita, PPP, constant 2021 international $ (slides use log10)
# Sample: countries with all four (N = 168), the same rows in every model.
#
# Figures (projection-deck frames):
#   corr_court                 simple regression of corruption on court independence
#   corr_court_dem             democracies highlighted
#   corr_court_groups          both groups colored
#   dem_points / dem_line0 / dem_lines            the two parallel lines, step by step
#   dem_zoom_lines / dem_zoom_b0 / _b1 / _b2      where the three coefficients appear
#   gdp_lines_1..4             predicted corruption at four GDP levels (parallel lines)
# Numbers printed on the slides: code/parallel_lines_output.txt
#
# Run from the deck folder (v2/):  Rscript code/fig_parallel_lines.R

source("../course_figures.R")

d <- read.csv("code/data/countries_xsec.csv")
d <- d[complete.cases(d[, c("execorr100", "courtind", "democracy", "gdppc")]), ]
d$lgdp <- log10(d$gdppc)
stopifnot(nrow(d) == 168, sum(d$democracy) == 85)

fit1 <- lm(execorr100 ~ courtind, data = d)
fit2 <- lm(execorr100 ~ courtind + democracy, data = d)
fit3 <- lm(execorr100 ~ courtind + lgdp, data = d)
b1 <- coef(fit1); b2 <- coef(fit2); b3 <- coef(fit3)
# same fits as lectures/_data/countries/gates_court_output.txt (models 1 and 2)
stopifnot(abs(b1 - c(82.3273, -17.6637)) < 5e-5,
          abs(b2 - c(78.3236, -10.6301, -21.5101)) < 5e-5)

out <- "code/parallel_lines_output.txt"
sink(out)
cat("Lecture 5-1 v2: parallel lines. Data: V-Dem v16 + WDI, year 2024, N =", nrow(d),
    "| democracies:", sum(d$democracy), "| autocracies:", sum(1 - d$democracy), "\n\n")
cat("-- (1) execorr100 ~ courtind\n"); print(round(summary(fit1)$coef, 4))
cat(sprintf("R2 = %.4f\n\n", summary(fit1)$r.squared))
cat("-- (2) execorr100 ~ courtind + democracy\n"); print(round(summary(fit2)$coef, 4))
cat(sprintf("R2 = %.4f\n", summary(fit2)$r.squared))
cat(sprintf("autocracies: yhat = %.4f + (%.4f) courtind\n", b2[1], b2[2]))
cat(sprintf("democracies: yhat = %.4f + (%.4f) courtind   [b0 + b2 = %.4f]\n",
            b2[1] + b2[3], b2[2], b2[1] + b2[3]))
cat(sprintf("courtind coefficient: %.4f -> %.4f, shrinks by %.1f%%\n\n",
            b1[2], b2[2], 100 * (1 - b2[2] / b1[2])))
cat("Group means (courtind 0-4, execorr100):\n")
print(round(aggregate(cbind(courtind, execorr100) ~ democracy, data = d, FUN = mean), 2))
cat(sprintf("cor(courtind, democracy) = %.3f\n", cor(d$courtind, d$democracy)))
cat("\n-- (3) execorr100 ~ courtind + log10 GDP pc (the plane)\n"); print(round(summary(fit3)$coef, 4))
cat(sprintf("R2 = %.4f\n", summary(fit3)$r.squared))
levels_g <- c(3000, 10000, 30000, 100000)
cat("\nLines at four GDP levels (courtind from 0 to 4):\n")
for (g in levels_g) cat(sprintf("  GDP pc $%s (log10 %.2f): at courtind = 0: %.2f, at courtind = 4: %.2f\n",
                                format(g, big.mark = ",", scientific = FALSE), log10(g),
                                b3[1] + b3[3] * log10(g),
                                b3[1] + b3[3] * log10(g) + b3[2] * 4))
# For Lecture 5-2 (OVB on the same data): short = long + aux * dem, and sigma-hat of model (2)
aux <- lm(democracy ~ courtind, data = d)
cat(sprintf("\nFor 5-2, OVB identity (democracy omitted): short %.6f = long %.6f + aux %.6f x dem %.6f = %.6f | diff %.1e\n",
            b1[2], b2[2], coef(aux)[2], b2[3], b2[2] + coef(aux)[2] * b2[3],
            b1[2] - (b2[2] + coef(aux)[2] * b2[3])))
cat(sprintf("aux: democracy = %.6f + %.6f courtind (R2 %.4f) | bias term aux x dem = %.6f\n",
            coef(aux)[1], coef(aux)[2], summary(aux)$r.squared, coef(aux)[2] * b2[3]))
cat(sprintf("residual SE of execorr100 ~ courtind + democracy: %.4f on %d df (SSR %.1f)\n",
            summary(fit2)$sigma, fit2$df.residual, sum(residuals(fit2)^2)))
cat(sprintf("residual SE of execorr100 ~ courtind: %.4f on %d df\n", summary(fit1)$sigma, fit1$df.residual))
cat("SEs, long model (courtind, democracy):", round(summary(fit2)$coef[2:3, 2], 5),
    "| t:", round(summary(fit2)$coef[2:3, 3], 2), "| short-model courtind SE:", round(summary(fit1)$coef[2, 2], 5), "\n")
lab_names <- c("Denmark", "Nicaragua", "Papua New Guinea")
cat("\nLabelled countries:\n")
print(d[d$country %in% lab_names, c("country", "execorr100", "courtind", "democracy")], row.names = FALSE)
sink()

col_dem <- unname(pal["blue"])
col_aut <- col_cardinal
grp_col <- ifelse(d$democracy == 1, col_dem, col_aut)
lab_x <- "High court independence (0–4)"
lab_y <- "Executive corruption (0–100)"

# column figures: 7 cm wide, 6 cm wide (lines at GDP levels)
w7 <- 7 / 2.54; h7 <- 5.6 / 2.54
w6 <- 6 / 2.54; h6 <- 5.2 / 2.54
mar_col <- c(2.7, 2.9, 1.4, 0.4)   # top line holds the legend

minus <- function(x) sub("^-", "−", x)
num_lab <- function(v) minus(trimws(format(v)))
frame_xy <- function(xlim, ylim, xat, yat, xlab = lab_x, ylab = lab_y) {
  par(mgp = c(1.6, 0.35, 0), tcl = -0.2)
  plot(NA, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
       xlab = xlab, ylab = ylab, axes = FALSE)
  axis(1, at = xat, labels = num_lab(xat), cex.axis = 0.85)
  axis(2, at = yat, labels = num_lab(yat), cex.axis = 0.85)
  box(bty = "l")
}
top_legend <- function(labels, cols, lwd = NA, pch = 16) {
  u <- par("usr")
  legend(u[1], u[4], legend = labels, col = cols, pch = pch, lwd = lwd,
         horiz = TRUE, bty = "n", xpd = NA, yjust = 0, x.intersp = 0.6,
         text.width = NA, cex = 0.85, seg.len = 1.2, inset = 0)
}
group_legend <- function(lines = FALSE) {
  top_legend(c("Democracies", "Autocracies"), c(col_dem, col_aut),
             lwd = if (lines) 2 else NA, pch = if (lines) NA else 16)
}

xl <- c(-0.15, 4.1); yl <- c(-6, 100)  # margins: points near 0 on either axis do not sit on it
xat <- 0:4; yat <- seq(0, 100, 20)
pts <- function(col, alpha = 0.6, cex = 0.7) {
  points(d$courtind, d$execorr100, pch = 16, cex = cex, col = alpha_col(col, alpha))
}
simple_line <- function() {
  r <- range(d$courtind)
  segments(r[1], b1[1] + b1[2] * r[1], r[2], b1[1] + b1[2] * r[2], col = col_dark, lwd = 1.8)
}
group_line <- function(dem, lwd = 2, from = min(d$courtind), to = max(d$courtind)) {
  a <- b2[1] + dem * b2[3]
  segments(from, a + b2[2] * from, to, a + b2[2] * to,
           col = if (dem == 1) col_dem else col_aut, lwd = lwd)
}
label_countries <- function() {
  lab <- d[d$country %in% lab_names, ]
  up <- lab$country != "Denmark"
  pos <- c(Nicaragua = 4, "Papua New Guinea" = 2)[lab$country[up]]
  text(lab$courtind[up], lab$execorr100[up], lab$country[up], pos = pos, offset = 0.35, cex = 0.7, col = col_dark)
  dk <- lab[!up, ]   # Denmark sits in a crowd near 0: label it in the empty strip below 0
  text(dk$courtind - 0.08, -3.2, "Denmark", adj = c(1, 0.5), cex = 0.7, col = col_dark)
  segments(dk$courtind - 0.06, -3.2, dk$courtind, dk$execorr100 - 0.8, col = col_dark, lwd = 0.6)
}

# ---- simple regression, then the two groups --------------------------------
fig_pdf("corr_court", width = w7, height = h7, pointsize = 9, mar = mar_col, {
  frame_xy(xl, yl, xat, yat)
  pts(col_grey)
  simple_line()
  label_countries()
})
fig_pdf("corr_court_dem", width = w7, height = h7, pointsize = 9, mar = mar_col, {
  frame_xy(xl, yl, xat, yat)
  pts(ifelse(d$democracy == 1, col_dem, col_light), alpha = 1)
  simple_line()
  top_legend(c("Democracies", "Autocracies"), c(col_dem, col_light))
})
fig_pdf("corr_court_groups", width = w7, height = h7, pointsize = 9, mar = mar_col, {
  frame_xy(xl, yl, xat, yat)
  pts(grp_col)
  simple_line()
  group_legend()
})

# ---- add the two parallel lines ---------------------------------------------
for (k in 0:2) {
  nm <- c("dem_points", "dem_line0", "dem_lines")[k + 1]
  fig_pdf(nm, width = w7, height = h7, pointsize = 9, mar = mar_col, {
    frame_xy(xl, yl, xat, yat)
    pts(grp_col)
    if (k >= 1) group_line(0)
    if (k >= 2) group_line(1)
    group_legend()
  })
}

# ---- where the coefficients appear ------------------------------------------
zx <- c(-0.15, 4.1); zy <- c(-6, 105)   # headroom for the intercept label
lab_center <- function(l, cex) l$x + (0.5 - l$adj_x) * strwidth(l$lab, cex = cex)
label_back <- function(l, cex = 0.9) {   # white backing, drawn before the lines
  w <- strwidth(l$lab, cex = cex); h <- strheight(l$lab, cex = cex)
  cx <- lab_center(l, cex)
  rect(cx - w / 2 - 0.05, l$y - h / 2 - 1.5, cx + w / 2 + 0.05, l$y + h / 2 + 1.5,
       col = alpha_col("white", 0.9), border = NA)
}
label_text <- function(l, cex = 0.9) text(lab_center(l, cex), l$y, l$lab, cex = cex)
zoom <- function(step) {
  frame_xy(zx, zy, xat, yat)
  pts(grp_col, alpha = 0.3, cex = 0.6)
  y0 <- function(x) b2[1] + b2[2] * x            # autocracies
  y1 <- function(x) b2[1] + b2[3] + b2[2] * x    # democracies
  b1_lab <- bquote(hat(beta)[1] == .(minus(sprintf("%.1f", b2[2]))))
  lab_b0  <- list(x = 0.08, y = b2[1] + 7, adj_x = 0,
                  lab = bquote(hat(beta)[0] == .(sprintf("%.1f", b2[1]))))
  lab_b1a <- list(x = 1.25, y = y0(1.8) + 8, adj_x = 0, lab = b1_lab)
  lab_b1b <- list(x = 1.25, y = y1(1.8) + 8, adj_x = 0, lab = b1_lab)
  lab_b2  <- list(x = 0.38, y = (y0(0.3) + y1(0.3)) / 2 - 4, adj_x = 0,
                  lab = bquote(hat(beta)[2] == .(minus(sprintf("%.1f", b2[3])))))
  if (step >= 2) { label_back(lab_b1a); label_back(lab_b1b) }
  if (step >= 3) label_back(lab_b2)
  group_line(0, from = 0, to = 4)
  group_line(1, from = 0, to = 4)
  group_legend(lines = TRUE)
  if (step >= 1) {                               # intercept of the autocracy line
    points(0, b2[1], pch = 21, bg = col_aut, col = "white", cex = 1.3, xpd = NA)
    label_text(lab_b0)
  }
  if (step >= 2) {                               # slope: change per 1 point, both lines
    segments(c(2.5, 3.5), c(y0(2.5), y0(2.5)), c(3.5, 3.5), c(y0(2.5), y0(3.5)), col = col_dark, lwd = 1)
    text(3, y0(2.5) + 3, "1", cex = 0.8)
    segments(c(2.5, 3.5), c(y1(2.5), y1(2.5)), c(3.5, 3.5), c(y1(2.5), y1(3.5)), col = col_dark, lwd = 1)
    text(3, y1(2.5) + 3, "1", cex = 0.8)
    label_text(lab_b1a)
    label_text(lab_b1b)
  }
  if (step >= 3) {                               # vertical gap between the lines
    arrows(0.3, y0(0.3), 0.3, y1(0.3), length = 0.04, angle = 25, code = 3, col = col_dark, lwd = 1)
    label_text(lab_b2)
  }
  box(bty = "l")
}
fig_pdf("dem_zoom_lines", width = w7, height = h7, pointsize = 9, mar = mar_col, zoom(0))
fig_pdf("dem_zoom_b0",    width = w7, height = h7, pointsize = 9, mar = mar_col, zoom(1))
fig_pdf("dem_zoom_b1",    width = w7, height = h7, pointsize = 9, mar = mar_col, zoom(2))
fig_pdf("dem_zoom_b2",    width = w7, height = h7, pointsize = 9, mar = mar_col, zoom(3))

# ---- one line for each level of GDP per capita -------------------------------
# Labels sit at the right end of each line (a legend would cover the top line).
ramp <- c("#E3A3B8", "#C4476F", col_cardinal, "#4F001C")
g_lab <- c("$3k", "$10k", "$30k", "$100k")
for (k in seq_along(levels_g)) {
  fig_pdf(paste0("gdp_lines_", k), width = w6, height = h6, pointsize = 9,
          mar = c(2.7, 2.9, 0.5, 2.3), {
    frame_xy(c(-0.15, 4.1), c(0, 100), xat, yat, ylab = "Predicted corruption")
    for (j in seq_len(k)) {
      a <- b3[1] + b3[3] * log10(levels_g[j])
      segments(0, a, 4, a + b3[2] * 4, col = ramp[j], lwd = 1.3)
      text(4.15, a + b3[2] * 4, g_lab[j], adj = c(0, 0.5), cex = 0.75, col = ramp[j], xpd = NA)
    }
    mtext("GDP pc", side = 3, at = 4.15, adj = 0, line = -0.6, cex = 0.65, col = col_grey)
  })
}
cat("fig_parallel_lines: done; numbers in", out, "\n")
