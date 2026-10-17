# fig_normal.R --- standard normal pictures for Lecture 2.2 (interval estimation).
#
#   figs/std_normal_cdf.pdf  CDF of N(0,1) with the points at -1.96 and 1.96
#                            (slide "CDF of the Standard Normal Distribution")
#   figs/z_upper_tail.pdf    N(0,1) density with the upper alpha/2 tail shaded,
#                            to define z_{alpha/2} (slide "Constructing a
#                            Confidence Interval with Known sigma^2", 2nd one)
#
# No data, no randomness. Run from the deck folder:
#   Rscript code/fig_normal.R

source("../course_figures.R")
source("code/lib/stable_pdf.R")   # stable_pdf(): fig_pdf() that skips unchanged figures

minus <- function(x) sub("-", "\u2212", format(x, trim = TRUE))  # typographic minus

# --- CDF of the standard normal -------------------------------------------
lo <- round(pnorm(-1.96), 3)            # .025
hi <- round(pnorm(1.96), 3)             # .975
cat(sprintf("Pr(Z <= -1.96) = %.4f   Pr(Z <= 1.96) = %.4f   difference = %.4f\n",
            pnorm(-1.96), pnorm(1.96), pnorm(1.96) - pnorm(-1.96)))

stable_pdf("std_normal_cdf", {
  plot(NA, xlim = c(-3.6, 3.6), ylim = c(0, 1), xaxs = "i", yaxs = "i",
       axes = FALSE, xlab = "z", ylab = "Pr(Z \u2264 z)")
  xt <- c(-3, -1.96, -1, 0, 1, 1.96, 3)
  axis(1, at = xt, labels = FALSE)
  axis(1, at = xt[!abs(xt) %in% 1.96], labels = minus(xt[!abs(xt) %in% 1.96]))
  axis(1, at = c(-1.96, 1.96), labels = minus(c(-1.96, 1.96)),
       col.axis = col_cardinal)
  axis(2, at = seq(0, 1, 0.25), labels = c("0", ".25", ".50", ".75", "1"))
  # reference lines
  abline(h = c(pnorm(-1.96), pnorm(1.96)), col = col_grey, lty = 2, lwd = 1)
  segments(c(-1.96, 1.96), 0, c(-1.96, 1.96), pnorm(c(-1.96, 1.96)),
           col = col_grey, lty = 2, lwd = 1)
  # the CDF
  curve(pnorm(x), from = -3.6, to = 3.6, n = 400, add = TRUE, lwd = 2.2, col = col_dark)
  points(c(-1.96, 1.96), pnorm(c(-1.96, 1.96)), pch = 19, cex = 1.1, col = col_cardinal)
  # values of the two reference lines, at their right ends
  text(3.55, pnorm(1.96) - 0.035, sprintf(".%03d", round(hi * 1000)), adj = c(1, 1),
       col = col_cardinal)
  text(3.55, pnorm(-1.96) + 0.035, sprintf(".%03d", round(lo * 1000)), adj = c(1, 0),
       col = col_cardinal)
  # bracket for the probability between the two points
  xb <- 2.75
  arrows(xb, pnorm(-1.96) + 0.01, xb, pnorm(1.96) - 0.01, code = 3, angle = 90,
         length = 0.05, col = col_cardinal, lwd = 1.2)
  text(xb + 0.1, 0.5, ".95", adj = c(0, 0.5), col = col_cardinal)
  # the arithmetic, in the empty upper-left corner
  x0 <- -3.4
  text(x0, 0.80, "Pr(Z \u2264 1.96) = .975", adj = c(0, 0.5))
  text(x0, 0.70, "Pr(Z \u2264 \u22121.96) = .025", adj = c(0, 0.5))
  text(x0, 0.57, "Pr(\u22121.96 < Z < 1.96)", adj = c(0, 0.5), col = col_cardinal)
  text(x0, 0.48, "   = .975 \u2212 .025 = .95", adj = c(0, 0.5), col = col_cardinal)
}, mar = c(3.2, 3.8, 0.6, 0.8))

# --- definition of z_{alpha/2} ---------------------------------------------
# Drawn at the size it is shown (inside a 0.4\textwidth column, about 2.36in),
# so 8.5pt text stays about 8.5pt on the slide. The cut-off is drawn at
# z = 1.645 (alpha = .10), as in the 2012 figure, only so the shaded tail is
# easy to see; the axis shows the symbol, not the number.
zc <- qnorm(0.95)
stable_pdf("z_upper_tail", {
  plot(NA, xlim = c(-3.4, 3.4), ylim = c(0, 0.43), xaxs = "i", yaxs = "i",
       axes = FALSE, xlab = "", ylab = "")
  xs <- seq(zc, 3.4, length.out = 200)
  polygon(c(zc, xs, 3.4), c(0, dnorm(xs), 0), col = alpha_col(col_cardinal, 0.55),
          border = NA)
  curve(dnorm(x), from = -3.4, to = 3.4, n = 400, add = TRUE, lwd = 1.6, col = col_dark)
  axis(1, at = c(-3.4, 3.4), labels = FALSE, lwd.ticks = 0)
  axis(1, at = 0, labels = "0", cex.axis = 0.95)
  axis(1, at = zc, labels = expression(italic(z)[alpha/2]), col.axis = col_cardinal,
       cex.axis = 1.05, padj = 0.1)
  arrows(2.75, 0.17, 2.15, 0.035, length = 0.04, col = col_cardinal, lwd = 1)
  text(2.75, 0.19, expression("area " * alpha/2), adj = c(0.5, 0), col = col_cardinal)
}, width = 2.4, height = 1.05, pointsize = 8.5, mar = c(1.7, 0.4, 0.2, 0.4))
