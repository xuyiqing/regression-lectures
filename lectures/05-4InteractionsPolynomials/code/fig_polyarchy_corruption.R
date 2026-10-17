# fig_polyarchy_corruption.R --- Lecture 5-4, polynomials: executive corruption on electoral democracy and its
# square (V-Dem v16, 2024). Replaces fig_age_income.R (1996 ANES) and the Persson-Tabellini GOVEXP^2 R output in
# v2, 2026-10-11. Same outcome as the running example of Lectures 5-1 and 5-2.
#
# Data: code/data/countries_xsec.csv (lectures/_data/countries/; V-Dem v16, CC BY-SA 4.0).
#   execorr100  100 x V-Dem v2x_execorr, executive corruption (0 = clean, 100 = most corrupt)
#   polyarchy   V-Dem v2x_polyarchy, electoral democracy index (0-1)
# Sample: the 179 countries with both, 2024.
#
#   poly_corr_1  scatter
#   poly_corr_2  + linear fit
#   poly_corr_3  + quadratic fit (linear dashed) + binned means (deciles of polyarchy, squares)
# Numbers printed on the slides, and the R output on the "Dealing with Polynomials in R" frame:
# code/polyarchy_corruption_output.txt
#
# Run from the deck folder (v2/):  Rscript code/fig_polyarchy_corruption.R

source("../course_figures.R")

x <- read.csv("code/data/countries_xsec.csv")
d <- x[complete.cases(x[, c("execorr100", "polyarchy")]), c("country", "execorr100", "polyarchy")]
stopifnot(nrow(d) == 179)
fit1 <- lm(execorr100 ~ polyarchy, data = d)
fit2 <- lm(execorr100 ~ polyarchy + I(polyarchy^2), data = d)
fit3 <- lm(execorr100 ~ polyarchy + I(polyarchy^2) + I(polyarchy^3), data = d)
b <- coef(fit2)
tp <- -b[[2]] / (2 * b[[3]])
bins <- cut(d$polyarchy, quantile(d$polyarchy, 0:10 / 10), include.lowest = TRUE)
bm <- data.frame(x = tapply(d$polyarchy, bins, mean), y = tapply(d$execorr100, bins, mean), n = as.vector(table(bins)))

out <- "code/polyarchy_corruption_output.txt"
sink(out)
cat("Executive corruption on electoral democracy, V-Dem v16, 2024, N =", nrow(d), "\n\n")
cat("-- linear\n"); print(round(summary(fit1)$coef, 3)); cat(sprintf("R2 %.3f\n", summary(fit1)$r.squared))
cat("\n-- the R output on the slide:\n> summary(lm(execorr100 ~ polyarchy + I(polyarchy^2), data = d))\n")
print(summary(fit2))
cat(sprintf("turning point -b1/(2 b2) = %.3f ; fitted value there %.1f\n", tp, predict(fit2, data.frame(polyarchy = tp))))
cat(sprintf("slope b1 + 2 b2 x at x = 0.1: %.1f ; 0.5: %.1f ; 0.9: %.1f\n", b[[2]] + 2 * b[[3]] * 0.1, b[[2]] + 2 * b[[3]] * 0.5, b[[2]] + 2 * b[[3]] * 0.9))
cat(sprintf("R2: linear %.3f, quadratic %.3f, cubic %.3f\n", summary(fit1)$r.squared, summary(fit2)$r.squared, summary(fit3)$r.squared))
cat(sprintf("cubic term t = %.2f (p = %.2f)\n", summary(fit3)$coef[4, 3], summary(fit3)$coef[4, 4]))
cat("\nBinned means (deciles of polyarchy):\n")
print(data.frame(bin = levels(bins), mean_polyarchy = round(bm$x, 3), mean_corruption = round(bm$y, 1), n = bm$n), row.names = FALSE)
cat("Countries with polyarchy below the turning point:", sum(d$polyarchy < tp), "\n")
sink()

col_lin <- pal[["blue"]]; col_quad <- col_cardinal
pg <- seq(min(d$polyarchy), max(d$polyarchy), length.out = 200)
step <- function(k) {
  plot(d$polyarchy, d$execorr100, xlim = c(0, 1), ylim = c(0, 100), pch = 16, cex = 0.4,
       col = alpha_col(col_grey, 0.6), xlab = "Electoral democracy (0–1)", ylab = "Executive corruption")
  if (k >= 2) lines(pg, predict(fit1, data.frame(polyarchy = pg)), col = col_lin, lwd = 2.2, lty = if (k == 3) 2 else 1)
  if (k >= 3) {
    lines(pg, predict(fit2, data.frame(polyarchy = pg)), col = col_quad, lwd = 2.4)
    points(bm$x, bm$y, pch = 22, cex = 0.9, bg = "white", col = col_dark, lwd = 1.2)
  }
  on <- c(k >= 2, k >= 3, k >= 3)
  legend("top", inset = c(0, -0.2), xpd = NA, horiz = TRUE, bty = "n", cex = 0.8,
         legend = c("linear", "quadratic", "bin means"), lwd = c(2.2, 2.2, NA), seg.len = 1.2,
         lty = c(if (k == 3) 2 else 1, 1, NA), pch = c(NA, NA, 22), pt.bg = "white",
         col = ifelse(on, c(col_lin, col_quad, col_dark), NA),
         text.col = ifelse(on, col_dark, alpha_col("white", 0)), x.intersp = 0.4,
         text.width = strwidth("quadratic ") * 0.8)
}
for (k in 1:3) fig_pdf(paste0("poly_corr_", k), width = 2.7, height = 2.5, mar = c(3.0, 3.0, 1.9, 0.5), step(k))
cat("fig_polyarchy_corruption: done; numbers in", out, "\n")
