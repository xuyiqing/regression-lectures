# fig_partialling_out.R --- Lecture 5-1, "Partialling Out": the steps behind the
# Frisch-Waugh-Lovell result in the running example, with X = high court independence
# (0-4) and Z = log10 GDP per capita (the same model as the regression plane).
#
# Data: code/data/countries_xsec.csv, 2024, N = 168 (see fig_parallel_lines.R).
#
# Figures (all 6 x 2.8 in):
#   fwl_corr_court          corruption on court independence
#   fwl_court_gdp           court independence on log10 GDP per capita
#   fwl_corr_gdp            corruption on log10 GDP per capita
#   fwl_corr_court_partial  simple slope vs slope holding GDP fixed
#   fwl_court_gdp_resid     + the residuals of court independence on GDP
#   fwl_corr_resid          corruption on those residuals
# Court independence (first and fourth) and its residual (last) get x-axes of
# equal width, so the two slopes can be compared by eye.
# Numbers printed on the slides, and the R output on the three code slides:
# code/partialling_out_output.txt
#
# Run from the deck folder (v2/):  Rscript code/fig_partialling_out.R

source("../course_figures.R")

d <- read.csv("code/data/countries_xsec.csv")
d <- d[complete.cases(d[, c("execorr100", "courtind", "democracy", "gdppc")]), ]
d$lgdp <- log10(d$gdppc)
stopifnot(nrow(d) == 168)
f_simple <- lm(execorr100 ~ courtind, data = d)
f_aux    <- lm(courtind ~ lgdp, data = d)
f_cgdp   <- lm(execorr100 ~ lgdp, data = d)
f_multi  <- lm(execorr100 ~ courtind + lgdp, data = d)
d$res_court_gdp <- residuals(f_aux)
f_resid  <- lm(execorr100 ~ res_court_gdp, data = d)
stopifnot(abs(coef(f_resid)[2] - coef(f_multi)[2]) < 1e-10,     # the FWL result
          abs(coef(f_simple) - c(82.3273, -17.6637)) < 5e-5)

out <- "code/partialling_out_output.txt"
sink(out)
cat("Lecture 5-1 v2: partialling out (FWL). V-Dem v16 + WDI, 2024, N =", nrow(d), "\n\n")
show <- function(lab, f) {
  cat("--", lab, "\n"); print(round(summary(f)$coef, 4))
  cat(sprintf("R2 = %.4f\n\n", summary(f)$r.squared))
}
show("execorr100 ~ courtind", f_simple)
show("courtind ~ lgdp (auxiliary)", f_aux)
show("execorr100 ~ lgdp", f_cgdp)
show("execorr100 ~ courtind + lgdp", f_multi)
show("execorr100 ~ res_court_gdp", f_resid)
cat(sprintf("FWL: slope on residual %.10f | long slope %.10f | diff %.2e\n",
            coef(f_resid)[2], coef(f_multi)[2], coef(f_resid)[2] - coef(f_multi)[2]))
cat(sprintf("intercept of the residual regression = mean(execorr100) = %.4f\n", mean(d$execorr100)))
cat(sprintf("cor(courtind, lgdp) = %.3f | R2 of courtind on lgdp = %.3f\n", cor(d$courtind, d$lgdp),
            summary(f_aux)$r.squared))
cat(sprintf("residual range %.2f to %.2f | sd(courtind) %.2f, sd(residual) %.2f\n\n",
            min(d$res_court_gdp), max(d$res_court_gdp), sd(d$courtind), sd(d$res_court_gdp)))
cat("==== R output for the code slides ====\n\n")
cat("> summary(lm(execorr100 ~ courtind, data = d))\n"); print(summary(f_simple))
cat("> fit <- lm(courtind ~ lgdp, data = d)\n> summary(fit)\n"); print(summary(f_aux))
cat("> summary(lm(execorr100 ~ courtind + lgdp, data = d))\n"); print(summary(f_multi))
cat("> summary(lm(execorr100 ~ res_court_gdp, data = d))\n"); print(summary(f_resid))
sink()

col_aux <- unname(pal["blue"])
minus <- function(v) sub("^-", "−", trimws(format(v)))
lab_crt <- "High court independence (0–4)"
lab_cor <- "Executive corruption (0–100)"
lab_gdp <- expression(log[10] * " GDP per capita")
lab_res <- "Court independence net of GDP (residual)"
r_res <- range(d$res_court_gdp)
x_res <- c(-2.45, 2.05); x_crt <- c(-0.15, 4.35)   # equal widths (4.5 points)
x_gdp <- c(2.95, 5.2)
y_cor <- c(-6, 100); y_crt <- c(-0.15, 4.1)
stopifnot(abs(diff(x_crt) - diff(x_res)) < 1e-9, r_res[1] > x_res[1], r_res[2] < x_res[2])

frame <- function(xlim, ylim, xlab, ylab, xat, yat) {
  par(mgp = c(1.9, 0.45, 0), tcl = -0.25)
  plot(NA, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = xlab, ylab = ylab)
  axis(1, at = xat, labels = minus(xat), cex.axis = 0.9)
  axis(2, at = yat, labels = minus(yat), cex.axis = 0.9)
  box(bty = "l")
}
dots <- function(x, y) points(x, y, pch = 16, cex = 0.75, col = alpha_col(col_grey, 0.75))
seg_line <- function(fit, x, col, lwd = 2, lty = 1) {
  r <- range(x)
  segments(r[1], coef(fit)[1] + coef(fit)[2] * r[1], r[2], coef(fit)[1] + coef(fit)[2] * r[2],
           col = col, lwd = lwd, lty = lty)
}
W <- 6; H <- 2.8; MAR <- c(3.1, 3.4, 0.5, 0.6); PS <- 11
at_crt <- 0:4; at_cor <- seq(0, 100, 20); at_gdp <- seq(3, 5, 0.5)

fig_pdf("fwl_corr_court", width = W, height = H, pointsize = PS, mar = MAR, {
  frame(x_crt, y_cor, lab_crt, lab_cor, at_crt, at_cor)
  dots(d$courtind, d$execorr100)
  seg_line(f_simple, d$courtind, col_dark)
})

aux <- function(resid) {
  frame(x_gdp, y_crt, lab_gdp, "Court independence", at_gdp, at_crt)
  if (resid) segments(d$lgdp, fitted(f_aux), d$lgdp, d$courtind,
                      col = alpha_col(col_aux, 0.55), lwd = 0.8)
  dots(d$lgdp, d$courtind)
  seg_line(f_aux, d$lgdp, col_aux)
}
fig_pdf("fwl_court_gdp",       width = W, height = H, pointsize = PS, mar = MAR, aux(FALSE))
fig_pdf("fwl_court_gdp_resid", width = W, height = H, pointsize = PS, mar = MAR, aux(TRUE))

fig_pdf("fwl_corr_gdp", width = W, height = H, pointsize = PS, mar = MAR, {
  frame(x_gdp, y_cor, lab_gdp, lab_cor, at_gdp, at_cor)
  dots(d$lgdp, d$execorr100)
  seg_line(f_cgdp, d$lgdp, col_aux)
})

fig_pdf("fwl_corr_court_partial", width = W, height = H, pointsize = PS, mar = MAR, {
  frame(x_crt, y_cor, lab_crt, lab_cor, at_crt, at_cor)
  dots(d$courtind, d$execorr100)
  seg_line(f_simple, d$courtind, col_dark, lty = 2)
  # the fitted plane at the mean of log GDP: slope -13.95, through the means
  r <- range(d$courtind); g_bar <- mean(d$lgdp); bm <- coef(f_multi)
  segments(r[1], bm[1] + bm[3] * g_bar + bm[2] * r[1], r[2], bm[1] + bm[3] * g_bar + bm[2] * r[2],
           col = col_cardinal, lwd = 2.2)
  legend("topright", bty = "o", box.col = NA, bg = alpha_col("white", 0.85), cex = 0.9,
         lwd = c(2, 2.2), lty = c(2, 1), col = c(col_dark, col_cardinal), seg.len = 2.2,
         legend = c(paste0("Court independence alone: slope ", minus(sprintf("%.2f", coef(f_simple)[2]))),
                    paste0("Holding GDP at its mean: slope ", minus(sprintf("%.2f", bm[2])))),
         inset = c(0.012, 0))
  box(bty = "l")
})

fig_pdf("fwl_corr_resid", width = W, height = H, pointsize = PS, mar = MAR, {
  frame(x_res, y_cor, lab_res, lab_cor, seq(-2, 2, 1), at_cor)
  dots(d$res_court_gdp, d$execorr100)
  seg_line(f_resid, d$res_court_gdp, col_cardinal, lwd = 2.2)
  legend("topright", bty = "o", box.col = NA, bg = alpha_col("white", 0.85), cex = 0.9,
         lwd = 2.2, col = col_cardinal, seg.len = 2.2,
         legend = paste0("Slope ", minus(sprintf("%.2f", coef(f_resid)[2]))), inset = c(0.012, 0))
  box(bty = "l")
})
cat("fig_partialling_out: done; numbers in", out, "\n")
