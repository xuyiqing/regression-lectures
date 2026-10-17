# fig_multicollinearity_vdem.R --- Lecture 5-2 v2, multicollinearity on real data.
#
# Executive corruption (execorr100 = 100 x v2x_execorr) on two democracy indices that V-Dem builds one from the other: the electoral
# democracy index (polyarchy) and the liberal democracy index (libdem), which "also takes
# the level of electoral democracy into account" (V-Dem v16 codebook, v2x_libdem).
#
# Data: code/data/countries_xsec.csv (copy of lectures/_data/countries/; V-Dem v16 + WDI,
# 2024; built in lectures/_data/countries/). Sample: complete on execorr100, polyarchy, libdem
# (N = 179; court independence and GDP are not used here, so no country is dropped for them).
#
# Figure: figs/polyarchy_libdem.pdf (scatter of the two indices, shares a slide with a table).
# Numbers on the slide: code/multicollinearity_vdem_output.txt
# Run from the deck folder (v2/):  Rscript code/fig_multicollinearity_vdem.R

source("../course_figures.R")

d <- read.csv("code/data/countries_xsec.csv")
M <- d[complete.cases(d[, c("execorr100", "polyarchy", "libdem")]), ]
stopifnot(nrow(M) == 179)

r   <- cor(M$polyarchy, M$libdem)
aux <- lm(polyarchy ~ libdem, data = M)
R2j <- summary(aux)$r.squared
vif <- 1 / (1 - R2j)
k1 <- lm(execorr100 ~ polyarchy, data = M)
k2 <- lm(execorr100 ~ libdem, data = M)
k3 <- lm(execorr100 ~ polyarchy + libdem, data = M)
# same numbers as lectures/_data/countries/gates_court_output.txt (5-2 multicollinearity)
stopifnot(abs(coef(k1)[2] - (-82.596)) < 0.001, abs(coef(k3)[2] - 130.543) < 0.001, abs(coef(k3)[3] - (-216.558)) < 0.001)

se <- function(m, v) coef(summary(m))[v, "Std. Error"]
tv <- function(m, v) coef(summary(m))[v, "t value"]

out <- "code/multicollinearity_vdem_output.txt"
sink(out)
cat("fig_multicollinearity_vdem.R --- Lecture 5-2 v2, multicollinearity on real data\n")
cat("Data: V-Dem v16, 2024, N =", nrow(M), "\n\n")
cat(sprintf("r(polyarchy, libdem) = %.4f ; R_j^2 = %.4f ; VIF = 1/(1 - R_j^2) = %.2f ; sqrt(VIF) = %.2f\n\n",
            r, R2j, vif, sqrt(vif)))
for (m in list(k1, k2, k3)) {
  cat("--", deparse(formula(m)), "\n"); print(round(coef(summary(m)), 4))
  cat(sprintf("R2 = %.4f   resid SE = %.3f\n\n", summary(m)$r.squared, summary(m)$sigma))
}
cat(sprintf("SE inflation, together / alone: polyarchy %.2f / %.2f = x%.2f ; libdem %.2f / %.2f = x%.2f\n",
            se(k3, "polyarchy"), se(k1, "polyarchy"), se(k3, "polyarchy") / se(k1, "polyarchy"),
            se(k3, "libdem"), se(k2, "libdem"), se(k3, "libdem") / se(k2, "libdem")))
cat(sprintf("t values together: polyarchy %.2f, libdem %.2f (both still |t| > 2)\n",
            tv(k3, "polyarchy"), tv(k3, "libdem")))
cat("\nAs typed on the slide (coef, SE in parentheses):\n")
cat(sprintf("  alone:    polyarchy %.1f (%.1f) | libdem %.1f (%.1f)\n",
            coef(k1)[2], se(k1, "polyarchy"), coef(k2)[2], se(k2, "libdem")))
cat(sprintf("  together: polyarchy %+.1f (%.1f) | libdem %.1f (%.1f)\n",
            coef(k3)[2], se(k3, "polyarchy"), coef(k3)[3], se(k3, "libdem")))
cat(sprintf("  R2: %.2f | %.2f | %.2f\n", summary(k1)$r.squared, summary(k2)$r.squared, summary(k3)$r.squared))
cat(sprintf("  range: polyarchy %.3f-%.3f ; libdem %.3f-%.3f\n", min(M$polyarchy), max(M$polyarchy),
            min(M$libdem), max(M$libdem)))
sink()
cat(readLines(out), sep = "\n")

fig_pdf("polyarchy_libdem", width = 3.0, height = 2.9, mar = c(3.2, 3.4, 0.6, 0.6), {
  plot(M$polyarchy, M$libdem, pch = 19, cex = 0.7, col = alpha_col(col_cardinal, 0.5),
       xlim = c(0, 1), ylim = c(0, 1), xlab = "Electoral democracy (polyarchy)",
       ylab = "Liberal democracy (libdem)")
  text(0.02, 0.95, sprintf("r = %.2f\nVIF = %.1f", r, vif), adj = c(0, 1), col = col_dark)
})
