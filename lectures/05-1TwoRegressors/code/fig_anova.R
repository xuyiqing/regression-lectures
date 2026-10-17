# fig_anova.R --- Lecture 5-1, "Analysis of Variance": the three sums of squares
# for the running example, executive corruption ~ high court independence + democracy.
#
# Data: code/data/countries_xsec.csv, 2024, N = 168 (see fig_parallel_lines.R).
# Same regression as the "Adjusted R^2" output slide; that slide's R output is
# printed into code/anova_output.txt by this script.
#
# Figures (one per overlay step):
#   anova_sst   deviations of y from its mean
#   anova_sse   deviations of the fitted values from the mean
#   anova_ssr   residuals
# Each panel prints its sum, so SST = SSE + SSR can be checked by eye.
#
# Run from the deck folder (v2/):  Rscript code/fig_anova.R

source("../course_figures.R")

d <- read.csv("code/data/countries_xsec.csv")
d <- d[complete.cases(d[, c("execorr100", "courtind", "democracy", "gdppc")]), ]
stopifnot(nrow(d) == 168)
fit <- lm(execorr100 ~ courtind + democracy, data = d)
b <- coef(fit)
stopifnot(abs(b - c(78.3236, -10.6301, -21.5101)) < 5e-5)

y <- d$execorr100; x <- d$courtind; yhat <- fitted(fit); ybar <- mean(y)
sst <- sum((y - ybar)^2); sse <- sum((yhat - ybar)^2); ssr <- sum((y - yhat)^2)
stopifnot(abs(sst - (sse + ssr)) < 1e-6)

out <- "code/anova_output.txt"
sink(out)
cat("Lecture 5-1 v2: ANOVA and adjusted R^2. V-Dem v16 + WDI, 2024, N =", nrow(d), "\n")
cat(sprintf("SST = %.1f, SSE = %.1f, SSR = %.1f, R2 = %.4f\n\n", sst, sse, ssr, sse / sst))
cat("R output for the 'Adjusted R^2' slide:\n\n")
cat("> summary(lm(execorr100 ~ courtind + democracy, data = d))\n")
print(summary(fit))
sink()

col_dem <- unname(pal["blue"])
col_aut <- col_cardinal
minus <- function(v) sub("^-", "−", trimws(format(v)))

panel <- function(which) {
  par(mgp = c(1.6, 0.35, 0), tcl = -0.2)
  plot(NA, xlim = c(-0.15, 4.3), ylim = c(-6, 100), xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "High court independence (0–4)", ylab = "Executive corruption (0–100)")
  axis(1, at = 0:4, cex.axis = 0.85)
  axis(2, at = seq(0, 100, 20), cex.axis = 0.85)
  box(bty = "l")
  from <- switch(which, sst = ybar, sse = ybar, ssr = yhat)
  to   <- switch(which, sst = y,    sse = yhat, ssr = y)
  segments(x, from, x, to, col = alpha_col(col_dark, 0.45), lwd = 0.7)
  abline(h = ybar, lty = 2, col = col_dark, lwd = 1)
  text(par("usr")[2] - 0.02, ybar, expression(bar(y)), adj = c(1, -0.35), cex = 0.95)
  for (g in 0:1) {
    r <- range(x[d$democracy == g])
    segments(r[1], b[1] + g * b[3] + b[2] * r[1], r[2], b[1] + g * b[3] + b[2] * r[2],
             col = if (g == 1) col_dem else col_aut, lwd = 1.5)
  }
  points(x, y, pch = 16, cex = 0.55, col = alpha_col(col_grey, 0.8))
  if (which != "sst") points(x, yhat, pch = 16, cex = 0.35,
                             col = ifelse(d$democracy == 1, col_dem, col_aut))
  val <- switch(which, sst = sst, sse = sse, ssr = ssr)
  u <- par("usr")
  text(u[1], u[4], sprintf("%s = %s", toupper(which), format(round(val), big.mark = ",")),
       adj = c(0, -0.5), xpd = NA, cex = 0.95, font = 2)
  text(u[2], u[4], "fit: court + democracy", adj = c(1, -0.7), xpd = NA,
       cex = 0.75, col = col_grey)
}

w <- 7 / 2.54; h <- 5.4 / 2.54
for (k in c("sst", "sse", "ssr")) {
  fig_pdf(paste0("anova_", k), width = w, height = h, pointsize = 9,
          mar = c(2.7, 2.9, 1.3, 0.4), panel(k))
}
cat("fig_anova: done; numbers in", out, "\n")
