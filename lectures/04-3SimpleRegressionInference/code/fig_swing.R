# fig_swing.R --- a null that matters (Lecture 4-3, the two frames after the test of beta1 = 0).
#
# Regress Trump's 2024 share of the two-party vote on his 2020 share, across the 50 states, and test
# H0: beta1 = 1 ("uniform swing": every state moved by the same number of points). Added in the
# 2026-10-11 examples rebuild (PLAN.md slot "4-3, a null that matters").
#
# Data: code/data/state_pres.csv, a copy of lectures/_data/us_elections/state_pres.csv. rep2p_2020 and
# rep2p_2024 are Trump / (Trump + Biden or Harris) from MEDSL "U.S. President 1976-2024"
# (doi:10.7910/DVN/42MVDX, CC0), official state totals. Shares are in percent. DC excluded.
#
# Run from the deck folder:  Rscript code/fig_swing.R
# Writes figs/swing_scatter.pdf and code/swing_output.txt (every number the slides print).
# Gate (D1, _data/us_elections/gates_output.txt): slope 0.9558 (SE 0.0156), t for H0 slope = 1
# -2.83, p = 0.0068, R2 0.9873.

source("../course_figures.R")

raw <- read.csv("code/data/state_pres.csv", colClasses = c(state_fips = "character"))
states <- data.frame(st = raw$state_po, trump20 = 100 * raw$rep2p_2020,
                     trump24 = 100 * raw$rep2p_2024)[raw$is_dc == 0, ]
stopifnot(nrow(states) == 50, !anyNA(states))
states$swing <- states$trump24 - states$trump20

options(width = 80, digits = 7)
lm.swing <- lm(trump24 ~ trump20, data = states)
s <- summary(lm.swing)
cf <- coef(s)
b0 <- cf[1, 1]; b1 <- cf[2, 1]; se1 <- cf[2, 2]; dfr <- s$df[2]
t1 <- (b1 - 1) / se1
p1 <- 2 * pt(-abs(t1), dfr)
stopifnot(round(b1, 4) == 0.9558, round(se1, 4) == 0.0156, round(t1, 2) == -2.83,
          round(s$r.squared, 4) == 0.9873, dfr == 48)

# The same test, written as a regression of the swing on the 2020 share: slope = b1 - 1, same SE.
lm.d <- lm(swing ~ trump20, data = states)
stopifnot(abs(coef(lm.d)[2] - (b1 - 1)) < 1e-10,
          abs(coef(summary(lm.d))[2, 2] - se1) < 1e-10)

# Under H0 the best line with slope 1 is trump24 = trump20 + mean swing.
msw <- mean(states$swing)

fmt <- function(v, d) sub("^-", "−", formatC(v, format = "f", digits = d))
show <- c("UT", "WA", "OK", "FL", "NJ", "NY")   # three smallest and three largest swings

fig_pdf("swing_scatter", width = 6.4, height = 2.9, mar = c(3.2, 3.6, 1.6, 0.8), {
  par(mfrow = c(1, 2))
  # left: 2024 on 2020
  lim <- c(30, 76)
  plot(states$trump20, states$trump24, xlim = lim, ylim = lim, xaxs = "i", yaxs = "i",
       pch = 16, cex = 0.7, col = alpha_col(col_dark, 0.6),
       xlab = "Trump share 2020 (%)", ylab = "Trump share 2024 (%)")
  clip(lim[1], lim[2], lim[1], lim[2])
  abline(msw, 1, col = col_grey, lwd = 1.6, lty = 2)
  abline(lm.swing, col = col_cardinal, lwd = 2)
  do.call(clip, as.list(par("usr")))
  mtext(bquote(R^2 == .(fmt(s$r.squared, 3))), side = 3, line = 0.2, cex = 0.85)
  legend("topleft", bty = "n", cex = 0.75, lwd = c(2, 1.6), lty = c(1, 2),
         col = c(col_cardinal, col_grey),
         legend = c(sprintf("OLS: slope %s", fmt(b1, 3)), "uniform swing: slope 1"))
  # right: the swing on 2020
  ylim <- c(-0.2, 6.2)
  plot(states$trump20, states$swing, xlim = lim, ylim = ylim, xaxs = "i", yaxs = "i",
       pch = 16, cex = 0.7, col = alpha_col(col_dark, 0.6),
       xlab = "Trump share 2020 (%)", ylab = "swing, 2024 minus 2020 (points)")
  abline(h = msw, col = col_grey, lwd = 1.6, lty = 2)
  abline(lm.d, col = col_cardinal, lwd = 2)
  k <- match(show, states$st)
  text(states$trump20[k], states$swing[k], show, pos = c(3, 1, 1, 3, 3, 3), offset = 0.3,
       cex = 0.7, col = col_dark)
  mtext(sprintf("slope %s − 1 = %s", fmt(b1, 3), fmt(b1 - 1, 3)), side = 3, line = 0.2,
        cex = 0.85)
})

f4 <- function(v) sprintf("%.4f", v)
out <- c(
  "# fig_swing.R output: a null that matters (Lecture 4-3)",
  "# Y = trump24, X = trump20: Trump's % of the two-party vote (MEDSL state totals); 50 states, DC excluded",
  "",
  "> lm.swing <- lm(trump24 ~ trump20, data = states)",
  "> summary(lm.swing)",
  capture.output(s),
  "",
  "# ---- numbers on the slides",
  sprintf("n = %d; residual df = %d", nrow(states), dfr),
  sprintf("b0 = %.4f (SE %.4f); b1 = %.4f (SE %.4f); R2 = %.4f", b0, cf[1, 2], b1, se1, s$r.squared),
  sprintf("R's t value for H0: b1 = 0: %.2f", cf[2, 3]),
  sprintf("T for H0: b1 = 1: (%.4f - 1) / %.4f = %.4f; df %d; two-sided p = %.4f", b1, se1, t1, dfr, p1),
  sprintf("critical value qt(0.975, %d) = %.4f; |T| > critical value: %s", dfr, qt(0.975, dfr),
          abs(t1) > qt(0.975, dfr)),
  sprintf("95%% CI for b1: [%.4f, %.4f] (excludes 1: %s)", confint(lm.swing)[2, 1],
          confint(lm.swing)[2, 2], confint(lm.swing)[2, 2] < 1),
  sprintf("b1 - 1 = %.4f: a state 10 points more Republican in 2020 has a fitted swing %.2f points smaller",
          b1 - 1, -10 * (b1 - 1)),
  sprintf("fitted swing at a 2020 share of 35%%: %.2f; at 65%%: %.2f",
          predict(lm.d, data.frame(trump20 = 35)), predict(lm.d, data.frame(trump20 = 65))),
  sprintf("swing ~ trump20: slope %.4f (SE %.4f, t %.4f), R2 %.4f", coef(lm.d)[2],
          coef(summary(lm.d))[2, 2], coef(summary(lm.d))[2, 3], summary(lm.d)$r.squared),
  sprintf("swing (points): mean %.2f, SD %.2f, min %.2f (%s), max %.2f (%s); states with swing > 0: %d of 50",
          msw, sd(states$swing), min(states$swing), states$st[which.min(states$swing)],
          max(states$swing), states$st[which.max(states$swing)], sum(states$swing > 0)),
  sprintf("three smallest swings: %s", paste(sprintf("%s %.2f", states$st[order(states$swing)][1:3],
                                                     sort(states$swing)[1:3]), collapse = ", ")),
  sprintf("three largest swings: %s", paste(sprintf("%s %.2f", states$st[order(-states$swing)][1:3],
                                                    sort(states$swing, decreasing = TRUE)[1:3]), collapse = ", ")),
  sprintf("R2 of the uniform-swing line (trump24 = trump20 + %.2f): %.4f",
          msw, 1 - sum((states$trump24 - states$trump20 - msw)^2) /
            sum((states$trump24 - mean(states$trump24))^2))
)
writeLines(out, "code/swing_output.txt")
cat(paste(out[(length(out) - 11):length(out)], collapse = "\n"), "\n")
