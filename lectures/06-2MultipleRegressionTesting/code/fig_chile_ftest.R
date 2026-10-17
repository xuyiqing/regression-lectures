# fig_chile_ftest.R --- the 1988 Chile plebiscite example.
# Lecture 6.2: reproduces every regression output printed on the Chile slides
# ("Hypothesis Testing in R", "Confidence Intervals in R", "Unrestricted Model
# (UR)", "Restricted Model (R)", "F Test in R", "Omnibus Test in R") and draws
#   figs/f_null_chile.pdf   slide "Null Distribution, Critical Value, and Test Statistic"
#
# Data: code/data/Chile.dta, the FLACSO national survey of April-May 1988
# (Fox, carData::Chile), with vote1 = 1 if the respondent intends to vote Yes
# (for Pinochet), fem = 1 if female; 1,703 respondents who stated a Yes or No
# vote intention and have no missing covariates.
# NOTE: the file's educ is as.numeric(education) in carData's level order
# P, PS, S (1 = primary, 2 = post-secondary, 3 = secondary), which is not
# schooling order. The script recodes it right after reading: 1 = primary,
# 2 = secondary, 3 = post-secondary (follow-up of 2026-09-17; see CHANGES.md).
#
# Run from the deck folder:  Rscript code/fig_chile_ftest.R

source("../course_figures.R")
library(foreign)

Chile <- read.dta("code/data/Chile.dta")
# The file's educ follows the alphabetical levels (P, PS, S); recode it in schooling order.
Chile$educ <- match(Chile$education, c("P", "S", "PS"))   # 1 = primary, 2 = secondary, 3 = post-secondary
stopifnot(!anyNA(Chile$educ))
d <- Chile

# ---- individual coefficients -------------------------------------------------
fit <- lm(vote1 ~ fem + educ + age, data = d)
print(summary(fit))
print(confint(fit))
V <- vcov(fit)
print(V)
print(sqrt(diag(V)))

# ---- joint test of the three gender terms ------------------------------------
fit.UR <- lm(vote1 ~ fem + educ + age + fem:age + fem:educ, data = Chile)
print(summary(fit.UR))
fit.R <- lm(vote1 ~ educ + age, data = Chile)
print(summary(fit.R))

SSR.UR  <- sum(resid(fit.UR)^2)
SSR.R   <- sum(resid(fit.R)^2)
DFdenom <- df.residual(fit.UR)
DFnum   <- 3
F  <- ((SSR.R - SSR.UR) / DFnum) / (SSR.UR / DFdenom)
cv <- qf(0.99, DFnum, DFdenom)
cat("SSR.UR =", round(SSR.UR, 1), " SSR.R =", round(SSR.R, 1),
    " DFdenom =", DFdenom, " n =", nobs(fit.UR), "\n")
print(F)
print(cv)
cat("p-value =", format(pf(F, DFnum, DFdenom, lower.tail = FALSE), digits = 3), "\n")
print(anova(fit.R, fit.UR))              # the same F test in one call

# ---- figure: null distribution, 1% critical value, observed statistic --------
fig_pdf("f_null_chile", width = 5.6, height = 2.4,
        mar = c(3.1, 3.6, 1.6, 0.8), {
  xs <- seq(0, 15, length.out = 1501)
  ys <- df(xs, DFnum, DFdenom)
  plot(NA, xlim = c(0, 15), ylim = c(0, 0.8), xaxs = "i", yaxs = "i",
       xlab = expression("Value of the " * italic(F) * " statistic"),
       ylab = "Density")
  title(main = bquote("Null distribution " * italic(F)[list(.(DFnum), .(DFdenom))]),
        font.main = 1, cex.main = 0.95)
  # rejection region at the 1% level, marked along the axis
  rect(cv, 0, 15, 0.035, col = alpha_col(col_cardinal, 0.25), border = NA)
  text(mean(c(cv, F)), 0.035, "rejection region (1% level)", pos = 3,
       cex = 0.8, col = col_cardinal)
  lines(xs, ys, lwd = 2, col = col_dark)
  segments(cv, 0, cv, 0.62, lty = 2, lwd = 1.2, col = col_grey)
  text(cv, 0.62, sprintf("critical value = %.2f", cv), pos = 4, cex = 0.85)
  segments(F, 0, F, 0.62, lwd = 2, col = col_cardinal)
  text(F, 0.62, bquote("observed " * italic(F) == .(sprintf("%.2f", F))),
       pos = 2, cex = 0.85, col = col_cardinal)
})
