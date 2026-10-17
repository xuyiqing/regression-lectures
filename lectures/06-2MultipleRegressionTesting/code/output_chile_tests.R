# output_chile_tests.R --- the new Chile examples in Lecture 6-2 (no figure).
# Rebuild of 2026-10-11 (replaces output_gdp_region.R, the Persson-Tabellini
# GDP-on-regions example). Reproduces every number printed on these slides and
# writes them to code/chile_tests_output.txt:
#   - "Example: Chilean Referendum on Pinochet": the LPM note (range of fitted
#     values; robust vs conventional standard errors)
#   - "Testing Hypothesis About a Linear Combination of beta_j" (two frames),
#     "Example: Is the 1-2-3 Coding of Education OK?" (two frames):
#     vote1 ~ fem + age + S + PS and the test of beta_PS = 2 beta_S
#   - "Testing Equality of Coefficients in R" (two frames): beta_S = beta_PS
#   - "When t and F Disagree: Age and Age Squared": vote1 ~ fem + educ + age + I(age^2)
#   - "Testing General Linear Hypotheses in R": linearHypothesis() for beta_PS = 2 beta_S
#
# Data: code/data/Chile.dta (carData::Chile, FLACSO survey, April-May 1988),
# 1,703 respondents with a Yes/No vote intention and no missing covariates.
# The file's educ is alphabetical (1 = P, 2 = PS, 3 = S); recode it in schooling
# order first (CONTRACT section 5): educ <- match(education, c("P", "S", "PS")).
#
# Run from the v2 deck folder:  Rscript code/output_chile_tests.R

library(foreign)
library(car)      # linearHypothesis(), hccm()

sink("code/chile_tests_output.txt", split = TRUE)
cat("output_chile_tests.R run", format(Sys.time(), "%Y-%m-%d"),
    "| R", as.character(getRversion()), "| car", as.character(packageVersion("car")), "\n\n")

d <- read.dta("code/data/Chile.dta")
d$educ <- match(d$education, c("P", "S", "PS"))   # 1 = primary, 2 = secondary, 3 = post-secondary
stopifnot(!anyNA(d$educ), nrow(d) == 1703)
d$S  <- as.numeric(d$education == "S")    # secondary
d$PS <- as.numeric(d$education == "PS")   # post-secondary; baseline = primary
print(table(education = d$education))

# ---- 1. LPM note on the first Chile slide ------------------------------------
fit <- lm(vote1 ~ fem + educ + age, data = d)
cat("\n== LPM check: vote1 ~ fem + educ + age ==\n")
cat("range of fitted values:", sprintf("%.3f", range(fitted(fit))), "\n")
se_conv <- sqrt(diag(vcov(fit)))
se_hc1  <- sqrt(diag(hccm(fit, type = "hc1")))
se_hc3  <- sqrt(diag(hccm(fit, type = "hc3")))
print(round(cbind(conventional = se_conv, HC1 = se_hc1, HC3 = se_hc3,
                  ratio_HC1 = se_hc1 / se_conv, ratio_HC3 = se_hc3 / se_conv), 5))
cat("largest robust/conventional ratio:",
    sprintf("%.4f", max(se_hc1 / se_conv, se_hc3 / se_conv)), "\n")

# ---- 2. education dummies; H0: beta_PS = 2 beta_S ----------------------------
cat("\n== Education dummies: vote1 ~ fem + age + S + PS ==\n")
fit.dum <- lm(vote1 ~ fem + age + S + PS, data = d)
print(summary(fit.dum))
V <- vcov(fit.dum)
print(V)
print(V[c("S", "PS"), c("S", "PS")])

b <- coef(fit.dum)
est  <- b["PS"] - 2 * b["S"]
se   <- sqrt(V["PS", "PS"] + 4 * V["S", "S"] - 4 * V["S", "PS"])
se_nocov <- sqrt(V["PS", "PS"] + 4 * V["S", "S"])          # wrong: drops the covariance term
tstat <- est / se
p_t   <- 2 * pt(abs(tstat), df.residual(fit.dum), lower.tail = FALSE)
cat("\n-- as printed on the slide --\n")
print(se)
tstat_slide <- (coef(fit.dum)["PS"] - 2 * coef(fit.dum)["S"]) / se
print(tstat_slide)
print(sqrt(V["PS", "PS"] + 4 * V["S", "S"]))
cat("\nbeta_PS - 2 beta_S =", sprintf("%.4f", est), "\n")
cat("V[PS,PS] =", sprintf("%.6f", V["PS", "PS"]), " V[S,S] =", sprintf("%.6f", V["S", "S"]),
    " Cov[S,PS] =", sprintf("%.6f", V["S", "PS"]), "\n")
cat("SE (with covariance)    =", sprintf("%.4f", se), "\n")
cat("SE (covariance dropped) =", sprintf("%.4f", se_nocov), "\n")
cat("t =", sprintf("%.3f", tstat), "  t^2 =", sprintf("%.4f", tstat^2),
    "  p =", sprintf("%.4f", p_t), "\n")
cat("cor(b_S, b_PS) =", sprintf("%.3f", cov2cor(V)["S", "PS"]), "\n")

# The restricted model under H0 is the deck's first model: S + 2 PS = educ - 1.
stopifnot(all(d$S + 2 * d$PS == d$educ - 1))
cat("\n-- restricted (1-2-3 coding) vs unrestricted (dummies), by anova() --\n")
print(anova(fit, fit.dum))

# Same test as R beta = r with car::linearHypothesis (slide "Testing General Linear Hypotheses in R")
cat("\n-- linearHypothesis(fit.dum, R, r) --\n")
R <- matrix(c(0, 0, 0, -2, 1), nrow = 1)    # coefficients: (Intercept), fem, age, S, PS
r <- 0
print(linearHypothesis(fit.dum, R, r))
dev <- R %*% b - r
W <- drop(t(dev) %*% solve(R %*% V %*% t(R)) %*% dev)
cat("Wald W by hand =", sprintf("%.4f", W), " (q = 1, so W = F = t^2)\n")

# ---- 3. plain equality: H0: beta_S = beta_PS ---------------------------------
cat("\n== Equality: H0: beta_S = beta_PS ==\n")
d$Xstar <- d$S + d$PS                         # = 1 if secondary or post-secondary
fit.R2  <- lm(vote1 ~ fem + age + Xstar, data = d)
print(summary(fit.R2))
SSR.UR2 <- sum(resid(fit.dum)^2)
SSR.R2  <- sum(resid(fit.R2)^2)
DFdenom <- df.residual(fit.dum)
F2 <- ((SSR.R2 - SSR.UR2) / 1) / (SSR.UR2 / DFdenom)
cat("SSR.UR2 =", sprintf("%.2f", SSR.UR2), " SSR.R2 =", sprintf("%.2f", SSR.R2),
    " DFdenom =", DFdenom, "\n")
print(F2)
print(pf(F2, 1, DFdenom, lower.tail = FALSE))
t2 <- (b["PS"] - b["S"]) / sqrt(V["PS", "PS"] + V["S", "S"] - 2 * V["S", "PS"])
cat("t for beta_PS - beta_S =", sprintf("%.3f", t2), "  t^2 =", sprintf("%.4f", t2^2), "\n")
print(linearHypothesis(fit.dum, "S = PS"))

# ---- 4. t and F disagree: age and age^2 -------------------------------------
cat("\n== t vs F: vote1 ~ fem + educ + age + I(age^2) ==\n")
fit.sq <- lm(vote1 ~ fem + educ + age + I(age^2), data = d)
print(summary(fit.sq))
fit.noage <- lm(vote1 ~ fem + educ, data = d)
print(anova(fit.noage, fit.sq))
cat("cor(age, age^2) =", sprintf("%.3f", cor(d$age, d$age^2)), "\n")
cat("cor(b_age, b_age2) =", sprintf("%.3f", cov2cor(vcov(fit.sq))["age", "I(age^2)"]), "\n")
cat("age range:", range(d$age), "\n")
# individual 95% CIs both cover 0; the joint test rejects
print(confint(fit.sq)[c("age", "I(age^2)"), ])

sink()
