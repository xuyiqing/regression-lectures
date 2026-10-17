# worksheet_q1c.R --- Worksheet 5-3, Q1(c): the test of E[Y|Wales] = E[Y|Scotland] in the regions
# model with London as the baseline (slide 12). Not on a slide; the worksheet solution uses it as an
# optional numerical extension. Same data and sample as fig_brexit.R (380 counting areas of Great
# Britain; NI and Gibraltar left out).
#
# Run from the deck folder:  Rscript code/worksheet_q1c.R   (writes code/worksheet_q1c_output.txt)

options(width = 80, digits = 7)
out <- "code/worksheet_q1c_output.txt"
sink(out)
on.exit(sink(), add = TRUE)

b <- read.csv("code/data/brexit_la.csv")
b <- b[b$nation %in% c("England", "Wales", "Scotland"), ]
stopifnot(nrow(b) == 380)
D <- data.frame(Leave = b$pct_leave, Region = relevel(factor(b$region), ref = "London"))
fit <- lm(Leave ~ Region, data = D)
cat("Worksheet 5-3 Q1(c): H0 E[Y|Wales] = E[Y|Scotland], i.e. beta_Wales - beta_Scot = 0\n")
cat("Model: lm(Leave ~ Region), London baseline, n =", nobs(fit), "\n\n")

b.w <- coef(fit)["RegionWales"]; b.s <- coef(fit)["RegionScotland"]
V <- vcov(fit)
v.w <- V["RegionWales", "RegionWales"]; v.s <- V["RegionScotland", "RegionScotland"]
c.ws <- V["RegionWales", "RegionScotland"]
est <- b.w - b.s
se <- sqrt(v.w + v.s - 2 * c.ws)
se.nocov <- sqrt(v.w + v.s)
tstat <- est / se
df <- fit$df.residual
cat(sprintf("beta_Wales %.4f (SE %.4f); beta_Scot %.4f (SE %.4f)\n", b.w, sqrt(v.w), b.s, sqrt(v.s)))
cat(sprintf("Var(b_Wales) %.4f; Var(b_Scot) %.4f; Cov(b_Wales, b_Scot) %.4f\n", v.w, v.s, c.ws))
cat(sprintf("sigma-hat^2 / n_London = %.4f (equals the covariance: both slopes subtract the London mean)\n",
            summary(fit)$sigma^2 / sum(D$Region == "London")))
cat(sprintf("Estimate beta_Wales - beta_Scot = %.4f\n", est))
cat(sprintf("SE %.4f (without the covariance term %.4f)\n", se, se.nocov))
cat(sprintf("t %.3f on %d df; two-sided p %.3g; t_{%d} 5%% critical value %.3f\n",
            tstat, df, 2 * pt(-abs(tstat), df), df, qt(0.975, df)))
cat(sprintf("Check: group means Wales %.4f, Scotland %.4f, difference %.4f\n",
            mean(D$Leave[D$Region == "Wales"]), mean(D$Leave[D$Region == "Scotland"]),
            mean(D$Leave[D$Region == "Wales"]) - mean(D$Leave[D$Region == "Scotland"])))
cat(sprintf("Check: sigma-hat * sqrt(1/n_W + 1/n_S) = %.4f\n",
            summary(fit)$sigma * sqrt(1 / sum(D$Region == "Wales") + 1 / sum(D$Region == "Scotland"))))
library(car)
cat("\n> car::linearHypothesis(fit, \"RegionWales = RegionScotland\")\n")
print(linearHypothesis(fit, "RegionWales = RegionScotland"))
