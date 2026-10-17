# residual_se.R --- Lecture 5-2 v2, frame "Example: Executive Corruption on Court Independence and Democracy".
#
# The Lecture 5-1 running example, model (2): executive corruption on high court
# independence and a democracy dummy. Shows the R output and the residual standard error
# computed by hand, sqrt(SSR / (n - 3)).
#
# Data: code/data/countries_xsec.csv (copy of lectures/_data/countries/; V-Dem v16 + WDI,
# 2024). Sample: the 5-1 sample, complete on execorr100, courtind, democracy and log_gdppc (N = 168).
#   execorr100 100 x V-Dem v2x_execorr, executive corruption (0 = clean, 100 = most corrupt)
#   courtind   V-Dem v2juhcind_osp, high court independence (0 = never independent, 4 = always)
#   democracy  1 if V-Dem polyarchy > 0.5
# Output (verbatim R console, as typed on the slide): code/residual_se_output.txt
# Run from the deck folder (v2/):  Rscript code/residual_se.R

d <- read.csv("code/data/countries_xsec.csv")
d <- d[complete.cases(d[, c("execorr100", "courtind", "democracy", "log_gdppc")]), ]
stopifnot(nrow(d) == 168)

out <- "code/residual_se_output.txt"
sink(out)
cat("> fit <- lm(execorr100 ~ courtind + democracy, data = d)\n> summary(fit)\n")
fit <- lm(execorr100 ~ courtind + democracy, data = d)
print(summary(fit))
cat("> DF <- nrow(d) - length(coef(fit))  # = DF of the regression\n> DF\n")
DF <- nrow(d) - length(coef(fit)); print(DF)
cat("> sqrt(sum(residuals(fit)^2) / DF)  # = residual standard error\n")
print(sqrt(sum(residuals(fit)^2) / DF))
sink()
cat(readLines(out), sep = "\n")
