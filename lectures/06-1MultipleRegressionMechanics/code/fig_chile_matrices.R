# fig_chile_matrices.R --- Lecture 6.1, R output for two slides (no figure).
#
# Feeds two slides, whose Verbatim blocks are typed from the printed output
# (saved to code/fig_chile_matrices.txt):
#   "Explanatory Data Matrix"                    dim(X) and head(X, 8), where
#                                                X = model.matrix(fit)
#   "Variance-covariance matrix of beta_hat"     summary(fit) coefficients,
#   (the R-code slide)                           vcov(fit), sqrt(diag(vcov(fit)))
#
# Data. National survey by FLACSO, April-May 1988, before the Chilean
# plebiscite on Pinochet; the same file and the same regression open
# Lecture 6.2 (copied from 06-2MultipleRegressionTesting/code/data/Chile.dta).
# The file keeps only respondents who said they would vote Yes or No
# (n = 1703).
#   vote1  1 = intends to vote Yes (for Pinochet), 0 = intends to vote No
#   fem    1 = female
#   educ   schooling level, recoded below from the factor `education`:
#          1 = primary (P), 2 = secondary (S), 3 = post-secondary (PS).
#          (The file's own educ is the alphabetical factor code, 1 = P,
#          2 = PS, 3 = S, which is not in order of schooling.)
#   age    years
#
# No randomness: the output is deterministic.
#
# Run from the deck folder:  Rscript code/fig_chile_matrices.R

source("../course_figures.R")   # shared style; loaded for convention only
library(foreign)

d   <- read.dta("code/data/Chile.dta")
# The file's educ follows the alphabetical levels (P, PS, S); recode it in schooling order.
d$educ <- match(d$education, c("P", "S", "PS"))   # 1 = primary, 2 = secondary, 3 = post-secondary
stopifnot(!anyNA(d$educ))
fit <- lm(vote1 ~ fem + educ + age, data = d)
X   <- model.matrix(fit)
V   <- vcov(fit)

# The prompts are printed so the text file reads like the console session
# shown on the slides.
out <- capture.output({
  cat("> fit <- lm(vote1 ~ fem + educ + age, data = d)\n")
  cat("> X <- model.matrix(fit)\n")
  cat("> dim(X)\n");        print(dim(X))
  cat("> head(X, 8)\n");    print(head(X, 8))
  cat("\n> summary(fit)\n"); print(summary(fit))
  cat("> V <- vcov(fit)\n> V\n"); print(V)
  cat("\n> sqrt(diag(V))\n"); print(sqrt(diag(V)))
})
writeLines(out, "code/fig_chile_matrices.txt")

# Checks that tie the slide numbers to the formulas on the slides.
stopifnot(nrow(X) == 1703, ncol(X) == 4, all(X[, 1] == 1))
sig2 <- sum(resid(fit)^2) / (nrow(X) - ncol(X))          # SSR / (n - (k + 1))
stopifnot(isTRUE(all.equal(V, sig2 * solve(crossprod(X)),  # sigma^2 (X'X)^{-1}
                           check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(unname(sqrt(diag(V))),
                           unname(coef(summary(fit))[, "Std. Error"]))))

cat(out, sep = "\n")
