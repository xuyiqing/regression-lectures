# fig_gerber.R --- lecture 6-3, "Example: Social Pressure Experiment".
#
# Draws no figure: the slide before the regressions shows the study's
# "Neighbors" mailer (figs/gerber1.pdf, kept from 2010). This script writes
# code/fig_gerber_output.txt, the R output and the numbers quoted on the two
# regression slides:
#   - OLS of turnout on the four treatment indicators, homoskedastic SEs
#   - the same with SEs clustered by household, via sandwich::vcovCL (this
#     replaces the hand-written vcovCluster() the slides used before; the two
#     give identical matrices: same CR1 small-sample factor
#     M/(M-1) * (N-1)/(N-K))
#   - the within-household error correlation rho, estimated with the
#     variance-components method of the deck's (commented-out) slide
#     "Estimating the Variance Components", in this subset and in the full
#     sample of the experiment
#   - the ratio of clustered to homoskedastic SEs
#
# Data (both copied from ../02-4MoreInference/code/data/):
#   code/data/gerber_reduced.dta  the random subset of the Gerber, Green and
#     Larimer (2008) Michigan experiment used on these slides since 2011
#     (14,392 voters in 7,599 households)
#   code/data/gerber.dta  the full experiment (344,084 voters), used only for
#     the full-sample rho quoted on the slide
#
# lmtest (coeftest) is used because the slides show coeftest() output; it is
# installed as a dependency of AER.
#
# Run from the deck folder:  Rscript code/fig_gerber.R

source("../course_figures.R")
suppressPackageStartupMessages({
  library(foreign)
  library(sandwich)
  library(lmtest)
})

dta <- read.dta("code/data/gerber_reduced.dta")
dta$voted <- as.numeric(dta$voted == "Yes")
dta$hh_id <- factor(dta$hh_id)   # CONTRACT 5: household ID as a factor
stopifnot(nrow(dta) == 14392)

mod1 <- lm(voted~Tr_Hawthorne+Tr_CivicDuty+Tr_Neighbors+Tr_Self,data=dta)
ct_homo <- coeftest(mod1)
ct_cl   <- coeftest(mod1, vcov = vcovCL(mod1, cluster = ~ hh_id))

# within-household correlation of the errors (variance components)
rho_vc <- function(e, cluster, k) {
  N <- length(e)
  M <- length(unique(cluster))
  s2       <- sum(e^2) / (N - k - 1)              # sigma^2
  e_tilde  <- e - ave(e, cluster)                 # residuals minus household mean
  s2_tilde <- sum(e_tilde^2) / (N - M - k - 1)    # (1 - rho) sigma^2
  c(rho = (s2 - s2_tilde) / s2, N = N, M = M)
}
rv <- rho_vc(resid(mod1), dta$hh_id, k = 4)
rho <- rv[["rho"]]
N <- rv[["N"]]
M <- rv[["M"]]

# the same in the full sample (same model: turnout on the four treatments)
full <- read.dta("code/data/gerber.dta")
full$voted <- as.numeric(full$voted == "Yes")
full$hh_id <- factor(full$hh_id)
tr <- trimws(as.character(full$treatment))
stopifnot(setequal(unique(tr), c("Control", "Hawthorne", "Civic Duty", "Neighbors", "Self")))
mod_full <- lm(full$voted ~ (tr == "Hawthorne") + (tr == "Civic Duty") +
                 (tr == "Neighbors") + (tr == "Self"))
rv_full <- rho_vc(resid(mod_full), full$hh_id, k = 4)

ratio <- ct_cl[, 2] / ct_homo[, 2]

out <- "code/fig_gerber_output.txt"
sink(out)
cat("> mod1 <- lm(voted~Tr_Hawthorne+Tr_CivicDuty+Tr_Neighbors+Tr_Self,data=dta)\n")
cat("> coeftest(mod1) # homoskedastic\n")
print(ct_homo)
cat("> coeftest(mod1, vcov = vcovCL(mod1, cluster = ~ hh_id)) # clustered\n")
print(ct_cl)
cat(sprintf("\nN = %d voters, M = %d households\n", N, M))
cat(sprintf("within-household error correlation rho = %.4f  (slide: %.2f)\n", rho, rho))
cat(sprintf("full sample: N = %s voters, M = %s households, rho = %.4f  (slide: %.2f)\n",
            format(rv_full[["N"]], big.mark = ","), format(rv_full[["M"]], big.mark = ","),
            rv_full[["rho"]], rv_full[["rho"]]))
cat("clustered / homoskedastic SE:\n")
print(round(ratio, 3))
cat(sprintf("clustered SEs are %.0f%% to %.0f%% larger\n",
            100 * (min(ratio) - 1), 100 * (max(ratio) - 1)))
sink()
cat("wrote", out, "\n")
