# fig_brexit_cluster.R --- lecture 6-3, clustered SEs with large clusters and a
# regressor assigned at the cluster level (v2, 2026-10-11).
#
# Colantone & Stanig (2018), "Global Competition and Brexit," APSR 112(2):
# 201-218. Individual file: British Election Study wave 8 (May 6 - June 22,
# 2016), each respondent given the Chinese import shock of his or her NUTS-3
# region (Harvard Dataverse doi:10.7910/DVN/AL1A4Q, CC0). Every respondent in
# a region has the same value of the regressor, so the regressor is assigned
# by cluster, as the letter was assigned by household in the Gerber example.
#
# Model: leave (1 = intends to vote Leave) on import_shock + age + gender
# (linear probability model), without and with dummies for the 11 NUTS-1
# regions of Great Britain. SEs: classical and clustered by NUTS-3 region
# (sandwich::vcovCL, the deck's default CR1). The within-region error
# correlation rho uses the variance-components method of fig_gerber.R.
#
# Data: code/data/colantone_stanig_bes_w8.csv (copy of
# lectures/_data/brexit/colantone_stanig_bes_w8.csv). Draws no figure.
# Writes code/fig_brexit_cluster_output.txt.
#
# Run from the deck folder:  Rscript code/fig_brexit_cluster.R

suppressPackageStartupMessages({
  library(sandwich)
  library(lmtest)
})

bes <- read.csv("code/data/colantone_stanig_bes_w8.csv")
bes <- bes[!is.na(bes$leave) & !is.na(bes$import_shock) & !is.na(bes$age) & !is.na(bes$gender), ]
bes$nuts3 <- factor(bes$nuts3)
bes$nuts1 <- factor(bes$nuts1)
stopifnot(nrow(bes) == 16331, nlevels(bes$nuts3) == 167)
# the regressor is constant within each NUTS-3 region
stopifnot(all(tapply(bes$import_shock, bes$nuts3, function(x) length(unique(x))) == 1))
size <- table(bes$nuts3)

rho_vc <- function(e, cluster, k) {             # same estimator as fig_gerber.R
  N <- length(e); M <- length(unique(cluster))
  s2       <- sum(e^2) / (N - k - 1)
  e_tilde  <- e - ave(e, cluster)
  s2_tilde <- sum(e_tilde^2) / (N - M - k - 1)
  (s2 - s2_tilde) / s2
}

m1 <- lm(leave ~ import_shock + age + gender, data = bes)
m2 <- lm(leave ~ import_shock + age + gender + nuts1, data = bes)

row <- function(m, k) {
  c0 <- coeftest(m)["import_shock", ]
  c1 <- coeftest(m, vcov = vcovCL(m, cluster = ~ nuts3))["import_shock", ]
  c(b = c0[[1]], se_classical = c0[[2]], t_classical = c0[[3]],
    se_cluster = c1[[2]], t_cluster = c1[[3]], ratio = c1[[2]] / c0[[2]],
    rho = rho_vc(resid(m), bes$nuts3, k))
}
tab <- rbind(`without NUTS-1 dummies` = row(m1, 3),
             `with NUTS-1 dummies`    = row(m2, 3 + nlevels(bes$nuts1) - 1))

out <- "code/fig_brexit_cluster_output.txt"
sink(out)
cat(sprintf("Respondents: %d ; NUTS-3 regions (clusters): %d ; NUTS-1 regions: %d\n",
            nrow(bes), nlevels(bes$nuts3), nlevels(bes$nuts1)))
cat(sprintf("Respondents per NUTS-3 region: mean %.0f, median %.0f, min %d, max %d\n",
            mean(size), median(size), min(size), max(size)))
cat(sprintf("Share intending Leave: %.3f\n", mean(bes$leave)))
cat("NUTS-1 regions:", levels(bes$nuts1), "\n\n")
cat("> m1 <- lm(leave ~ import_shock + age + gender, data = bes)\n")
cat("> m2 <- lm(leave ~ import_shock + age + gender + nuts1, data = bes)\n")
cat("> coeftest(m1); coeftest(m1, vcov = vcovCL(m1, cluster = ~ nuts3))   # and the same for m2\n\n")
print(round(tab, 4))
cat("\nclustered / classical SE: ", sprintf("%.2f (without), %.2f (with NUTS-1 dummies)\n", tab[1, "ratio"], tab[2, "ratio"]))
nbar <- mean(size)
cat(sprintf("Moulton factor sqrt(1 + (nbar - 1) rho) with nbar = %.0f: %.2f (without), %.2f (with)\n",
            nbar, sqrt(1 + (nbar - 1) * tab[1, "rho"]), sqrt(1 + (nbar - 1) * tab[2, "rho"])))
sink()
cat("wrote", out, "\n")
