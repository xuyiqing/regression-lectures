# fig_gerber_icc.R --- household clustering in the Gerber, Green & Larimer
# (2008) turnout experiment: conventional versus cluster-adjusted standard error
# and 95% CI for mean turnout (lecture 2.4, "Example: Gerber Voter Turnout Data").
#
# Run from the deck folder:   Rscript code/fig_gerber_icc.R
# Reads  code/data/gerber_reduced.dta: 14,392 registered voters in 7,599 whole
#        households (every household keeps all of its members), a subsample of
#        the experiment's 344,084 voters; outcome = voted in the August 2006
#        Michigan primary.
# Writes code/fig_gerber_icc.txt, the numbers typeset on the slide. No figure.
#
# Fix relative to the 2015 slide. The old code ran lm(vote ~ hh_id): the
# household ID entered as a NUMBER, so the "between" row of anova() had 1 degree
# of freedom (a linear trend in the ID) and rho-hat came out as 0.954. The
# households have to enter as groups, with G - 1 = 7,598 degrees of freedom.
# lm(vote ~ factor(hh_id)) would build a 14,392 x 7,599 model matrix, so the
# mean squares are computed from the household means; a check on 300
# households confirms they equal what anova() reports.
#
# As on the slides, unequal household sizes are handled by setting m to the
# average household size.

source("../course_figures.R")   # shared course setup (this script draws no figure)
library(foreign)

d    <- read.dta("code/data/gerber_reduced.dta")
vote <- as.numeric(d$voted == "Yes")
hh   <- factor(d$hh_id)

# One-way ANOVA mean squares with the groups given by g.
mean_squares <- function(x, g) {
  g  <- droplevels(factor(g))
  gm <- ave(x, g)                              # each voter's household mean
  c(MSB = sum((gm - mean(x))^2) / (nlevels(g) - 1),
    MSW = sum((x - gm)^2) / (length(x) - nlevels(g)))
}

# Check against anova() on a subset small enough for lm() with a factor.
sub <- hh %in% levels(hh)[1:300]
a   <- anova(lm(vote[sub] ~ droplevels(hh[sub])))
stopifnot(isTRUE(all.equal(unname(mean_squares(vote[sub], hh[sub])),
                           a[["Mean Sq"]])))

n    <- length(vote)
G    <- nlevels(hh)
m    <- n / G                                  # average household size
ms   <- mean_squares(vote, hh)
rho  <- unname((ms["MSB"] - ms["MSW"]) / (ms["MSB"] + (m - 1) * ms["MSW"]))
xbar <- mean(vote)

se_conv <- sqrt(var(vote) / n)
se_clus <- sqrt(var(vote) / n * (1 + (m - 1) * rho))
ci_conv <- xbar + c(-1, 1) * qt(0.975, n - 1) * se_conv
ci_clus <- xbar + c(-1, 1) * qt(0.975, G - 1) * se_clus

# For the record only (not on the slide): the 2015 computation, and a
# cluster-robust (sandwich) standard error that uses the actual household sizes.
old     <- anova(lm(vote ~ d$hh_id))
rho_old <- (old[1, 3] - old[2, 3]) / (old[1, 3] + (m - 1) * old[2, 3])
se_cr   <- sqrt(sum(tapply(vote - xbar, hh, sum)^2) * G / (G - 1)) / n

f4 <- function(x) formatC(x, format = "f", digits = 4)
f3 <- function(x) formatC(x, format = "f", digits = 3)
f5 <- function(x) formatC(x, format = "f", digits = 5)
out <- c(
  "Gerber, Green & Larimer (2008), code/data/gerber_reduced.dta",
  sprintf("n = %s voters, G = %s households, average size m = n/G = %s",
          format(n, big.mark = ","), format(G, big.mark = ","), f3(m)),
  sprintf("household sizes: %s",
          paste(names(table(table(hh))), table(table(hh)), sep = ":", collapse = "  ")),
  sprintf("mean turnout = %s", f4(xbar)),
  sprintf("MSB = %s (df %d), MSW = %s (df %d)", f4(ms["MSB"]), G - 1, f4(ms["MSW"]), n - G),
  sprintf("rho-hat = %s", f3(rho)),
  sprintf("conventional:     SE = %s, 95%% CI [%s, %s], width %s",
          f5(se_conv), f4(ci_conv[1]), f4(ci_conv[2]), f4(diff(ci_conv))),
  sprintf("cluster-adjusted: SE = %s, 95%% CI [%s, %s], width %s",
          f5(se_clus), f4(ci_clus[1]), f4(ci_clus[2]), f4(diff(ci_clus))),
  sprintf("SE ratio conventional / cluster-adjusted = %s (conventional is %s%% too small)",
          f3(se_conv / se_clus), round(100 * (1 - se_conv / se_clus))),
  sprintf("SE ratio cluster-adjusted / conventional = %s", f3(se_clus / se_conv)),
  "--- values as typeset on the slide ---",
  sprintf("n = %s; G = %s; m = %s; turnout = %s; rho-hat = %s",
          format(n, big.mark = ","), format(G, big.mark = ","),
          formatC(m, format = "f", digits = 2), f4(xbar),
          formatC(rho, format = "f", digits = 2)),
  sprintf("Conventional (i.i.d.) & %s & [%s, %s]", f5(se_conv), f4(ci_conv[1]), f4(ci_conv[2])),
  sprintf("Cluster-adjusted & %s & [%s, %s]", f5(se_clus), f4(ci_clus[1]), f4(ci_clus[2])),
  sprintf("understates the standard error by %d%%", round(100 * (1 - se_conv / se_clus))),
  "--- not on the slide ---",
  sprintf("2015 slide (hh_id as a number, between df = %d): rho-hat = %s, SE = %s",
          old[1, 1], f3(rho_old),
          f5(sqrt(var(vote) / n * (1 + (m - 1) * rho_old)))),
  sprintf("cluster-robust sandwich SE (actual household sizes, G/(G-1) factor) = %s", f5(se_cr))
)
writeLines(out, "code/fig_gerber_icc.txt")
writeLines(out)
