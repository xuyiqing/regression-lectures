# fig_florida_hc.R --- lecture 6-3, Florida 2000 follow-up (v2, 2026-10-11).
#
# Numbers for the two frames added after the Florida residual plots and the
# robust-SE output (fig_florida.R keeps the original slides' numbers):
#   1. why the spread grows: Buchanan votes is a count, so the residual spread
#      grows with county size (residual SD by thirds of total votes);
#   2. three large counties (Miami-Dade, Broward, Palm Beach) drive the fit in
#      levels: their share of all votes, their leverage h_ii (diagonal of the
#      hat matrix P), and the slope with each left out;
#   3. the unequal spread survives dropping Palm Beach: Breusch-Pagan test and
#      the ratio of robust to classical SEs on the 66 other counties;
#   4. HC3 next to HC0 and HC1, with and without Palm Beach.
#
# Data: code/data/REPLICATION_DATA.asc (Wand et al. 2001; 67 counties).
# Draws no figure. Writes code/fig_florida_hc_output.txt.
#
# Run from the deck folder:  Rscript code/fig_florida_hc.R

suppressPackageStartupMessages({
  library(sandwich)
  library(lmtest)
})

dta <- read.table("code/data/REPLICATION_DATA.asc", header = TRUE)
stopifnot(nrow(dta) == 67)
big3 <- c("DADE", "BROWARD", "PALM BEACH")
stopifnot(all(big3 %in% dta$name))

mod1 <- lm(buchanan00 ~ TotalVotes00, data = dta)
nopb <- dta[dta$name != "PALM BEACH", ]
mod_pb <- lm(buchanan00 ~ TotalVotes00, data = nopb)
se <- function(V) sqrt(diag(V))
ses <- function(m) rbind(classical = se(vcov(m)),
                         HC0 = se(vcovHC(m, type = "HC0")),
                         HC1 = se(vcovHC(m, type = "HC1")),
                         HC3 = se(vcovHC(m, type = "HC3")))

out <- "code/fig_florida_hc_output.txt"
sink(out)

cat("## 1. Spread of the residuals by county size (thirds of total votes)\n")
third <- cut(dta$TotalVotes00, quantile(dta$TotalVotes00, 0:3 / 3), include.lowest = TRUE,
             labels = c("smallest third", "middle third", "largest third"))
cat("median total votes:\n"); print(round(tapply(dta$TotalVotes00, third, median)))
cat("residual SD, all 67 counties (levels model):\n"); print(round(tapply(resid(mod1), third, sd)))
cat("residual SD, 66 counties without Palm Beach (refit):\n")
print(round(tapply(resid(mod_pb), third[dta$name != "PALM BEACH"], sd)))
cat(sprintf("Buchanan share of total votes: median county %.2f%%, statewide %.2f%%\n",
            100 * median(dta$buchanan00 / dta$TotalVotes00),
            100 * sum(dta$buchanan00) / sum(dta$TotalVotes00)))

cat("\n## 2. Three large counties drive the fit in levels\n")
h <- hatvalues(mod1)
i3 <- match(big3, dta$name)
print(data.frame(county = big3, total_votes = dta$TotalVotes00[i3], buchanan = dta$buchanan00[i3],
                 leverage_h = round(h[i3], 3), cooks_d = round(cooks.distance(mod1)[i3], 2)),
      row.names = FALSE)
cat(sprintf("share of all votes cast in the three: %.1f%%\n",
            100 * sum(dta$TotalVotes00[i3]) / sum(dta$TotalVotes00)))
cat(sprintf("average leverage (k+1)/n = %.3f ; largest leverage among the other 64 counties = %.3f\n",
            mean(h), max(h[-i3])))
slope <- function(drop) coef(lm(buchanan00 ~ TotalVotes00, data = dta[!dta$name %in% drop, ]))[2]
cat(sprintf("slope (Buchanan votes per 1,000 votes cast): all %.2f ; without Palm Beach %.2f ; without Miami-Dade and Broward %.2f ; without all three %.2f\n",
            1000 * coef(mod1)[2], 1000 * slope("PALM BEACH"), 1000 * slope(c("DADE", "BROWARD")),
            1000 * slope(big3)))

cat("\n## 3. Without Palm Beach (66 counties)\n")
cat("> bptest(mod_pb)                      # Koenker's studentized version (bptest default)\n")
print(bptest(mod_pb))
cat("> bptest(mod_pb, studentize = FALSE)  # original Breusch-Pagan\n")
print(bptest(mod_pb, studentize = FALSE))
s_pb <- ses(mod_pb)
cat("coefficients:\n"); print(coef(mod_pb))
cat("standard errors:\n"); print(signif(s_pb, 4))
cat("ratio to classical (slope):\n"); print(round(s_pb[, "TotalVotes00"] / s_pb["classical", "TotalVotes00"], 2))

cat("\n## 4. All 67 counties: HC3 next to HC0 and HC1\n")
s_all <- ses(mod1)
print(signif(s_all, 4))
cat("ratio to classical (slope):\n"); print(round(s_all[, "TotalVotes00"] / s_all["classical", "TotalVotes00"], 2))
cat(sprintf("HC3 / HC0 (slope): all 67 counties %.2f ; without Palm Beach %.2f\n",
            s_all["HC3", 2] / s_all["HC0", 2], s_pb["HC3", 2] / s_pb["HC0", 2]))

cat("\n## Slide table: slope and its SEs in Buchanan votes per 1,000 votes cast\n")
tab <- cbind(`all 67` = c(slope = 1000 * coef(mod1)[[2]], 1000 * s_all[, 2]),
             `without Palm Beach` = c(slope = 1000 * coef(mod_pb)[[2]], 1000 * s_pb[, 2]))
print(round(tab, 2))
cat("\nHC3 weight 1/(1 - h_ii)^2 for the three counties (all 67):\n")
print(setNames(round(1 / (1 - h[i3])^2, 2), big3))
sink()
cat("wrote", out, "\n")
