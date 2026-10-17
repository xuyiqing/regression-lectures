## Lecture 5-4: Lecture 5-3's Brexit example as an interaction model (dummy x continuous).
## Run from the deck folder: Rscript code/scotland_interaction.R
## Writes code/scotland_interaction_output.txt. Same data and variables as 5-3's fig_brexit.R:
##   Leave     % of valid votes cast for Leave
##   Scotland  1 if the counting area is in Scotland, 0 if in England or Wales
##   NoQual    % of residents aged 16+ with no qualifications, 2011 Census
## Added 2026-10-11 (Yiqing): 5-3 says the separate slopes (1.47 vs 0.60) are taken up in 5-4.

b <- read.csv("code/data/brexit_la.csv")
b <- b[b$nation %in% c("England", "Wales", "Scotland"), ]
D <- data.frame(Leave = b$pct_leave, Scotland = b$scotland, NoQual = b$pct_noqual_2011)

sink("code/scotland_interaction_output.txt")
cat("n =", nrow(D), "\n\n")
fit <- lm(Leave ~ Scotland * NoQual, data = D)
print(summary(fit))   # R's own printout: small p-values show as <2e-16, never as 0
cf <- coef(fit)
cat("\nslope of NoQual: England & Wales", round(cf["NoQual"], 3),
    "; Scotland", round(cf["NoQual"] + cf["Scotland:NoQual"], 3), "\n")
for (g in 0:1) {
  s <- coef(lm(Leave ~ NoQual, data = D[D$Scotland == g, ]))
  cat("separate fit, Scotland =", g, ": intercept", round(s[1], 2), "slope", round(s[2], 3), "\n")
}
sink()
