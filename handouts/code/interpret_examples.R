## Numbers quoted in interpret_reg_coeffs.tex.
## Run from this folder:  Rscript interpret_examples.R > interpret_examples.txt
## Data (see SOURCES.md): countries_xsec.csv (V-Dem v16 and World Bank WDI, 2024),
## state_pres.csv and county_pres.csv (MEDSL election returns, Census ACS 2019-2023).

countries <- read.csv("data/countries_xsec.csv")
states    <- read.csv("data/state_pres.csv")
counties  <- read.csv("data/county_pres.csv")

states   <- subset(states, is_dc == 0)                # the 50 states, as in lecture 4-3
counties <- subset(counties, !is.na(rep2p_2024))      # the 3,034 counties used in lecture 3

countries$gdp1000 <- countries$gdppc / 1000           # GDP per capita, thousands of dollars
countries$wshare  <- countries$women / 100            # women's share of the legislature, 0 to 1
states$bashare    <- states$pct_ba / 100              # share of adults 25+ with a BA, 0 to 1
counties$bashare  <- counties$pct_ba / 100

show <- function(label, fit) {
  cat("\n====", label, "====\n")
  cat("n =", nobs(fit), "\n")
  print(signif(coef(summary(fit)), 4))
  invisible(fit)
}
pred <- function(fit, ...) unname(predict(fit, newdata = data.frame(...)))

## ---- Section 1: reporting, Trump vote on college share across the 50 states ----
m_st <- show("States: Trump share (0-1) on BA share (0-1)", lm(rep2p_2024 ~ bashare, states))
b <- coef(m_st)[2]; se <- coef(summary(m_st))[2, 2]
ci <- b + c(-1, 1) * qt(.975, df.residual(m_st)) * se
cat("95% CI for the slope:", round(ci, 3), "\n")
cat("p-value:", signif(coef(summary(m_st))[2, 4], 3), "\n")
cat("mean Trump share:", round(mean(states$rep2p_2024), 4), "\n")
cat("SD BA share:", round(sd(states$bashare), 4), " SD Trump share:", round(sd(states$rep2p_2024), 4), "\n")
cat("one SD of BA share times slope:", round(sd(states$bashare) * b, 4),
    " = ", round(sd(states$bashare) * b / sd(states$rep2p_2024), 3), "SD of Trump share\n")
q <- quantile(states$bashare, c(.25, .75), type = 1)
cat("25th and 75th percentile BA share (type 1):", round(q, 4), "\n")
print(states[states$bashare %in% q, c("state", "pct_ba", "rep2p_2024")])
cat("predicted difference, 75th minus 25th:", round(b * diff(q), 4), "\n")
cat("range of BA share:", round(range(states$bashare), 4), "\n")
print(states[states$bashare %in% range(states$bashare), c("state", "pct_ba")])

## ---- Section 1 caveat: support of X, women's share of the legislature ----
cat("\nWomen's share of the legislature, 168 countries with corruption and women's share\n")
w <- subset(countries, !is.na(wshare) & !is.na(corr100))
cat("n =", nrow(w), " range:", round(range(w$wshare), 3), "\n")
print(w[w$wshare %in% range(w$wshare), c("country", "women")])
qw <- quantile(w$wshare, c(.25, .75), type = 1)
cat("25th and 75th percentile (type 1):", round(qw, 3), "\n")
print(w[w$wshare %in% qw, c("country", "women")])

## ---- Section 3: one regression per case ----
## Level-level, neither variable a share
m1 <- show("Corruption (0-100) on GDP per capita ($1,000s)", lm(corr100 ~ gdp1000, countries))
cat("fitted at $20,000 and $21,000:", round(pred(m1, gdp1000 = c(20, 21)), 3), "\n")

## Level-level, both shares
cat("\nfitted Trump share at BA share .30, .31, .303:",
    round(pred(m_st, bashare = c(.30, .31, .303)), 5), "\n")
f <- pred(m_st, bashare = c(.30, .303))
cat("relative change from a 1 percent rise in BA share (.30 to .303):",
    round(100 * (f[2] - f[1]) / f[1], 4), "percent\n")

## Level-level, Y a share, X not
m3 <- show("Women's share (0-1) on GDP per capita ($1,000s)", lm(wshare ~ gdp1000, countries))
cat("fitted at $20,000 and $21,000:", round(pred(m3, gdp1000 = c(20, 21)), 5), "\n")
cat("fitted at $20,000 and $30,000:", round(pred(m3, gdp1000 = c(20, 30)), 5), "\n")
cat("mean women's share:", round(mean(countries$wshare[!is.na(countries$gdp1000)], na.rm = TRUE), 4), "\n")

## Level-level, Y not a share, X a share
m4 <- show("Corruption (0-100) on women's share (0-1)", lm(corr100 ~ wshare, countries))
cat("fitted at women's share .25 and .26:", round(pred(m4, wshare = c(.25, .26)), 3), "\n")
cat("fitted at the 25th and 75th percentile:", round(pred(m4, wshare = qw), 3),
    " difference:", round(diff(pred(m4, wshare = qw)), 3), "\n")

## Log-level
m5 <- show("log GDP per capita on corruption (0-100)", lm(log(gdppc) ~ corr100, countries))
g <- exp(pred(m5, corr100 = c(50, 51)))
cat("fitted GDP per capita at corruption 50 and 51:", round(g, 1),
    " percent change:", round(100 * (g[2] / g[1] - 1), 3), "\n")

## Log-level, X binary
m6 <- show("log GDP per capita on democracy (0/1)", lm(log(gdppc) ~ democracy, countries))
b6 <- coef(m6)[2]
cat("100*b:", round(100 * b6, 1), "  exact 100*(exp(b)-1):", round(100 * (exp(b6) - 1), 1), "\n")
cat("fitted GDP per capita (exp of fitted log), autocracies and democracies:",
    round(exp(pred(m6, democracy = c(0, 1))), 0), "\n")
cat("geometric means by group:", round(tapply(countries$gdppc, countries$democracy,
    function(x) exp(mean(log(x[!is.na(x)])))), 0), "\n")
cat("counts by group (with GDP):", table(countries$democracy[!is.na(countries$gdppc)]), "\n")

## Log-level, X a share
m7 <- show("Counties: log density on BA share (0-1)", lm(log(density) ~ bashare, counties))
d <- exp(pred(m7, bashare = c(.20, .21)))
cat("fitted density at BA share .20 and .21:", round(d, 3),
    " percent change:", round(100 * (d[2] / d[1] - 1), 3), "\n")

## Level-log
m8 <- show("Corruption (0-100) on log GDP per capita", lm(corr100 ~ log(gdppc), countries))
cat("fitted at $20,000 and $20,200:", round(pred(m8, gdppc = c(20000, 20200)), 4),
    " difference:", round(diff(pred(m8, gdppc = c(20000, 20200))), 4), "\n")
cat("doubling GDP per capita, b*log(2):", round(coef(m8)[2] * log(2), 2), "\n")

## Level-log, Y a share
m9 <- show("Counties: Trump share (0-1) on log density", lm(rep2p_2024 ~ log(density), counties))
cat("fitted at density 100 and 101:", round(pred(m9, density = c(100, 101)), 6),
    " difference:", round(diff(pred(m9, density = c(100, 101))), 6), "\n")
cat("tenfold density, b*log(10):", round(coef(m9)[2] * log(10), 4), "\n")

## Log-log
m10 <- show("Counties: log votes cast 2024 on log population", lm(log(totalvotes_2024) ~ log(pop), counties))
v <- exp(pred(m10, pop = c(50000, 50500)))
cat("fitted votes at population 50,000 and 50,500:", round(v, 1),
    " percent change:", round(100 * (v[2] / v[1] - 1), 4), "\n")
