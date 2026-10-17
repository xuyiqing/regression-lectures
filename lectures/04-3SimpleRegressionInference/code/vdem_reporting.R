# vdem_reporting.R --- numbers for the "Reporting Substantive Significance" slide (Lecture 4-3),
# second example: democracy on GDP per capita. Replaces the Polity index (no longer updated) with
# V-Dem's electoral democracy index (2026-10-11 examples rebuild). No figure.
#
# Data: code/data/countries_xsec.csv, a copy of lectures/_data/countries/countries_xsec.csv (year 2024):
#   polyarchy  V-Dem v16 v2x_polyarchy, electoral democracy index, 0-1 (CC BY-SA 4.0)
#   gdppc      World Bank WDI NY.GDP.PCAP.PP.KD, GDP per capita, PPP, constant 2021 international $ (CC BY 4.0)
# Sources and licenses: lectures/_data/countries/README.md.
#
# Run from the deck folder:  Rscript code/vdem_reporting.R
# Writes code/vdem_output.txt.
#
# The contrast follows the old slide: go from the 25th to the 75th percentile of GDP per capita, read off
# the change in fitted polyarchy from a regression of polyarchy on log GDP per capita, and say where that
# change moves a country that starts at the 25th percentile of the democracy distribution.

d <- read.csv("code/data/countries_xsec.csv")
stopifnot(nrow(d) == 179, all(d$year == 2024), !anyNA(d$polyarchy))

# percentiles of democracy: all 179 V-Dem countries (as in _data/countries/gates_output.txt)
qp <- quantile(d$polyarchy, c(0.10, 0.25, 0.50, 0.75, 0.90))
stopifnot(round(qp[["10%"]], 3) == 0.154, round(qp[["50%"]], 3) == 0.496,
          round(qp[["90%"]], 3) == 0.840)
near <- function(v, k = 4) {
  o <- d[order(abs(d$polyarchy - v)), c("country", "polyarchy")][1:k, ]
  paste(sprintf("%s %.3f", o$country, o$polyarchy), collapse = ", ")
}

# regression: countries with WDI GDP for 2024
g <- d[!is.na(d$gdppc), ]
m <- lm(polyarchy ~ log(gdppc), data = g)
sm <- summary(m)
qg <- quantile(g$gdppc, c(0.25, 0.75))
neargdp <- function(v, k = 4) {
  o <- g[order(abs(g$gdppc - v)), c("country", "gdppc", "polyarchy")][1:k, ]
  paste(sprintf("%s $%s (polyarchy %.3f)", o$country, formatC(round(o$gdppc), big.mark = ",", format = "d"),
                o$polyarchy), collapse = "; ")
}
delta <- coef(m)[2] * (log(qg[2]) - log(qg[1]))
start <- qp[["25%"]]
end <- start + delta
pct_end <- 100 * mean(d$polyarchy <= end)     # percentile rank among all 179

pick <- c("Republic of the Congo", "Romania")  # the countries nearest the 25th and 75th GDP percentiles
pk <- g[match(pick, g$country), ]
stopifnot(!anyNA(pk$gdppc))

out <- c(
  "# vdem_reporting.R output: V-Dem polyarchy (v16) and WDI GDP per capita (PPP, 2021 int'l $), year 2024",
  "",
  sprintf("polyarchy percentiles (N = %d): 10%% %.3f; 25%% %.3f; 50%% %.3f; 75%% %.3f; 90%% %.3f",
          nrow(d), qp[1], qp[2], qp[3], qp[4], qp[5]),
  sprintf("  nearest the 10th: %s", near(qp[["10%"]])),
  sprintf("  nearest the 50th: %s", near(qp[["50%"]])),
  sprintf("  nearest the 90th: %s", near(qp[["90%"]])),
  "",
  sprintf("> lm(polyarchy ~ log(gdppc))   N = %d (countries with WDI GDP for 2024)", nrow(g)),
  capture.output(print(coef(sm), digits = 4)),
  sprintf("R2 = %.3f", sm$r.squared),
  "",
  sprintf("GDP per capita percentiles (N = %d): 25%% $%.0f; 75%% $%.0f", nrow(g), qg[1], qg[2]),
  sprintf("  nearest the 25th: %s", neargdp(qg[1])),
  sprintf("  nearest the 75th: %s", neargdp(qg[2])),
  sprintf("slide pair: %s $%s; %s $%s", pk$country[1], formatC(round(pk$gdppc[1]), big.mark = ",", format = "d"),
          pk$country[2], formatC(round(pk$gdppc[2]), big.mark = ",", format = "d")),
  sprintf("ratio 75th / 25th GDP = %.2f; log difference = %.4f", qg[2] / qg[1], log(qg[2] / qg[1])),
  sprintf("change in fitted polyarchy, 25th -> 75th GDP percentile: %.4f x %.4f = %.4f",
          coef(m)[2], log(qg[2] / qg[1]), delta),
  sprintf("a country at the 25th percentile of polyarchy (%.3f) + %.3f = %.3f, the %.1fth percentile (share of the %d countries at or below)",
          start, delta, end, pct_end, nrow(d))
)
writeLines(out, "code/vdem_output.txt")
cat(out, sep = "\n")
