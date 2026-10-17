# fig_anes20_ft.R --- feeling-thermometer (FT) example, 2020 ANES.
#
#   figs/ft_distribution.pdf  share of ratings at each FT score,
#                             Joe Biden and Donald Trump
#                             (slide "Empirical Distribution of FT Scores")
#   figs/ft_intervals.pdf     95% normal intervals for the two mean scores,
#                             treating each sample SD as the known sigma
#                             (slide "Example: FT Scores Interval Estimates")
#
# Also prints the numbers used on the slide "Feeling Thermometer Example:
# Normal & Known Variance".
#
# Data: code/data/anes2020_ft.csv --- three columns of the ANES 2020 Time
# Series study, February 2022 release (anes_timeseries_2020_csv_20220210.csv):
# V200001 (case id), V201151 (pre-election FT, Joe Biden), V201152
# (pre-election FT, Donald Trump). All 8,280 respondents, raw codes kept.
# Codes outside 0-100 are dropped here: -9 refused, -4 technical error,
# 998 don't know where to rate. Unweighted: the slides treat the respondents
# as an i.i.d. sample and say so, ignoring the ANES weights and design.
#
# Run from the deck folder:  Rscript code/fig_anes20_ft.R

source("../course_figures.R")
source("code/lib/stable_pdf.R")   # stable_pdf(): fig_pdf() that skips unchanged figures

anes <- read.csv("code/data/anes2020_ft.csv")
stopifnot(nrow(anes) == 8280)

ft <- function(v) {
  x <- anes[[v]]
  x[!is.na(x) & x >= 0 & x <= 100]
}
cands <- list(
  list(name = "Joe Biden",    x = ft("V201151"), col = pal[["blue"]]),
  list(name = "Donald Trump", x = ft("V201152"), col = pal[["cardinal"]])
)

z <- qnorm(0.975)
for (i in seq_along(cands)) {
  x <- cands[[i]]$x
  n <- length(x); m <- mean(x); s <- sd(x)
  cands[[i]] <- c(cands[[i]], list(n = n, m = m, s = s,
                                   lo = m - z * s / sqrt(n), hi = m + z * s / sqrt(n)))
  cat(sprintf("%-16s n = %4d  mean = %.3f  sd = %.3f  95%% CI = [%.3f, %.3f]  share at multiples of 5 = %.3f\n",
              cands[[i]]$name, n, m, s, m - z * s / sqrt(n), m + z * s / sqrt(n),
              mean(x %% 5 == 0)))
}
# the slides quote these sample sizes; fail loudly if the extract ever changes
stopifnot(cands[[1]]$n == 8060, cands[[2]]$n == 8048)

# Slide "Feeling Thermometer Example": Biden, with the rounded inputs shown there.
b <- cands[[1]]
n_r <- b$n; m_r <- round(b$m, 2); s_r <- round(b$s, 1)   # mean at two decimals since 2026-10-11 (matches video 2-2)
cat(sprintf("Slide inputs: n = %d, mean = %.2f, sigma = %.1f -> [%.2f, %.2f]\n",
            n_r, m_r, s_r, m_r - 1.96 * s_r / sqrt(n_r), m_r + 1.96 * s_r / sqrt(n_r)))

comma <- function(n) formatC(n, format = "d", big.mark = ",")

# --- distribution of scores -------------------------------------------------
stable_pdf("ft_distribution", {
  layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, 0.085))
  for (cc in cands) {
    tab <- table(cc$x)
    val <- as.numeric(names(tab))
    pct <- 100 * as.numeric(tab) / cc$n
    stopifnot(max(pct) < 43)
    par(mar = c(3.2, 3.6, 3.0, 0.8))
    plot(NA, xlim = c(-3, 103), ylim = c(0, 43), xaxs = "i", yaxs = "i", axes = FALSE,
         xlab = "Feeling thermometer score", ylab = "Percent of ratings")
    axis(1, at = seq(0, 100, 25))
    axis(2, at = seq(0, 40, 10))
    rect(val - 1.3, 0, val + 1.3, pct, col = cc$col, border = NA)
    abline(v = cc$m, col = col_dark, lty = 2, lwd = 1)
    mtext(cc$name, side = 3, line = 1.55, adj = 0, font = 2, col = cc$col, cex = 1.05)
    mtext(sprintf("n = %s   mean = %.1f   SD = %.1f", comma(cc$n), cc$m, cc$s),
          side = 3, line = 0.45, adj = 0, cex = 0.9)
  }
  par(mar = c(0, 0.3, 0, 0.3))
  plot.new()
  text(0, 0.5, sprintf(paste0("Dashed line: mean.   %.0f%% of all ratings are ",
                              "multiples of 5.   Data: 2020 ANES, pre-election."),
                       100 * mean(c(cands[[1]]$x, cands[[2]]$x) %% 5 == 0)),
       adj = c(0, 0.5), cex = 0.78, col = col_grey)
}, height = 3.4)

# --- 95% intervals for the two means ----------------------------------------
stable_pdf("ft_intervals", {
  plot(NA, xlim = c(38, 52), ylim = c(0.35, 2.75), yaxs = "i", axes = FALSE,
       xlab = "Mean feeling thermometer score", ylab = "")
  axis(1, at = seq(38, 52, 2))
  ys <- c(2, 1)
  for (i in seq_along(cands)) {
    cc <- cands[[i]]; y <- ys[i]
    segments(cc$lo, y, cc$hi, y, lwd = 3, col = cc$col, lend = 1)
    segments(c(cc$lo, cc$hi), y - 0.09, c(cc$lo, cc$hi), y + 0.09, lwd = 3, col = cc$col)
    points(cc$m, y, pch = 19, cex = 1.3, col = cc$col)
    text(cc$m, y + 0.2, sprintf("%.1f   [%.1f, %.1f]", cc$m, cc$lo, cc$hi),
         adj = c(0.5, 0), col = cc$col)
    text(cc$m, y - 0.2, sprintf("n = %s,  SD = %.1f", comma(cc$n), cc$s),
         adj = c(0.5, 1), col = col_dark, cex = 0.9)
    mtext(cc$name, side = 2, at = y, las = 1, line = 0.5, col = cc$col, font = 2)
  }
  mtext(expression("95% intervals  " * bar(X)[n] %+-% 1.96 * sigma / sqrt(n) *
                     ",  with " * sigma * " set to each sample SD.   Data: 2020 ANES, pre-election."),
        side = 3, line = 0.3, adj = 1, cex = 0.8, col = col_grey)
}, mar = c(3.2, 8.2, 1.6, 0.8))
