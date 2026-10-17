# fig_ttest.R --- figures and numbers for Lecture 2.3 (hypothesis testing).
#
# Run from the deck folder:   Rscript code/fig_ttest.R
#
# Draws the deck's t-test figures as one set (same axes, same size, same
# cardinal shading), plus the test-by-confidence-interval figure, and writes
# every number the slides quote to code/fig_ttest_output.txt.
#
#   ttest_null            null distribution of T                 (handout p. 11)
#   ttest_crit_twosided   critical values, symbolic              (handout p. 12)
#   ttest_crit_onesided
#   ttest_rej_twosided    rejection regions with the observed T  (handout p. 13)
#   ttest_rej_onesided
#   ttest_p_onesided      p-values as tail areas                 (handout p. 16)
#   ttest_p_twosided
#   ttest_ci_biden        90/95/99% CIs against two null values  (handout p. 19)
#
# Null values on handout pp. 19 and 21: 49 and 48.4. The value 48.4 lies
# outside the 90% and 95% intervals but inside the 99% interval, so the
# test rejects it at alpha = .10 and .05 but not at .01. (The 2004 deck
# made the same point with 55 and 53.8 on the Edwards thermometer.) The
# script stops if 48.4 loses that property.
#
# Data: code/data/anes2020_ft.csv --- three columns of the ANES 2020 Time
# Series study, February 2022 release (anes_timeseries_2020_csv_20220210.csv):
# V200001 (case id), V201151 (pre-election FT, Joe Biden), V201152
# (pre-election FT, Donald Trump). All 8,280 respondents, raw codes kept.
# Codes outside 0-100 are dropped here: -9 refused, -4 technical error,
# 998 don't know where to rate. Unweighted, as on the slides. Same extract
# as Lecture 2.2's code/data/. The 2004 file and the Edwards version of
# this script are in _old/.

source("../course_figures.R")

## ---- data and the numbers on the slides ---------------------------------
anes <- read.csv("code/data/anes2020_ft.csv")
stopifnot(nrow(anes) == 8280)

jbFTS <- anes$V201151
jbFTS <- jbFTS[jbFTS >= 0 & jbFTS <= 100]

n     <- length(jbFTS)
df    <- n - 1
stopifnot(n == 8060)                          # the slides quote this n
mu_main <- 49                                 # null value of the worked example
tt49  <- t.test(x = jbFTS, mu = mu_main)
mu_alt <- 48.4                                # second null value (pp. 19, 21)
tt_alt <- t.test(x = jbFTS, mu = mu_alt)
t_obs <- unname(tt49$statistic)
c_two <- qt(0.975, df)                        # two-sided, alpha = .05
c_one <- qt(0.95, df)                         # one-sided, alpha = .05
p_one <- pt(t_obs, df, lower.tail = FALSE)    # H1: mu > 49
p_two <- 2 * p_one                            # H1: mu != 49
stopifnot(abs(p_two - tt49$p.value) < 1e-12)

conf_levels <- c(90, 95, 99)
ci <- sapply(conf_levels, function(l) t.test(jbFTS, conf.level = l / 100)$conf.int)
colnames(ci) <- conf_levels

# Test-CI duality, checked: a null value is rejected at level alpha exactly
# when it lies outside the 100(1 - alpha)% interval.
mu0s  <- c(mu_alt, mu_main)
pvals <- sapply(mu0s, function(m) t.test(jbFTS, mu = m)$p.value)
inside <- sapply(mu0s, function(m) ci[1, ] <= m & m <= ci[2, ])   # levels x null values
reject <- sapply(pvals, function(p) p < 1 - conf_levels / 100)
stopifnot(all(inside == !reject))
stopifnot(identical(unname(inside[, 1]), c(FALSE, FALSE, TRUE)),  # 48.4: out, out, in
          all(inside[, 2]))                                      # 49: always in

pct <- function(p, d = 1) paste0(formatC(100 * p, format = "f", digits = d), "%")
num <- function(x, d = 2) sub("-", "−", formatC(x, format = "f", digits = d), fixed = TRUE)

out <- "code/fig_ttest_output.txt"
sink(out)
cat("Output of code/fig_ttest.R: every number quoted on the slides.\n\n")
cat(sprintf("Biden thermometer (V201151): n = %d, df = %d\n", n, df))
cat(sprintf("mean = %.5f   sd = %.5f   se = %.5f\n", mean(jbFTS), sd(jbFTS), sd(jbFTS) / sqrt(n)))
cat(sprintf("critical values, alpha = .05: two-sided %.4f   one-sided %.4f\n", c_two, c_one))
cat(sprintf("T for H0: mu = %s: %.5f   (slides: %.3f)\n", mu_main, t_obs, t_obs))
cat(sprintf("p-value, H1: mu > %s:  %.5f   (slides: %.3f; shaded area %s)\n", mu_main, p_one, p_one, pct(p_one)))
cat(sprintf("p-value, H1: mu != %s: %.5f   (slides: %.3f; each tail %s)\n", mu_main, p_two, p_two, pct(p_one)))
for (l in conf_levels) cat(sprintf("%d%% CI: [%.4f, %.4f]\n", l, ci[1, paste(l)], ci[2, paste(l)]))
cat(sprintf("T for H0: mu = %s: %.4f   p-value %.6f\n", mu_alt, tt_alt$statistic, tt_alt$p.value))
cat("\nDecisions (two-sided). A null value is rejected at level alpha exactly when\n")
cat("it lies outside the 100(1 - alpha)% interval:\n")
for (j in seq_along(mu0s))
  cat(sprintf("  mu0 = %-4s p = %.5f   alpha = .10: %-14s alpha = .05: %-14s alpha = .01: %s\n",
              mu0s[j], pvals[j],
              ifelse(reject[1, j], "reject", "fail to reject"),
              ifelse(reject[2, j], "reject", "fail to reject"),
              ifelse(reject[3, j], "reject", "fail to reject")))
cat(sprintf("\n> t.test(x = jbFTS, mu = %s)\n", mu_main))
print(tt49)
cat(sprintf("> t.test(x = jbFTS, mu = %s)\n", mu_alt))
print(tt_alt)
sink()
cat(readLines(out)[3:17], sep = "\n")

## ---- the t-scale figure set ---------------------------------------------
# Every panel: same size, same x-range, same y-range, t(n-1) density in dark
# ink. Cardinal marks what H0 and alpha fix (shaded areas, critical values);
# blue marks the observed statistic.
xl        <- c(-3.5, 3.5)
yl        <- c(0, 0.48)
shade_col <- alpha_col(col_cardinal)
obs_col   <- pal[["blue"]]
y_crit    <- 0.30     # height of the critical-value labels
y_obs     <- 0.42     # height of the observed-T labels
y_area    <- 0.075    # height of the tail-area labels

shade <- function(from, to) {
  xs <- seq(from, to, length.out = 200)
  polygon(c(from, xs, to), c(0, dt(xs, df), 0), col = shade_col, border = NA)
}
crit <- function(x, label, y = y_crit, cex = 1) {
  segments(x, 0, x, y - 0.055, col = col_cardinal, lwd = 1.2, lty = 2)
  text(x, y, label, cex = cex, col = col_cardinal)
}
crit_sym <- function(x, label) crit(x, label, y = 0.36)   # t-quantile labels sit higher: they are wider
obs <- function(x, label, side = 1) {       # side = 1: label right of line
  segments(x, 0, x, y_obs + 0.02, col = obs_col, lwd = 1.8)
  text(x + side * 0.08, y_obs, label, adj = c((1 - side) / 2, 0.5), col = obs_col)
}
area <- function(x, p) text(x, 0.05, pct(p), cex = 0.9, col = col_dark)

panel <- function(name, main, extra) {
  fig_pdf(name, width = 3, height = 1.5, pointsize = 11,
          mar = c(2.0, 0.5, 1.2, 0.5), expr = {
    par(mgp = c(1.25, 0.3, 0), tcl = -0.25)
    plot.new()
    plot.window(xlim = xl, ylim = yl, xaxs = "i", yaxs = "i")
    extra()
    xs <- seq(xl[1], xl[2], length.out = 401)
    lines(xs, dt(xs, df), lwd = 1.6, col = col_dark)
    segments(xl[1], 0, xl[2], 0, col = col_dark, lwd = 0.8, xpd = NA)
    axis(1, at = -3:3, labels = num(-3:3, 0), lwd = 0, lwd.ticks = 0.8)
    title(xlab = expression(italic(T)))
    title(main = main, font.main = 1, cex.main = 1, line = 0.3)
  })
}

# Math labels are plotmath symbols, not Unicode strings, so they render in any locale.
lab_ta2_neg <- expression(paste(-italic(t)[alpha/2], "(", italic(n) - 1, ")"))
lab_ta2     <- expression(paste(italic(t)[alpha/2], "(", italic(n) - 1, ")"))
lab_ta      <- expression(paste(italic(t)[alpha], "(", italic(n) - 1, ")"))
obs_lab <- bquote(italic(T) == .(num(t_obs, 3)))

panel("ttest_null", expression(paste("Null distribution of ", italic(T))), function() {
  text(1.3, 0.27, bquote(italic(t)[italic(n) - 1]), adj = 0, col = col_dark)
})

panel("ttest_crit_twosided", "Two-sided test", function() {
  shade(xl[1], -c_two); shade(c_two, xl[2])
  crit_sym(-c_two, lab_ta2_neg); crit_sym(c_two, lab_ta2)
  text(c(-2.85, 2.85), y_area, expression(alpha/2), col = col_cardinal)
})

panel("ttest_crit_onesided", "One-sided test", function() {
  shade(c_one, xl[2])
  crit_sym(c_one, lab_ta)
  text(2.85, y_area, expression(alpha), col = col_cardinal)
})

panel("ttest_rej_twosided", "Two-sided test", function() {
  shade(xl[1], -c_two); shade(c_two, xl[2])
  crit(-c_two, num(-c_two)); crit(c_two, num(c_two))
  text(c(-2.85, 2.85), y_area, "2.5%", col = col_cardinal)
  obs(t_obs, obs_lab)
})

panel("ttest_rej_onesided", "One-sided test", function() {
  shade(c_one, xl[2])
  crit(c_one, num(c_one))
  text(2.85, y_area, "5%", col = col_cardinal)
  obs(t_obs, obs_lab)
})

panel("ttest_p_onesided", expression(paste("One-sided ", italic(p), "-value")), function() {
  shade(t_obs, xl[2])
  area(1.28, p_one)
  obs(t_obs, obs_lab)
})

panel("ttest_p_twosided", expression(paste("Two-sided ", italic(p), "-value")), function() {
  shade(xl[1], -t_obs); shade(t_obs, xl[2])
  area(c(-1.28, 1.28), p_one)
  obs(-t_obs, num(-t_obs, 3), side = -1); obs(t_obs, obs_lab)
})

## ---- testing with confidence intervals ----------------------------------
fig_pdf("ttest_ci_biden", width = 5.2, height = 2.0, pointsize = 11,
        mar = c(2.5, 4.0, 1.4, 0.8), expr = {
  par(mgp = c(1.45, 0.35, 0), tcl = -0.25)
  ys <- seq_along(conf_levels)
  plot.new()
  plot.window(xlim = c(47, 51), ylim = c(0.5, 3.5), xaxs = "i")
  segments(mu0s, 0.5, mu0s, 3.5, col = col_cardinal, lwd = 1.4, lty = 2)
  segments(ci[1, ], ys, ci[2, ], ys, col = obs_col, lwd = 2)
  segments(c(ci[1, ], ci[2, ]), ys - 0.13, c(ci[1, ], ci[2, ]), ys + 0.13, col = obs_col, lwd = 2)
  points(rep(mean(jbFTS), 3), ys, pch = 19, col = obs_col)
  axis(1, at = 47:51, lwd = 0.8)
  axis(2, at = ys, labels = paste0(conf_levels, "% CI"), lwd = 0, las = 1)
  for (m in mu0s)
    mtext(bquote(mu[0] == .(m)), side = 3, at = m, line = 0.15, col = col_cardinal)
  title(xlab = bquote(paste("Biden feeling thermometer (2020 ANES, ", italic(n), " = ", .(format(n, big.mark = ",")), ")")))
})
