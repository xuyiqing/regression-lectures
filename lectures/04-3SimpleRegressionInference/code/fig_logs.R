# fig_logs.R --- log transformations (Lecture 4-3, pp. 45-49).
#
# Run from the deck folder:  Rscript code/fig_logs.R
#
# Data: infant mortality (infant deaths by age 1 per 1,000 live births) and
# GDP per capita (US$) for 193 countries and territories, from United Nations
# social indicators, mostly 2009-2011 (R package carData, data set UN;
# Weisberg 2014, Applied Linear Regression). Rows with either value missing
# are dropped. A copy of the rows used is saved to code/data/un_carData.csv.
# This example replaces the 2010 American War Library example (killed vs.
# wounded), whose data were not kept.
#
# Writes figs/un_*.pdf and code/logs_output.txt (the coefficients quoted on the slides, and where
# the level fit crosses zero).

source("../course_figures.R")

fmt <- function(v, d) sub("^-", "\u2212", formatC(v, format = "f", digits = d))

un <- carData::UN
un <- un[complete.cases(un[, c("ppgdp", "infantMortality")]), c("region", "ppgdp", "infantMortality")]
un <- data.frame(country = rownames(un), un, row.names = NULL)
write.csv(un, "code/data/un_carData.csv", row.names = FALSE)
un$gdp <- un$ppgdp / 1000            # thousands of US$
un$im <- un$infantMortality
stopifnot(nrow(un) == 193)

m_lev <- lm(im ~ gdp, data = un)
m_loglev <- lm(log(im) ~ gdp, data = un)
m_loglog <- lm(log(im) ~ log(gdp), data = un)      # GDP in $1,000s, as on pp. 111, 114

show <- c("Afghanistan", "United States", "Qatar", "Luxembourg")
halo_text <- function(x, y, lab, pos, ...) {
  d <- strwidth("M", cex = 0.75) * 0.06; e <- strheight("M", cex = 0.75) * 0.08
  for (dx in c(-d, 0, d)) for (dy in c(-e, 0, e))
    text(x + dx, y + dy, lab, pos = pos, col = "white", ...)
  text(x, y, lab, pos = pos, col = col_dark, ...)
}
label_some <- function(xv, yv, pos = c(4, 3, 3, 2)) {
  k <- match(show, un$country)
  halo_text(xv[k], yv[k], show, pos = pos, offset = 0.35, cex = 0.75)
  points(xv[k], yv[k], pch = 21, bg = pal[["blue"]], col = "white", cex = 1.0)
}
fit_line <- function(m, xlim, ylim) {
  clip(xlim[1], xlim[2], ylim[1], ylim[2])
  abline(m, col = col_cardinal, lwd = 2.2)
  do.call(clip, as.list(par("usr")))
}
eq <- function(m, lhs, rhs_var, x, y) {
  cf <- coef(m)
  text(x, y, bquote({widehat(.(lhs)) == .(fmt(cf[1], 2))} ~ .(if (cf[2] < 0) "\u2212" else "+") ~
                      .(fmt(abs(cf[2]), if (abs(cf[2]) < 0.1) 4 else 3)) %.% .(rhs_var)),
       adj = c(1, 1), col = col_cardinal)
}
lab_gdp <- "GDP per capita (thousands of US$)"
lab_im <- "infant deaths per 1,000 live births"

# p. 45: levels
fig_pdf("un_level", width = 6, height = 3.0, {
  xlim <- c(0, 110); ylim <- c(-12, 132)
  plot(un$gdp, un$im, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
       pch = 16, cex = 0.6, col = alpha_col(col_dark, 0.55), xlab = lab_gdp, ylab = lab_im)
  fit_line(m_lev, xlim, ylim)
  label_some(un$gdp, un$im)
  eq(m_lev, quote(IM), quote(GDP), 108, 128)
})

# pp. 46-47: the outcome in levels and in logs (same size)
fig_pdf("un_hist_level", width = 6, height = 3.1, {
  h <- hist(un$im, breaks = seq(0, 130, by = 5), plot = FALSE)
  plot(h, col = alpha_col(pal[["blue"]], 0.45), border = "white", main = "",
       xlab = lab_im, ylab = "number of countries", xaxs = "i", ylim = c(0, max(h$counts) * 1.1))
  text(128, max(h$counts) * 1.05, sprintf("mean %.1f, median %.1f", mean(un$im), median(un$im)),
       adj = c(1, 1), col = col_dark, cex = 0.85)
  text(128, max(h$counts) * 0.92, "long right tail", adj = c(1, 1), col = col_grey, cex = 0.85)
})
fig_pdf("un_hist_log", width = 6, height = 3.1, mar = c(3.2, 3.6, 2.6, 0.8), {
  h <- hist(log(un$im), breaks = seq(0.5, 5, by = 0.25), plot = FALSE)
  plot(h, col = alpha_col(pal[["blue"]], 0.45), border = "white", main = "",
       xlab = "log(infant deaths per 1,000 live births)", ylab = "number of countries",
       xaxs = "i", ylim = c(0, max(h$counts) * 1.1))
  at <- c(2, 5, 10, 20, 50, 100)
  axis(3, at = log(at), labels = at, col.axis = col_grey, col = col_grey)
  mtext("same values, in deaths per 1,000 births", side = 3, line = 1.5, cex = 0.8, col = col_grey)
})

# p. 48: log-level
fig_pdf("un_loglevel", width = 6, height = 3.0, {
  xlim <- c(0, 110); ylim <- c(0.3, 5.3)
  plot(un$gdp, log(un$im), xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
       pch = 16, cex = 0.6, col = alpha_col(col_dark, 0.55), xlab = lab_gdp,
       ylab = "log(infant deaths per 1,000)")
  fit_line(m_loglev, xlim, ylim)
  label_some(un$gdp, log(un$im))
  eq(m_loglev, quote(log(IM)), quote(GDP), 108, 5.15)
})

# p. 49: log-log
fig_pdf("un_loglog", width = 6, height = 3.0, {
  xlim <- c(-2.5, 5.1); ylim <- c(0.3, 5.3)
  plot(log(un$gdp), log(un$im), xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
       pch = 16, cex = 0.6, col = alpha_col(col_dark, 0.55),
       xlab = "log(GDP per capita, thousands of US$)", ylab = "log(infant deaths per 1,000)")
  fit_line(m_loglog, xlim, ylim)
  label_some(log(un$gdp), log(un$im), pos = c(4, 1, 3, 1))
  eq(m_loglog, quote(log(IM)), quote(log(GDP)), 5.0, 5.15)
})

show_coef <- function(m, name) {
  s <- summary(m)
  sprintf("%-10s intercept %.4f  slope %.6f  (SE %.6f)  R2 %.3f", name,
          coef(m)[1], coef(m)[2], coef(s)[2, 2], s$r.squared)
}
# Level fit: the GDP per capita at which the fitted line crosses zero infant deaths, and the
# countries beyond it (added 2026-10-11, examples rebuild: one line on the level-fit slide).
x0 <- -coef(m_lev)[1] / coef(m_lev)[2]
beyond <- un[un$gdp > x0, ]
beyond <- beyond[order(-beyond$gdp), ]
out <- c(
  "# fig_logs.R output (Lecture 4-3, logs section); carData::UN, 193 countries and territories",
  sprintf("n = %d; mean IM = %.2f; median IM = %.2f", nrow(un), mean(un$im), median(un$im)),
  show_coef(m_lev, "level"),
  show_coef(m_loglev, "log-level"),
  show_coef(m_loglog, "log-log"),
  sprintf("log-level: 100*b = %.2f%%; exact 100*(exp(b)-1) = %.2f%%",
          100 * coef(m_loglev)[2], 100 * (exp(coef(m_loglev)[2]) - 1)),
  sprintf("level fit crosses zero at GDP per capita = %.4f thousand US$ (%.0f US$)", x0, 1000 * x0),
  sprintf("countries with GDP per capita above that (fitted IM < 0): %d of %d", nrow(beyond), nrow(un)),
  sprintf("  %s: GDP %.1f thousand, IM %.2f, fitted %.2f", beyond$country, beyond$gdp, beyond$im,
          predict(m_lev, beyond)),
  sprintf("lowest fitted value (at the richest, %s): %.2f",
          un$country[which.max(un$gdp)], min(fitted(m_lev)))
)
writeLines(out, "code/logs_output.txt")
cat(out, sep = "\n")
