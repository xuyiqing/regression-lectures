# fig_donovan.R --- lecture 6-3, serial correlation example (v2, 2026-10-11).
#
# Replaces the Bush approval vs gas price example (two trending series; data
# read back from a 2009 figure). New example: monthly presidential approval
# among independents on business expectations, with administration dummies,
# from the replication data of Donovan, Kellstedt, Key & Lebo (2020),
# "Motivated Reasoning, Public Opinion, and Presidential Approval," Political
# Behavior 42(4): 1201-1221 (Harvard Dataverse doi:10.7910/DVN/MN4PT4, CC0).
#
# Draws figs/donovan_series.pdf (two stacked panels on one time axis, as the
# old approval/gas figure) and writes code/fig_donovan_output.txt: the R
# output on the serial-correlation slides (OLS, Durbin-Watson, Newey-West)
# and the first-difference regression.
#
# Data: code/data/donovan2020_monthly.csv (copy of
# lectures/_data/papers/donovan2020/donovan2020_monthly.csv). The model uses
# the 416 consecutive months with both series, January 1981 to August 2015.
#   ind     presidential approval among independents (%)
#   bus5yr  Index of Consumer Sentiment, 5-year business expectations
#   clinton, gwb, obama  administration dummies (Reagan-Bush is the baseline)
#
# Run from the deck folder:  Rscript code/fig_donovan.R

source("../course_figures.R")
suppressPackageStartupMessages({
  library(sandwich)
  library(lmtest)
})

d <- read.csv("code/data/donovan2020_monthly.csv")
d <- d[order(d$monthtot), ]
dta <- d[!is.na(d$ind) & !is.na(d$bus5yr), ]
stopifnot(nrow(dta) == 416, all(diff(dta$monthtot) == 1))
tm <- dta$year + (dta$month - 1) / 12

# ---- figure -------------------------------------------------------------------
breaks <- c(1993, 2001, 2009)                       # January of each new administration
mid <- c(1987, 1997, 2005, 2012.3)
adm <- c("Reagan & Bush", "Clinton", "W. Bush", "Obama")
fig_pdf("donovan_series", {
  layout(matrix(1:2, 2))
  panel <- function(y, ylim, yat, col, ylab, bottom, labels = FALSE) {
    par(mar = c(if (bottom) 2.2 else 0.4, 4.4, if (labels) 1.3 else 0.5, 0.8))
    plot(tm, y, type = "n", xlim = c(1981, 2016), ylim = ylim,
         axes = FALSE, xlab = "", ylab = "", xaxs = "i")
    abline(v = breaks, col = col_grey, lty = 2, lwd = 0.9)
    lines(tm, y, col = col, lwd = 1.4)
    axis(2, at = yat)
    title(ylab = ylab, line = 2.6)
    if (bottom) axis(1, at = seq(1985, 2015, 5)) else axis(1, at = seq(1985, 2015, 5), labels = FALSE)
    if (labels) mtext(adm, side = 3, at = mid, line = 0.2, cex = 0.8, col = col_dark)
    box()
  }
  # panel label on a white patch, so the dashed administration lines do not run through it
  label <- function(x, y, s, col) {
    w <- strwidth(s, cex = 0.8); h <- strheight(s, cex = 0.8)
    rect(x - 0.2, y - 0.75 * h, x + w + 0.2, y + 0.75 * h, col = "white", border = NA)
    text(x, y, s, adj = c(0, 0.5), cex = 0.8, col = col)
    box()
  }
  panel(dta$ind, c(10, 95), seq(10, 90, 20), col_cardinal,
        "Approval (%)", bottom = FALSE, labels = TRUE)
  label(1981.6, 14, "Presidential approval among independents", col_cardinal)
  panel(dta$bus5yr, c(30, 160), seq(40, 160, 40), pal[["blue"]],
        "Index", bottom = TRUE)
  label(1981.6, 37, "Business expectations, next five years (ICS)", pal[["blue"]])
})

# ---- models -------------------------------------------------------------------
mod1 <- lm(ind ~ bus5yr + clinton + gwb + obama, data = dta)
nw   <- NeweyWest(mod1)                       # sandwich default: automatic lag, prewhitened
nw0  <- NeweyWest(mod1, prewhite = FALSE)     # same lag rule, no prewhitening
se   <- function(V) sqrt(diag(V))
ac1  <- function(x) cor(x[-1], x[-length(x)])
r    <- resid(mod1)
rho  <- sum(r[-1] * r[-length(r)]) / sum(r^2)  # first-order autocorrelation, as on the DW slide

# first differences (month to month changes; the administration dummies are differenced too)
fd <- data.frame(d_ind = diff(dta$ind), d_bus = diff(dta$bus5yr),
                 d_clinton = diff(dta$clinton), d_gwb = diff(dta$gwb), d_obama = diff(dta$obama))
mod_fd <- lm(d_ind ~ d_bus + d_clinton + d_gwb + d_obama, data = fd)
nw_fd  <- tryCatch(NeweyWest(mod_fd), error = function(e) NeweyWest(mod_fd, prewhite = FALSE))
nw_fd_note <- tryCatch({NeweyWest(mod_fd); "prewhitened"}, error = function(e) "not prewhitened (prewhitening fails: singular)")

# ---- R output for the slides ----------------------------------------------------
out <- "code/fig_donovan_output.txt"
sink(out)
cat("Months used:", nrow(dta), "from", sprintf("%d-%02d", dta$year[1], dta$month[1]),
    "to", sprintf("%d-%02d", dta$year[nrow(dta)], dta$month[nrow(dta)]), "\n")
cat("Months by administration:\n"); print(table(dta$admin))
cat("\n## Slide: R output, levels\n")
cat("> mod1 <- lm(ind ~ bus5yr + clinton + gwb + obama, data = dta)\n> coeftest(mod1)\n")
print(coeftest(mod1))
cat(sprintf("R2 = %.3f\n", summary(mod1)$r.squared))

cat("\n## Slide: Durbin-Watson\n")
cat("> dwtest(mod1)\n")
print(dwtest(mod1))
cat(sprintf("rho-hat (first-order autocorrelation of the residuals) = %.3f ; 2(1 - rho-hat) = %.3f\n",
            rho, 2 * (1 - rho)))
cat(sprintf("lag-1 autocorrelation of the series: ind %.3f, bus5yr %.3f\n", ac1(dta$ind), ac1(dta$bus5yr)))

cat("\n## Slide: HAC (Newey-West)\n")
cat("> coeftest(mod1, vcov = NeweyWest) # HAC errors\n")
print(coeftest(mod1, vcov = nw))
cat(sprintf("slope SE: OLS %.4f, Newey-West %.4f, ratio %.2f\n",
            se(vcov(mod1))["bus5yr"], se(nw)["bus5yr"], se(nw)["bus5yr"] / se(vcov(mod1))["bus5yr"]))
cat(sprintf("Newey-West without prewhitening (prewhite = FALSE): slope SE %.4f (ratio %.2f)\n",
            se(nw0)["bus5yr"], se(nw0)["bus5yr"] / se(vcov(mod1))["bus5yr"]))
cat("HAC / homoskedastic SE, all coefficients:\n"); print(round(se(nw) / se(vcov(mod1)), 2))

cat("\n## Slide: first differences\n")
cat("> mod_fd <- lm(d_ind ~ d_bus + d_clinton + d_gwb + d_obama, data = fd)\n> coeftest(mod_fd)\n")
print(coeftest(mod_fd))
b_fd <- coef(mod_fd)["d_bus"]
cat(sprintf("slope: levels %.4f, first differences %.4f (ratio %.2f), FD t = %.2f\n",
            coef(mod1)["bus5yr"], b_fd, b_fd / coef(mod1)["bus5yr"], coef(summary(mod_fd))["d_bus", 3]))
cat(sprintf("FD Durbin-Watson = %.3f\n", dwtest(mod_fd)$statistic))
cat(sprintf("FD Newey-West (%s): slope SE %.4f, t = %.2f\n",
            nw_fd_note, se(nw_fd)["d_bus"], b_fd / se(nw_fd)["d_bus"]))
sink()
cat("wrote", out, "\n")
