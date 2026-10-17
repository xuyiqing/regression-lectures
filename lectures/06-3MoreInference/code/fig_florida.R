# fig_florida.R --- lecture 6-3, "Example: Buchanan Votes in Florida".
#
# Redraws the 2009 base-R figures fig1, fig2, fig3, fig4, fig5, fig1a, fig2a,
# fig5a with the same content: Buchanan votes against total votes in the 67
# Florida counties (2000), the residual plot, the spread-location plot
# (|standardized residuals| against fitted values, with a loess line), and the
# same three plots after taking logs of both variables. Palm Beach is marked in
# every panel so it can be followed from plot to plot.
#
# Data: code/data/REPLICATION_DATA.asc, the county-level replication file of
# Wand et al. (2001, APSR), "The Butterfly Did It" (67 counties, 47 columns).
#
# Also writes code/fig_florida_output.txt: the R output shown on the slides
# (summary of the linear model) and a check of the numbers on the
# Breusch-Pagan and robust-SE slides, computed here with base R and sandwich.
#
# Run from the deck folder:  Rscript code/fig_florida.R

source("../course_figures.R")
library(sandwich)

dta <- read.table("code/data/REPLICATION_DATA.asc", header = TRUE)
stopifnot(nrow(dta) == 67)
pb <- dta$name == "PALM BEACH"
big_lab <- c(BROWARD = "Broward", DADE = "Miami-Dade")
big_pos <- c(BROWARD = 3, DADE = 1)    # Broward above its point, Miami-Dade below

mod1 <- lm(buchanan00 ~ TotalVotes00, data = dta)
dta$lnbuchanan00 <- log(dta$buchanan00)
dta$lnTotalVotes00 <- log(dta$TotalVotes00)
mod2 <- lm(lnbuchanan00 ~ lnTotalVotes00, data = dta)

residuals <- resid(mod1)
standardized_residuals <- rstandard(mod1)

# ---- drawing helpers -------------------------------------------------------
col_pts <- alpha_col(col_dark, 0.55)
col_fit <- pal[["blue"]]

pts <- function(x, y, lab_pos = 4, cex_pt = 0.75) {
  points(x[!pb], y[!pb], pch = 16, cex = cex_pt, col = col_pts)
  points(x[pb], y[pb], pch = 16, cex = cex_pt * 1.35, col = col_cardinal)
  text(x[pb], y[pb], "Palm Beach", pos = lab_pos, offset = 0.4,
       col = col_cardinal, cex = 0.9)
}

frame_plot <- function(x, y, xlab, ylab, xlim = range(x), ylim = range(y),
                       xat = NULL, xlabs = TRUE, yat = NULL, ylabs = TRUE,
                       ylab_line = 2.1) {
  plot(NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = xlab, ylab = "")
  title(ylab = ylab, line = ylab_line)
  axis(1, at = xat, labels = xlabs)
  axis(2, at = yat, labels = ylabs)
  box()
}

comma <- function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)

# column figures share one size, so the overlay steps do not jump
col_w <- 3.2
col_h <- 2.8
col_ps <- 10       # slightly smaller type: these sit beside footnote-size bullets
col_mar <- c(3.1, 3.5, 0.6, 0.6)

# ---- 1. votes: full slide and column -------------------------------------
tv <- dta$TotalVotes00 / 1000
bv <- dta$buchanan00
xl_tv <- "Total votes cast (thousands)"
yl_bv <- "Votes for Buchanan"

draw_votes <- function(full) {
  frame_plot(tv, bv, xl_tv, yl_bv, xlim = c(0, 650), ylim = c(0, 3600),
             xat = seq(0, 600, 100), yat = seq(0, 3500, 500),
             ylabs = comma(seq(0, 3500, 500)), ylab_line = 2.9)
  abline(coef(mod1)[1], coef(mod1)[2] * 1000, col = col_fit, lwd = 2)
  pts(tv, bv, lab_pos = 4, cex_pt = if (full) 0.8 else 0.75)
  if (full) {
    for (nm in names(big_lab)) {
      i <- dta$name == nm
      text(tv[i], bv[i], big_lab[[nm]], pos = big_pos[[nm]], offset = 0.45, col = col_dark, cex = 0.85)
    }
    text(640, coef(mod1)[1] + coef(mod1)[2] * 640000, "OLS fit", pos = 1,
         offset = 0.6, col = col_fit, cex = 0.9)
  }
}
fig_pdf("fl_votes", draw_votes(TRUE), mar = c(3.2, 4.1, 0.6, 0.8))
fig_pdf("fl_votes_col", draw_votes(FALSE), width = col_w, height = col_h, pointsize = col_ps,
        mar = c(3.1, 4.0, 0.6, 0.6))

# ---- 2. residuals against fitted values ----------------------------------
fv <- fitted(mod1)
draw_resid <- function(full) {
  frame_plot(fv, residuals, "Fitted values", "Residuals",
             xlim = c(0, 1600), ylim = c(-1050, 2450),
             xat = seq(0, 1500, 500), xlabs = comma(seq(0, 1500, 500)),
             yat = seq(-1000, 2000, 1000), ylabs = comma(seq(-1000, 2000, 1000)),
             ylab_line = 2.9)
  abline(h = 0, col = col_grey, lty = 2)
  pts(fv, residuals, lab_pos = 4, cex_pt = if (full) 0.8 else 0.75)
}
fig_pdf("fl_resid", draw_resid(TRUE), mar = c(3.2, 4.1, 0.6, 0.8))
fig_pdf("fl_resid_col", draw_resid(FALSE), width = col_w, height = col_h, pointsize = col_ps,
        mar = c(3.1, 4.0, 0.6, 0.6))

# ---- 3. spread-location plot, without and with loess (same axes) ---------
# The smooth line: the 2009 figures drew a default loess() fit (span 0.75,
# degree 2; checked against their vector paths to 0.0002). The same smoother
# is used here, and the slide text calls it a loess line.
smooth_line <- function(x, y) {
  o <- order(x)
  list(x = x[o], y = fitted(loess(y ~ x))[o])
}
ar <- abs(standardized_residuals)
lw1 <- smooth_line(fv, ar)
draw_spread <- function(with_loess) {
  frame_plot(fv, ar, "Fitted values", "|Standardized residuals|",
             xlim = c(0, 1600), ylim = c(0, 8),
             xat = seq(0, 1500, 500), xlabs = comma(seq(0, 1500, 500)),
             yat = seq(0, 8, 2))
  if (with_loess) lines(lw1, col = col_fit, lwd = 2)
  pts(fv, ar, lab_pos = 4)
}
fig_pdf("fl_spread_col", draw_spread(FALSE), width = col_w, height = col_h, pointsize = col_ps, mar = col_mar)
fig_pdf("fl_spread_loess_col", draw_spread(TRUE), width = col_w, height = col_h, pointsize = col_ps, mar = col_mar)

# ---- 4. the same after logs (column figures) -----------------------------
lx <- dta$lnTotalVotes00
ly <- dta$lnbuchanan00
fv2 <- fitted(mod2)
r2 <- resid(mod2)
ar2 <- abs(rstandard(mod2))
lw2 <- smooth_line(fv2, ar2)

fig_pdf("fl_log_votes_col", {
  frame_plot(lx, ly, "log(total votes)", "log(Buchanan votes)",
             xlim = c(7.6, 13.6), ylim = c(2, 8.5), xat = 8:13, yat = 2:8)
  abline(mod2, col = col_fit, lwd = 2)
  pts(lx, ly, lab_pos = 2)
}, width = col_w, height = col_h, pointsize = col_ps, mar = col_mar)

fig_pdf("fl_log_resid_col", {
  frame_plot(fv2, r2, "Fitted values", "Residuals",
             xlim = c(2.8, 7.2), ylim = c(-1.2, 1.6), xat = 3:7, yat = seq(-1, 1.5, 0.5))
  abline(h = 0, col = col_grey, lty = 2)
  pts(fv2, r2, lab_pos = 2)
}, width = col_w, height = col_h, pointsize = col_ps, mar = col_mar)

fig_pdf("fl_log_spread_loess_col", {
  frame_plot(fv2, ar2, "Fitted values", "|Standardized residuals|",
             xlim = c(2.8, 7.2), ylim = c(0, 3.4), xat = 3:7, yat = 0:3)
  lines(lw2, col = col_fit, lwd = 2)
  pts(fv2, ar2, lab_pos = 2)
}, width = col_w, height = col_h, pointsize = col_ps, mar = col_mar)

# ---- 5. R output for the slides ------------------------------------------
out <- "code/fig_florida_output.txt"
sink(out)
cat("## Slide 'Buchanan Votes and Total Votes' (R code)\n")
cat("> mod1 <- lm(buchanan00~TotalVotes00,data=dta)\n> summary(mod1)\n")
print(summary(mod1))

cat("## Check of slide 'Example: Buchanan Votes in Florida' (Breusch-Pagan)\n")
cat("## bptest(..., studentize = FALSE) is half the explained sum of squares from\n")
cat("## regressing u^2 / mean(u^2) on the regressors (Breusch and Pagan 1979).\n")
bp <- function(m) {
  u2 <- resid(m)^2
  f <- u2 / mean(u2)
  aux <- lm(f ~ model.matrix(m) - 1)
  s <- 0.5 * sum((fitted(aux) - mean(f))^2)
  c(BP = s, df = ncol(model.matrix(m)) - 1, p = pchisq(s, ncol(model.matrix(m)) - 1, lower.tail = FALSE))
}
print(rbind(mod1 = bp(mod1), mod2 = bp(mod2)), digits = 7)
cat("## Footnote on the BP slide: lmtest::bptest default (Koenker studentized)\n")
print(lmtest::bptest(mod1)); print(lmtest::bptest(mod2))

cat("\n## Check of slide 'Regular & Robust Standard Errors in Florida Example'\n")
tt <- function(m, V) {
  b <- coef(m); se <- sqrt(diag(V)); t <- b / se
  cbind(Estimate = b, `Std. Error` = se, `t value` = t,
        `Pr(>|t|)` = 2 * pt(abs(t), df.residual(m), lower.tail = FALSE))
}
cat("# homoskedasticity\n");  print(signif(tt(mod1, vcov(mod1)), 5))
cat("# HC0 (classic White)\n"); print(signif(tt(mod1, vcovHC(mod1, type = "HC0")), 5))
cat("# HC1\n");               print(signif(tt(mod1, vcovHC(mod1, type = "HC1")), 5))
sink()
cat("wrote", out, "\n")
