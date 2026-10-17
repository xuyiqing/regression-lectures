# fig_brexit.R --- Lecture 5-3 examples from the 2016 EU referendum (Brexit).
#
# Data: code/data/brexit_la.csv, copied from lectures/_data/brexit/ (built there by build.R from the
# Electoral Commission's results by counting area and the 2011 Census; sources and licences in that
# folder's README.md). We keep the 380 counting areas of Great Britain (England, Wales, Scotland);
# Northern Ireland (one counting area) and Gibraltar are left out.
#
#   Leave     % of valid votes cast for Leave (pct_leave)
#   Scotland  1 if the counting area is in Scotland, 0 if in England or Wales
#   NoQual    % of residents aged 16+ with no qualifications, 2011 Census (pct_noqual_2011)
#   Region    Electoral Commission region: 9 English regions, Wales, Scotland (11 categories)
#
# Writes every number and every R printout the slides show to code/brexit_output.txt, and
#   figs/brexit_parallel.pdf   Leave on NoQual, one line per group from Leave ~ Scotland + NoQual
#   figs/brexit_separate.pdf   same axes, a line fitted separately to each group
#
# Run from the deck folder:  Rscript code/fig_brexit.R

source("../course_figures.R")
options(width = 80, digits = 7)

out <- "code/brexit_output.txt"
sink(out)
on.exit(sink(), add = TRUE)
show <- function(cmd) {               # print a command as R would, then its output
  cat("> ", cmd, "\n", sep = "")
  print(eval(parse(text = cmd), envir = globalenv()))
  cat("\n")
}

b <- read.csv("code/data/brexit_la.csv")
b <- b[b$nation %in% c("England", "Wales", "Scotland"), ]
stopifnot(nrow(b) == 380, !anyNA(b$pct_leave), !anyNA(b$pct_noqual_2011))
D <- data.frame(Area = b$area, Leave = b$pct_leave, Scotland = b$scotland,
                NoQual = b$pct_noqual_2011, Region = factor(b$region))
cat("Brexit, 380 counting areas of Great Britain (Electoral Commission 2016; 2011 Census)\n")
cat("Areas: England", sum(b$nation == "England"), " Wales", sum(b$nation == "Wales"),
    " Scotland", sum(b$nation == "Scotland"), "\n\n")

# ---- 1. Two categories: Leave on the Scotland dummy -------------------------------------------
cat("==== 1. Leave ~ Scotland ====\n")
show("summary(lm(Leave ~ Scotland, data = D))")
leave.ew <- D$Leave[D$Scotland == 0]
leave.sc <- D$Leave[D$Scotland == 1]
show("summary(leave.ew)")
show("summary(leave.sc)")
cat(sprintf("Means: E&W %.2f (n %d, sd %.2f), Scotland %.2f (n %d, sd %.2f); difference %.2f\n",
            mean(leave.ew), length(leave.ew), sd(leave.ew), mean(leave.sc), length(leave.sc),
            sd(leave.sc), mean(leave.sc) - mean(leave.ew)))
tt <- t.test(Leave ~ Scotland, data = D, var.equal = TRUE)
cat(sprintf("t.test(Leave ~ Scotland, var.equal = TRUE): t = %.2f, df = %d (lm t on Scotland = %.2f)\n",
            tt$statistic, as.integer(tt$parameter),
            coef(summary(lm(Leave ~ Scotland, data = D)))["Scotland", "t value"]))
tw <- t.test(Leave ~ Scotland, data = D)
cat(sprintf("Welch t-test (unequal variances): t = %.2f, df = %.1f\n", tw$statistic, tw$parameter))
f1 <- lm(Leave ~ Scotland, data = D)
hc1 <- sqrt(diag(sandwich::vcovHC(f1, type = "HC1")))["Scotland"]
cat(sprintf("HC1 robust SE on Scotland %.3f, t %.2f\n", hc1, coef(f1)["Scotland"] / hc1))
cat("Highest Leave share in Scotland:", D$Area[D$Scotland == 1][which.max(leave.sc)],
    max(leave.sc), "; E&W areas below it:", sum(leave.ew < max(leave.sc)), "of", length(leave.ew), "\n\n")

# ---- 2. A dummy and a continuous control ------------------------------------------------------
cat("==== 2. Leave ~ Scotland + NoQual (parallel lines) ====\n")
show("summary(lm(Leave ~ Scotland + NoQual, data = D))")
fa <- lm(Leave ~ Scotland + NoQual, data = D)
cat(sprintf("Mean NoQual: E&W %.2f, Scotland %.2f; range %.1f-%.1f\n",
            mean(D$NoQual[D$Scotland == 0]), mean(D$NoQual[D$Scotland == 1]),
            min(D$NoQual), max(D$NoQual)))
fe <- lm(Leave ~ NoQual, data = D, subset = Scotland == 0)
fs <- lm(Leave ~ NoQual, data = D, subset = Scotland == 1)
cat("\nSeparate lines:\n")
cat(sprintf("  England & Wales: intercept %.2f, slope %.3f (SE %.3f), n %d\n",
            coef(fe)[1], coef(fe)[2], coef(summary(fe))[2, 2], nobs(fe)))
cat(sprintf("  Scotland:        intercept %.2f, slope %.3f (SE %.3f), n %d\n",
            coef(fs)[1], coef(fs)[2], coef(summary(fs))[2, 2], nobs(fs)))
fi <- lm(Leave ~ Scotland * NoQual, data = D)
ci <- coef(summary(fi))["Scotland:NoQual", ]
hci <- sqrt(diag(sandwich::vcovHC(fi, type = "HC1")))["Scotland:NoQual"]
cat(sprintf("Difference in slopes (Scotland:NoQual in Leave ~ Scotland * NoQual): %.4f, SE %.3f, t %.2f; HC1 t %.2f\n\n",
            ci[1], ci[2], ci[3], ci[1] / hci))

# figures: same axes in both
xl <- c(5, 40); yl <- c(20, 80)
col_ew <- pal[["blue"]]; col_sc <- col_cardinal
draw_base <- function() {
  plot(NA, xlim = xl, ylim = yl, xlab = "% of residents 16+ with no qualifications (2011)",
       ylab = "", axes = FALSE)
  title(ylab = "Leave (% of votes, 2016)", line = 2.4)
  axis(1, at = seq(5, 35, 5)); axis(2)
  points(D$NoQual[D$Scotland == 0], D$Leave[D$Scotland == 0], pch = 16, cex = 0.55,
         col = alpha_col(col_ew, 0.35))
  points(D$NoQual[D$Scotland == 1], D$Leave[D$Scotland == 1], pch = 16, cex = 0.75,
         col = alpha_col(col_sc, 0.8))
  legend("topleft", bty = "n", cex = 0.85, pch = 16, lwd = 2,
         col = c(col_ew, col_sc), legend = c("England & Wales (348 areas)", "Scotland (32 areas)"))
}
seg <- function(a, s, xr, col) segments(xr[1], a + s * xr[1], xr[2], a + s * xr[2], col = col, lwd = 2.2)
rng_ew <- range(D$NoQual[D$Scotland == 0]); rng_sc <- range(D$NoQual[D$Scotland == 1])
fig_pdf("brexit_parallel", {
  draw_base()
  seg(coef(fa)[1], coef(fa)["NoQual"], rng_ew, col_ew)
  seg(coef(fa)[1] + coef(fa)["Scotland"], coef(fa)["NoQual"], rng_sc, col_sc)
})
fig_pdf("brexit_separate", {
  draw_base()
  seg(coef(fe)[1], coef(fe)[2], rng_ew, col_ew)
  seg(coef(fs)[1], coef(fs)[2], rng_sc, col_sc)
  text(rng_ew[2], coef(fe)[1] + coef(fe)[2] * rng_ew[2], sprintf("slope %.2f", coef(fe)[2]),
       pos = 4, cex = 0.8, col = col_ew, xpd = TRUE)
  text(rng_sc[2], coef(fs)[1] + coef(fs)[2] * rng_sc[2], sprintf("slope %.2f", coef(fs)[2]),
       pos = 4, cex = 0.8, col = col_sc, xpd = TRUE)
})

# ---- 3. Multiple categories: regions ----------------------------------------------------------
cat("==== 3. Leave by region (11 regions of Great Britain) ====\n")
rm <- aggregate(Leave ~ Region, D, function(z) c(n = length(z), mean = round(mean(z), 2)))
rm <- data.frame(Region = rm$Region, rm$Leave)
print(rm[order(rm$mean), ], row.names = FALSE)
cat("\n")
# hand-made dummies, London as the baseline
D$SouthEast    <- as.integer(D$Region == "South East")
D$SouthWest    <- as.integer(D$Region == "South West")
D$East         <- as.integer(D$Region == "East")
D$EastMidlands <- as.integer(D$Region == "East Midlands")
D$WestMidlands <- as.integer(D$Region == "West Midlands")
D$Yorkshire    <- as.integer(D$Region == "Yorkshire and The Humber")
D$NorthEast    <- as.integer(D$Region == "North East")
D$NorthWest    <- as.integer(D$Region == "North West")
D$Wales        <- as.integer(D$Region == "Wales")
show(paste("summary(lm(Leave ~ SouthEast + SouthWest + East + EastMidlands + WestMidlands",
           "+ Yorkshire + NorthEast + NorthWest + Wales + Scotland, data = D))"))
show("levels(D$Region)")
show("summary(lm(Leave ~ Region, data = D))")
D$Region <- relevel(D$Region, ref = "London")
cat('> D$Region <- relevel(D$Region, ref = "London")\n')
show("summary(lm(Leave ~ Region, data = D))")

# ---- 4. For the T/F bank (not on a slide): a negative adjusted R-squared --------------------
cat("==== 4. London and Scotland only (65 areas): Leave ~ Scotland ====\n")
fl <- summary(lm(Leave ~ Scotland, data = D, subset = Region %in% c("London", "Scotland")))
cat(sprintf("n %d; Scotland %.3f (t %.3f); R2 %.6f; adjusted R2 %.4f\n",
            length(fl$residuals), fl$coefficients["Scotland", 1], fl$coefficients["Scotland", 3],
            fl$r.squared, fl$adj.r.squared))
