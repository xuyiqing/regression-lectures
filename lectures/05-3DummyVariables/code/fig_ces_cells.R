# fig_ces_cells.R --- Lecture 5-3, two dummy variables and their interaction: the 2024 Trump vote by
# race and college degree, Cooperative Election Study (CES) 2024.
#
# Replaces fig_gdp_cells.R (Persson & Tabellini GDP by majoritarian x Latin America). Same figure design:
# the four predicted group means, without and with the interaction term, 95% confidence bars (robust HC2).
#
# Data: code/data/ces2024_extract.csv, copied from lectures/_data/us_elections/ (built there by build.R
# from the CES 2024 Common Content, doi:10.7910/DVN/X11EP6, CC0; codes in that folder's README.md).
#   trump    1 = voted Trump, 0 = voted Harris (post-election self-report, CC24_410); others left out
#   white    1 = non-Hispanic white (race == 1 and hispanic == 2), 0 = everyone else
#   college  1 = 4-year degree or more (educ 5 or 6), 0 = less
#   commonpostweight  CES survey weight for the post-election wave; every model and mean is weighted
#
# Writes every number the slides print to code/ces_cells_output.txt, and
#   figs/ces_cells_additive.pdf     trump ~ white + college
#   figs/ces_cells_interaction.pdf  trump ~ white * college
#
# Run from the deck folder:  Rscript code/fig_ces_cells.R

source("../course_figures.R")
options(width = 80, digits = 7)

out <- "code/ces_cells_output.txt"
sink(out)
on.exit(sink(), add = TRUE)
show <- function(cmd) {
  cat("> ", cmd, "\n", sep = "")
  print(eval(parse(text = cmd), envir = globalenv()))
  cat("\n")
}

d <- read.csv("code/data/ces2024_extract.csv")
d <- d[!is.na(d$trump), ]
stopifnot(nrow(d) == 42028, !anyNA(d$race), !anyNA(d$hispanic), !anyNA(d$educ),
          !anyNA(d$commonpostweight))
ces <- data.frame(trump = d$trump,
                  white = as.integer(d$race == 1 & d$hispanic == 2),
                  college = as.integer(d$educ >= 5),
                  commonpostweight = d$commonpostweight)
ces$white_x_college <- ces$white * ces$college
cat("CES 2024, respondents who reported voting Trump or Harris:", nrow(ces), "\n")
cat("white = non-Hispanic white; college = 4-year degree or more; weights = commonpostweight\n\n")

# ---- cells ---------------------------------------------------------------------------------
cells <- expand.grid(white = 0:1, college = 0:1)
cells$n <- mapply(function(w, c) sum(ces$white == w & ces$college == c), cells$white, cells$college)
cells$mean_unweighted <- mapply(function(w, c) mean(ces$trump[ces$white == w & ces$college == c]),
                                cells$white, cells$college)
cells$mean_weighted <- mapply(function(w, c) {
  s <- ces$white == w & ces$college == c
  weighted.mean(ces$trump[s], ces$commonpostweight[s])
}, cells$white, cells$college)
cat("Group sizes and Trump shares:\n")
print(cbind(cells[, 1:3], round(cells[, 4:5], 4)), row.names = FALSE)
wm <- function(w, c) cells$mean_weighted[cells$white == w & cells$college == c]
gap_w <- wm(1, 1) - wm(1, 0); gap_nw <- wm(0, 1) - wm(0, 0)
cat(sprintf("\nCollege gap (weighted): white %.4f, non-white %.4f; difference in differences %.4f\n",
            gap_w, gap_nw, gap_w - gap_nw))
cat(sprintf("Race gap (weighted): no degree %.4f, degree %.4f\n\n", wm(1, 0) - wm(0, 0), wm(1, 1) - wm(0, 1)))

# ---- models --------------------------------------------------------------------------------
cat("==== additive ====\n")
show("summary(lm(trump ~ white + college, data = ces, weights = commonpostweight))")
cat("==== interaction (hand-made product) ====\n")
show("summary(lm(trump ~ white + college + white_x_college, data = ces, weights = commonpostweight))")
cat("==== interaction with : and * ====\n")
show("summary(lm(trump ~ white + college + white:college, data = ces, weights = commonpostweight))")
show("summary(lm(trump ~ white * college, data = ces, weights = commonpostweight))")

fits <- list(additive = lm(trump ~ white + college, data = ces, weights = commonpostweight),
             interaction = lm(trump ~ white * college, data = ces, weights = commonpostweight))
fi <- fits$interaction
for (ty in c("HC1", "HC2")) {
  se <- sqrt(diag(sandwich::vcovHC(fi, type = ty)))
  cat(sprintf("Robust (%s) SE: interaction %.4f, t %.2f (classical weighted SE %.4f, t %.2f; ratio %.2f)\n",
              ty, se["white:college"], coef(fi)["white:college"] / se["white:college"],
              coef(summary(fi))["white:college", 2], coef(summary(fi))["white:college", 3],
              se["white:college"] / coef(summary(fi))["white:college", 2]))
}
fu <- lm(trump ~ white * college, data = ces)
seu <- sqrt(diag(sandwich::vcovHC(fu, type = "HC2")))
cat(sprintf("Unweighted interaction %.4f, classical t %.2f, HC2 t %.2f\n\n",
            coef(fu)["white:college"], coef(summary(fu))["white:college", 3],
            coef(fu)["white:college"] / seu["white:college"]))

# four-group dummies give the same fit
ces$g_w_nocol <- ces$white * (1 - ces$college)
ces$g_nw_col  <- (1 - ces$white) * ces$college
ces$g_w_col   <- ces$white * ces$college
fg <- lm(trump ~ g_w_nocol + g_nw_col + g_w_col, data = ces, weights = commonpostweight)
cat("Four-group dummies (baseline: non-white, no degree):\n"); print(round(coef(fg), 4))
cat("Max |fitted(four-group) - fitted(interaction)|:", signif(max(abs(fitted(fg) - fitted(fi))), 3), "\n")
cat("Interaction-model fitted values by group (= weighted group means):\n")
print(cbind(cells[, 1:2], fit = round(predict(fi, cells), 4)), row.names = FALSE)
cat("\n")

# ---- figures -------------------------------------------------------------------------------
col_col <- col_cardinal
col_nocol <- pal[["blue"]]
draw_cells <- function(fit) {
  # 95% intervals from robust (HC2) standard errors: lm()'s own SEs treat survey weights as precision
  # weights and come out too small here (see the robust-SE line in the output)
  X <- model.matrix(delete.response(terms(fit)), cells[, c("white", "college")])
  fitv <- drop(X %*% coef(fit))
  sev <- sqrt(rowSums((X %*% sandwich::vcovHC(fit, type = "HC2")) * X))
  pr <- cbind(fit = fitv, lwr = fitv - 1.96 * sev, upr = fitv + 1.96 * sev)
  print(cbind(cells[, c("white", "college")], round(pr, 4)))
  plot(NA, xlim = c(-0.3, 1.3), ylim = c(0.2, 0.75), axes = FALSE, xlab = "", ylab = "")
  title(ylab = "Share voting Trump (weighted)", line = 3.0)
  axis(1, at = 0:1, labels = c("Non-white (incl. Hispanic)", "White (non-Hispanic)"))
  axis(2, at = seq(0.2, 0.7, 0.1), labels = sprintf("%.1f", seq(0.2, 0.7, 0.1)))
  gap <- pr[cells$college == 1, "fit"] - pr[cells$college == 0, "fit"]
  nudge <- ifelse(abs(gap) < 0.06, 0.022 * sign(gap + 1e-9), 0)
  for (cl in 0:1) {
    s <- cells$college == cl
    dx <- if (cl == 1) 0.045 else -0.045
    col <- if (cl == 1) col_col else col_nocol
    x <- cells$white[s] + dx
    lines(x, pr[s, "fit"], col = col, lwd = 2)
    arrows(x, pr[s, "lwr"], x, pr[s, "upr"], length = 0.04, angle = 90,
           code = 3, col = alpha_col(col, 0.7), lwd = 1.5)
    points(x, pr[s, "fit"], pch = 16, col = col, cex = 1.1)
    xlab_at <- ifelse(x < 0.5, -0.045, 1.045)
    text(xlab_at, pr[s, "fit"] + (if (cl == 1) nudge else -nudge),
         sprintf("%.3f", pr[s, "fit"]),
         pos = ifelse(x < 0.5, 2, 4), offset = 0.7, cex = 0.85, col = col)
  }
  legend("top", horiz = TRUE, bty = "n", cex = 0.9, lwd = 2, pch = 16,
         legend = c("No 4-year degree", "4-year degree or more"), col = c(col_nocol, col_col))
}
ces_mar <- c(2.4, 4.0, 0.6, 0.8)
cat("Predicted means with 95% confidence intervals (weighted lm, HC2 SEs):\nadditive\n")
fig_pdf("ces_cells_additive", mar = ces_mar, draw_cells(fits$additive))
cat("interaction\n")
fig_pdf("ces_cells_interaction", mar = ces_mar, draw_cells(fits$interaction))
