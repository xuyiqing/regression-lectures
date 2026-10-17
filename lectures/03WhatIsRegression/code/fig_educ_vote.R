# fig_educ_vote.R --- Lecture 3, subsection "Discrete X".
# 2024 presidential vote by education, Cooperative Election Study
# (CES) 2024 Common Content (doi:10.7910/DVN/X11EP6, CC0).
#
#   Y = 1 if the respondent reported voting for Trump, 0 for Harris
#       (post-election wave, CC24_410; voters for other candidates and
#       non-voters are left out; the vote is self-reported, not validated)
#   X = education (educ, codes 1-6: No HS, High school graduate, Some college,
#       2-year, 4-year, Post-grad; labels from the CES 2024 guide, p. 29)
#
# Group shares use the CES survey weight for the post-election wave
# (commonpostweight), so each level's share describes the population of
# voters rather than the sample. Every number the slides print is written to
# code/educ_vote_output.txt.
#
#   figs/vote_marginal.pdf      distribution of Y (weighted)
#   figs/educ_vote_scatter.pdf  Y against X, jittered
#   figs/educ_vote_bars.pdf     distribution of Y within each level of X, p(y|x)
#   figs/educ_vote_means.pdf    the scatter again, with the weighted share at
#                              each level and a straight line fitted to the data
#
# Data: code/data/ces2024_extract.csv, copied from lectures/_data/us_elections/.
#
# Run from the deck folder:  Rscript --vanilla code/fig_educ_vote.R

source("../course_figures.R")

ces <- read.csv("code/data/ces2024_extract.csv")
d <- subset(ces, !is.na(trump))
stopifnot(all(d$educ %in% 1:6))
stopifnot(all(d$trump %in% 0:1), !anyNA(d$commonpostweight))
w <- d$commonpostweight

out <- file("code/educ_vote_output.txt", "w")
say <- function(...) { cat(..., file = out); cat(...) }
say("CES 2024: Trump (1) vs Harris (0) among two-party voters, by education (educ 1-6)\n")
say(sprintf("N = %d respondents\n", nrow(d)))

ed_lab <- c("No high school", "High school graduate", "Some college", "2-year degree",
            "4-year degree", "Postgraduate")
ed_short <- c("No high\nschool", "High school\ngraduate", "Some\ncollege", "2-year\ndegree",
              "4-year\ndegree", "Post-\ngraduate")

# Marginal distribution of Y ------------------------------------------------------
share_trump <- sum(w * d$trump) / sum(w)
say(sprintf("weighted share Trump = %.3f, Harris = %.3f (unweighted Trump %.3f)\n",
            share_trump, 1 - share_trump, mean(d$trump)))
col_vote <- c(Harris = pal[["blue"]], Trump = col_cardinal)

fig_pdf("vote_marginal", {
  bp <- barplot(c(1 - share_trump, share_trump), col = col_vote, border = NA,
                width = 0.6, space = 0.8, xlim = c(0, 2.6), ylim = c(0, 0.7), axes = FALSE,
                names.arg = c("Y = 0: Harris", "Y = 1: Trump"),
                ylab = "Share of voters (weighted)")
  axis(2, at = seq(0, 0.6, 0.2))
  text(bp, c(1 - share_trump, share_trump), sprintf("%.1f%%", 100 * c(1 - share_trump, share_trump)),
       pos = 3, col = col_dark)
}, mar = c(2.4, 3.6, 1.0, 0.8))

# Shares within each level of X -----------------------------------------------------
K <- 6
n_k  <- as.vector(table(factor(d$educ, levels = 1:K)))
uw_k <- as.vector(tapply(d$trump, d$educ, mean))
w_k  <- as.vector(tapply(seq_len(nrow(d)), d$educ,
                         function(i) sum(w[i] * d$trump[i]) / sum(w[i])))
stopifnot(min(n_k) >= 300)
fit_line <- lm(trump ~ educ, data = d, weights = commonpostweight)
line_k <- unname(predict(fit_line, data.frame(educ = 1:K)))
tab <- data.frame(code = 1:K, label = ed_lab, n = n_k,
                  unweighted = round(uw_k, 3), weighted = round(w_k, 3),
                  line = round(line_k, 3), weighted_minus_line = round(w_k - line_k, 3))
say("\nShare voting Trump at each level of education (weighted = commonpostweight)\n")
capture.output(print(tab, row.names = FALSE), file = out); print(tab, row.names = FALSE)
say(sprintf("smallest cell: %s, n = %s\n", ed_lab[which.min(n_k)], format(min(n_k), big.mark = ",")))
say(sprintf("straight line fitted to the same data (weighted): %.3f + %.3f x educ; largest |share - line| = %.3f (%s)\n",
            coef(fit_line)[1], coef(fit_line)[2], max(abs(w_k - line_k)),
            ed_lab[which.max(abs(w_k - line_k))]))
say(sprintf("weighted shares (%%): %s\n", paste(sprintf("%.1f", 100 * w_k), collapse = ", ")))
say(sprintf("steps between adjacent levels (points): %s\n",
            paste(sprintf("%.1f", 100 * diff(w_k)), collapse = ", ")))
cl <- sapply(1:K, function(k) { i <- d$educ == k; wi <- w[i] / sum(w[i])
  sqrt(sum(wi^2 * (d$trump[i] - w_k[k])^2)) })
say(sprintf("approx. SE of each weighted share (points, ignoring design effects beyond the weights): %s\n",
            paste(sprintf("%.1f", 100 * cl), collapse = ", ")))
stopifnot(all(diff(w_k[2:6]) < 0), which.min(n_k) == 1)

# Joint distribution: jittered scatter -----------------------------------------------
set.seed(2024)
xj <- d$educ + runif(nrow(d), -0.3, 0.3)
yj <- d$trump + runif(nrow(d), -0.07, 0.07)
ylab_vote <- "Vote: Trump (1) or Harris (0)"

scatter_panel <- function(show_means) {
  plot(NA, xlim = c(0.5, K + 0.5), ylim = c(-0.1, 1.1), axes = FALSE, xlab = "", ylab = ylab_vote)
  points(xj, yj, pch = 16, cex = 0.18,
         col = alpha_col(col_dark, if (show_means) 0.06 else 0.12))
  axis(1, at = 1:K, labels = FALSE)
  mtext(1:K, side = 1, at = 1:K, line = 0.55, cex = 0.9)
  mtext(ed_short, side = 1, at = 1:K, line = 2.1, cex = 0.72, col = col_grey)
  mtext("Education (CES code)", side = 1, line = 3.55)
  axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1"))
  if (show_means) {
    lines(1:K, line_k, col = col_grey, lwd = 1.6, lty = 2)
    segments(1:K - 0.34, w_k, 1:K + 0.34, w_k, col = col_cardinal, lwd = 3)
    points(1:K, w_k, pch = 21, bg = col_cardinal, col = "white", cex = 1.5, lwd = 1)
    text(1:K, w_k + 0.09, sprintf("%.3f", w_k), col = col_cardinal, cex = 0.8, font = 2)
    # legend in the top margin, drawn by hand so the two keys do not overlap
    yl <- 1.1 + 0.085 * 1.2
    points(0.6, yl, pch = 21, bg = col_cardinal, col = "white", cex = 1.4, xpd = NA)
    text(0.75, yl, "Share voting Trump at each level (weighted)", adj = c(0, 0.5),
         col = col_cardinal, cex = 0.8, xpd = NA)
    segments(3.95, yl, 4.3, yl, col = col_grey, lwd = 1.6, lty = 2, xpd = NA)
    text(4.4, yl, "Straight line fitted to the same data", adj = c(0, 0.5),
         col = col_grey, cex = 0.8, xpd = NA)
  }
}
fig_pdf("educ_vote_scatter", scatter_panel(FALSE), mar = c(4.6, 3.6, 1.9, 0.8))
fig_pdf("educ_vote_means", scatter_panel(TRUE), mar = c(4.6, 3.6, 1.9, 0.8))

# Conditional distributions p(y | x): one panel per level --------------------------------
fig_pdf("educ_vote_bars", {
  par(mfrow = c(2, 4), mar = c(1.7, 2.3, 1.7, 0.4), oma = c(1.3, 1.4, 0.6, 0),
      mgp = c(2, 0.45, 0), cex.axis = 0.8)
  panel <- function(p1, main, cols, n) {
    bp <- barplot(c(1 - p1, p1), col = cols, border = NA, space = 0.4,
                  ylim = c(0, 1.3), axes = FALSE, names.arg = c("0", "1"))
    axis(2, at = c(0, 0.5, 1), labels = c("0", ".5", "1"))
    mtext(main, side = 3, line = 0.45, cex = 0.66, col = col_dark)
    text(mean(bp), 1.2, paste("n =", format(n, big.mark = ",")), cex = 0.78, col = col_grey)
  }
  for (k in 1:K) panel(w_k[k], sprintf("X = %d: %s", k, ed_lab[k]), col_vote, n_k[k])
  panel(share_trump, "All voters", c(col_grey, col_grey), nrow(d))
  plot.new()
  mtext("Y: 0 = Harris, 1 = Trump", side = 1, outer = TRUE, line = 0.2, cex = 0.8)
  mtext("Share within education level (weighted)", side = 2, outer = TRUE, line = 0.2,
        cex = 0.8, las = 0)
}, mar = c(1.7, 2.3, 1.7, 0.4))

close(out)
