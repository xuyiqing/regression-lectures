# fig_hopkins.R --- a real binary X for Lecture 4-2 (frames after "Example: Binary Independent Variable").
#
# Hopkins, Daniel J., John Sides, and Jack Citrin. 2019. "The Muted Consequences of Correct Information
# about Immigration." Journal of Politics 81(1): 315-320. doi:10.1086/699914.
# Study: Morning Consult online survey, March 2017 (the authors' MCdata). Respondents were assigned with
# equal probability to four arms (codebook Q001). We compare arm A (control: no information) with arm C
# (information: "the Census Bureau has estimated that about 13 out of every 100 people living in the
# United States are immigrants who were born outside of the U.S."), non-Hispanic respondents only, as the
# authors do. After the treatment everyone answered three attitude items (Q004-Q006) and then guessed again
# how many of every 100 US residents were born abroad (Q007).
#
# Data: code/data/hopkins2019_mc2017.csv, a copy of lectures/_data/papers/hopkins2019/hopkins2019_mc2017.csv
# (built there by build.R from Harvard Dataverse doi:10.7910/DVN/ICBCM2, CC0). Variables used:
#   group       arm A, B, C or D;   hisp  1 if Hispanic (dropped)
#   guess_post  Q007 guess of % foreign-born (0-100), after treatment
#   index       restrict + takejobs + threaten (authors' INDEX, 3-13, higher = more restrictive)
# The numbers match lectures/_data/papers/hopkins2019/gates_output.txt (checked by stopifnot below).
#
# Run from the deck folder:  Rscript code/fig_hopkins.R
# Writes: code/hopkins_output.txt (every number the slides print) and figs/hopkins_means.pdf.

source("../course_figures.R")

d <- read.csv("code/data/hopkins2019_mc2017.csv")
s <- d[d$group %in% c("A", "C") & d$hisp == 0, ]
hsc <- data.frame(info = s$info, guess = s$guess_post, index = s$index)

options(useFancyQuotes = FALSE, width = 80, digits = 7)

# ---------------------------------------------------------------- guess of % foreign-born
g <- hsc[!is.na(hsc$guess), ]
lm.guess <- lm(guess ~ info, data = hsc)   # lm drops the rows with no guess
sg <- summary(lm.guess); cg <- coef(sg)
# ---------------------------------------------------------------- attitude index
a <- hsc[!is.na(hsc$index), ]
lm.index <- lm(index ~ info, data = hsc)   # lm drops the rows with no index
si <- summary(lm.index); ci <- coef(si)

grp <- function(y, x) {
  t(sapply(c(0, 1), function(k) {
    v <- y[x == k]; m <- mean(v); se <- sd(v) / sqrt(length(v))
    c(info = k, n = length(v), mean = m, median = median(v), sd = sd(v),
      lo = m - qnorm(0.975) * se, hi = m + qnorm(0.975) * se)
  }))
}
mg <- grp(g$guess, g$info)
mi <- grp(a$index, a$info)

# gate numbers (lectures/_data/papers/hopkins2019/gates_output.txt)
stopifnot(nrow(g) == 3808, nobs(lm.guess) == 3808, nobs(lm.index) == 3028, round(cg[2, 1], 3) == -6.394, round(cg[2, 2], 3) == 0.748,
          round(cg[2, 3], 2) == -8.54, round(mg[1, "mean"], 2) == 32.49, round(mg[2, "mean"], 2) == 26.10,
          mg[1, "median"] == 25, mg[2, "median"] == 13,
          nrow(a) == 3028, round(ci[2, 1], 3) == 0.078, round(ci[2, 2], 3) == 0.101,
          round(ci[2, 3], 2) == 0.77)
# with a binary X the slope is the difference in means and the intercept is the control mean
stopifnot(abs(cg[1, 1] - mg[1, "mean"]) < 1e-10, abs(cg[2, 1] - diff(mg[, "mean"])) < 1e-10,
          abs(ci[2, 1] - diff(mi[, "mean"])) < 1e-10)
share13 <- mean(g$guess[g$info == 1] == 13); share13c <- mean(g$guess[g$info == 0] == 13)

out <- c(
  "# fig_hopkins.R output (Lecture 4-2): Hopkins, Sides & Citrin (2019), Morning Consult March 2017",
  "# arms A (control, info = 0) and C (information, info = 1), non-Hispanic respondents",
  "",
  "> lm.guess <- lm(guess ~ info, data = hsc)",
  "> summary(lm.guess)",
  capture.output(sg),
  "",
  "> lm.index <- lm(index ~ info, data = hsc)",
  "> summary(lm.index)",
  capture.output(si),
  "",
  "# ---- group summaries (lo/hi = mean -/+ 1.96 SE of the group mean; drawn in figs/hopkins_means.pdf)",
  "guess of % foreign-born:",
  capture.output(print(round(mg, 2))),
  "attitude index (3-13, higher = more restrictive):",
  capture.output(print(round(mi, 3))),
  "",
  "# ---- numbers on the slides",
  sprintf("guess: control mean %.1f, information mean %.1f; difference %.2f (SE %.2f, t %.2f); N = %s",
          mg[1, "mean"], mg[2, "mean"], cg[2, 1], cg[2, 2], cg[2, 3], format(nrow(g), big.mark = ",")),
  sprintf("guess: intercept %.4f = control mean %.4f; intercept + slope = %.4f = information mean %.4f",
          cg[1, 1], mg[1, "mean"], cg[1, 1] + cg[2, 1], mg[2, "mean"]),
  sprintf("guess: median %g (control) -> %g (information); share answering exactly 13: %.1f%% (information), %.1f%% (control)",
          mg[1, "median"], mg[2, "median"], 100 * share13, 100 * share13c),
  sprintf("index: control mean %.2f, information mean %.2f; difference %+.2f (SE %.2f, t %.2f); N = %s",
          mi[1, "mean"], mi[2, "mean"], ci[2, 1], ci[2, 2], ci[2, 3], format(nrow(a), big.mark = ","))
)
writeLines(out, "code/hopkins_output.txt")
cat(out, sep = "\n")

# ---------------------------------------------------------------- figure: group means with 95% intervals
panel <- function(m, ylim, ylab, ref = NULL, reflab = NULL, main) {
  plot(NA, xlim = c(0.45, 2.55), ylim = ylim, yaxs = "i", xaxt = "n", xlab = "", ylab = ylab)
  mtext(c("Control", "Told 13"), side = 1, line = 0.6, at = 1:2, cex = 0.8, col = col_dark)
  mtext(main, side = 3, line = 0.1, adj = 0, cex = 0.9, font = 2, col = col_dark)
  if (!is.null(ref)) {
    abline(h = ref, lty = 2, col = col_grey)
    text(2.55, ref, reflab, adj = c(1, -0.4), cex = 0.78, col = col_grey)
  }
  cols <- c(col_grey, col_cardinal)
  arrows(1:2, m[, "lo"], 1:2, m[, "hi"], angle = 90, code = 3, length = 0.05, lwd = 1.6, col = cols)
  points(1:2, m[, "mean"], pch = 16, cex = 1.3, col = cols)
  text(1:2, m[, "hi"], sprintf("%.1f", m[, "mean"]), pos = 3, offset = 0.35, cex = 0.85, col = cols)
}
fig_pdf("hopkins_means", width = 3.4, height = 3.0, mar = c(2.2, 3.4, 1.4, 0.4), {
  par(mfrow = c(1, 2))
  panel(mg, c(0, 40), "Mean guess (% born abroad)", ref = 13, reflab = "Census: 13",
        main = "Guess")
  panel(mi, c(3, 13), "Mean index (3 to 13)", main = "Attitudes")
})
