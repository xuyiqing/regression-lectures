# fig_lln.R --- Lecture 2-1 (Point Estimation), slide "Example: Mean of n
# independent tosses of a coin" (weak law of large numbers).
#
# Run from the deck folder:   Rscript code/fig_lln.R
# Writes figs/lln_coin.pdf.
#
# Redraws the 2015 figure (LLN.pdf, from code/legacy/LLN.R, which had no seed):
# the running share of heads in n tosses of a fair coin settles at 0.5. One
# sequence of tosses is highlighted; a few more sequences are drawn in grey to
# show that the same happens in every sequence, not just in one lucky run.

source("../course_figures.R")

set.seed(450)
n_max  <- 500
n_seq  <- 6                                   # sequence 1 is highlighted
eps    <- 0.05
tosses <- matrix(rbinom(n_max * n_seq, size = 1, prob = 0.5), nrow = n_max)
running <- apply(tosses, 2, function(x) cumsum(x) / seq_along(x))

cat(sprintf("sequence %d: share of heads after %d tosses = %.3f\n",
            seq_len(n_seq), n_max, running[n_max, ]), sep = "")

fig_pdf("lln_coin", {
  par(mar = c(3.2, 3.6, 0.6, 0.8))
  plot.new()
  plot.window(xlim = c(1, n_max), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  rect(1, 0.5 - eps, n_max, 0.5 + eps, col = alpha_col(col_cardinal, 0.10), border = NA)
  for (j in 2:n_seq) lines(seq_len(n_max), running[, j], col = alpha_col(col_grey, 0.55), lwd = 1)
  abline(h = 0.5, col = col_cardinal, lty = 2, lwd = 1.5)
  lines(seq_len(n_max), running[, 1], col = col_dark, lwd = 1.6)
  points(seq_len(n_max), running[, 1], pch = 16, cex = 0.28, col = col_dark)
  axis(1, at = c(1, seq(100, n_max, by = 100)))
  axis(2, at = seq(0, 1, by = 0.25))
  box(bty = "l")
  title(xlab = "Number of tosses n",
        ylab = expression("Share of heads so far, " * bar(X)[n]))
  legend("topright", bty = "n", cex = 0.85, inset = c(0.01, 0.02),
         legend = c("one sequence of tosses", "five more sequences",
                    expression("true mean " * mu == 0.5),
                    expression("shaded band: " * mu %+-% 0.05)),
         col = c(col_dark, col_grey, col_cardinal, NA),
         lwd = c(1.6, 1, 1.5, NA), lty = c(1, 1, 2, NA),
         fill = c(NA, NA, NA, alpha_col(col_cardinal, 0.10)),
         border = NA, x.intersp = 0.8)
})
