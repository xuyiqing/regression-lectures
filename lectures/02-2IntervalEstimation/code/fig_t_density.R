# fig_t_density.R --- Student's t densities against N(0, 1).
#
#   figs/t_densities.pdf  densities of t with nu = 1, 4, 15 and of N(0, 1)
#                         (slide "Properties of Student's t-Distribution")
#
# Wide and short, because the slide puts five bullets under it.
# No data, no randomness. Run from the deck folder:
#   Rscript code/fig_t_density.R

source("../course_figures.R")
source("code/lib/stable_pdf.R")   # stable_pdf(): fig_pdf() that skips unchanged figures

minus <- function(x) sub("-", "\u2212", format(x, trim = TRUE))

dens <- list(
  list(lab = "N(0, 1)",   f = function(x) dnorm(x),        col = col_dark,           lwd = 2.4, lty = 1),
  list(lab = "\u03bd = 15", f = function(x) dt(x, df = 15), col = pal[["green"]],    lwd = 1.8, lty = 1),
  list(lab = "\u03bd = 4",  f = function(x) dt(x, df = 4),  col = pal[["blue"]],     lwd = 1.8, lty = 1),
  list(lab = "\u03bd = 1",  f = function(x) dt(x, df = 1),  col = pal[["cardinal"]], lwd = 1.8, lty = 1)
)

for (d in dens) cat(sprintf("%-8s density at 0 = %.3f, at 3 = %.4f\n",
                            sub("\u03bd", "nu", d$lab, fixed = TRUE), d$f(0), d$f(3)))

stable_pdf("t_densities", {
  plot(NA, xlim = c(-4, 4), ylim = c(0, 0.42), xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "", ylab = "Density")   # no x title: it crowded the first bullet
  axis(1, at = -4:4, labels = minus(-4:4))
  axis(2, at = seq(0, 0.4, 0.1), labels = c("0", ".1", ".2", ".3", ".4"))
  for (d in rev(dens)) {
    curve(d$f(x), from = -4, to = 4, n = 500, add = TRUE, col = d$col, lwd = d$lwd, lty = d$lty)
  }
  legend("topright", legend = sapply(dens, `[[`, "lab"),
         col = sapply(dens, `[[`, "col"), lwd = sapply(dens, `[[`, "lwd"),
         bty = "n", seg.len = 1.6, y.intersp = 1.05, inset = c(0, -0.02))
  text(-3.9, 0.36, "t densities with\n\u03bd degrees of freedom", adj = c(0, 1), col = col_dark)
}, width = 6, height = 2.2, mar = c(2.9, 3.4, 0.4, 0.4))
