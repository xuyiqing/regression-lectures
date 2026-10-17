# fig_t_vs_f.R --- why a joint F test and two t tests can disagree.
# Lecture 6.2, slide "Some More Notes on F Tests" (second of the two).
# Replaces a scanned textbook figure (t- versus F-tests) with a drawing whose
# geometry is computed: the 95% joint confidence region for (beta1, beta2) is
# an ellipse; the two individual 95% confidence intervals form a box. A null
# value (b1, b2) is rejected by the F test when it lies outside the ellipse and
# by a t test when it lies outside the box.
#   figs/t_vs_f_region.pdf
#
# The estimates are hypothetical: standard errors 1 and 1, correlation 0.6.
# Large-sample critical values: sqrt(chi2_2(0.95)) for the ellipse, z(0.975)
# for the intervals; with finite-sample t and F values the picture is the same.
#
# Run from the deck folder:  Rscript code/fig_t_vs_f.R

source("../course_figures.R")

b_hat <- c(5, 4)
se    <- c(1, 1)
rho   <- 0.6
V     <- diag(se) %*% matrix(c(1, rho, rho, 1), 2) %*% diag(se)

c_joint <- sqrt(qchisq(0.95, df = 2))       # radius of the joint region
c_ind   <- qnorm(0.975)                     # half-width of each interval, in se units

ang <- seq(0, 2 * pi, length.out = 721)[-721]
ell <- t(b_hat + t(chol(V)) %*% rbind(cos(ang), sin(ang)) * c_joint)
box <- c(b_hat[1] - c_ind * se[1], b_hat[1] + c_ind * se[1],
         b_hat[2] - c_ind * se[2], b_hat[2] + c_ind * se[2])   # xmin xmax ymin ymax

# Sutherland-Hodgman: clip a polygon to an axis-parallel box.
clip_to_box <- function(P, xmin, xmax, ymin, ymax) {
  cut_at <- function(k, v) function(p, q) p + (v - p[k]) / (q[k] - p[k]) * (q - p)
  one_edge <- function(P, keep, cross) {
    out <- NULL
    for (i in seq_len(nrow(P))) {
      cur  <- P[i, ]
      prev <- P[if (i == 1) nrow(P) else i - 1, ]
      if (keep(cur)) {
        if (!keep(prev)) out <- rbind(out, cross(prev, cur))
        out <- rbind(out, cur)
      } else if (keep(prev)) {
        out <- rbind(out, cross(prev, cur))
      }
    }
    out
  }
  P <- one_edge(P, function(p) p[1] >= xmin, cut_at(1, xmin))
  P <- one_edge(P, function(p) p[1] <= xmax, cut_at(1, xmax))
  P <- one_edge(P, function(p) p[2] >= ymin, cut_at(2, ymin))
  P <- one_edge(P, function(p) p[2] <= ymax, cut_at(2, ymax))
  P
}
both_keep <- clip_to_box(ell, box[1], box[2], box[3], box[4])

# Which box corners fall outside the ellipse (these are the "F rejects only" areas)?
corners <- expand.grid(x = box[1:2], y = box[3:4])
qf_form <- apply(corners, 1, function(p) {
  d <- p - b_hat
  drop(t(d) %*% solve(V) %*% d)
})
cat("Box corners and whether the F test rejects there (quadratic form >", round(c_joint^2, 2), "):\n")
print(cbind(corners, quad_form = round(qf_form, 2), F_rejects = qf_form > c_joint^2))

col_f_only <- alpha_col(pal["blue"], 0.35)     # t tests keep, F test rejects
col_t_only <- alpha_col(pal["orange"], 0.45)   # F test keeps, a t test rejects

xl <- c(0, 9.6)
yl <- c(0, 8.0)

fig_pdf("t_vs_f_region", width = 5.4, height = 2.5, mar = c(1.2, 1.4, 0.4, 0.2), {
  layout(matrix(1:2, 1), widths = c(1, 1.05))
  plot(NA, xlim = xl, ylim = yl, xaxs = "i", yaxs = "i", axes = FALSE,
       xlab = "", ylab = "", asp = 1)
  par(xpd = NA)
  # shaded disagreement areas: ellipse, then box, then their overlap in white
  polygon(ell, col = col_t_only, border = NA)
  rect(box[1], box[3], box[2], box[4], col = col_f_only, border = NA)
  polygon(both_keep, col = "white", border = NA)
  # the box's edges, extended to the axes, and the two intervals on the axes
  segments(box[1:2], 0, box[1:2], box[4], lty = 2, col = col_grey)
  segments(0, box[3:4], box[2], box[3:4], lty = 2, col = col_grey)
  segments(box[1], 0, box[2], 0, lwd = 4, col = col_dark, lend = 1)
  segments(0, box[3], 0, box[4], lwd = 4, col = col_dark, lend = 1)
  # the ellipse and the estimate
  polygon(ell, border = col_cardinal, lwd = 2)
  points(b_hat[1], b_hat[2], pch = 16, cex = 0.9, col = col_dark)
  text(b_hat[1], b_hat[2], expression((list(hat(beta)[1], hat(beta)[2]))),
       pos = 3, offset = 0.3, cex = 0.8)     # above the point, clear of the ellipse
  # axes
  arrows(0, 0, xl[2], 0, length = 0.07, lwd = 1, col = col_dark)
  arrows(0, 0, 0, yl[2], length = 0.07, lwd = 1, col = col_dark)
  text(xl[2], 0, expression(beta[1]), pos = 1, offset = 0.35)
  text(0, yl[2], expression(beta[2]), pos = 2, offset = 0.35)
  text(0, 0, "0", pos = 1, offset = 0.35)
  text(b_hat[1], 0, expression(hat(beta)[1]), pos = 1, offset = 0.35, cex = 0.85)
  text(0, b_hat[2], expression(hat(beta)[2]), pos = 2, offset = 0.35, cex = 0.85)

  # key
  par(mar = c(0.4, 0.4, 0.4, 0.2))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  y <- c(0.93, 0.71, 0.45, 0.19)
  segments(0.02, y[1], 0.14, y[1], col = col_cardinal, lwd = 2)
  segments(0.02, y[2], 0.14, y[2], col = col_grey, lty = 2)
  rect(0.03, y[3] - 0.055, 0.13, y[3] + 0.055, col = col_f_only, border = NA)
  rect(0.03, y[4] - 0.055, 0.13, y[4] + 0.055, col = col_t_only, border = NA)
  # two lines per entry, set separately so t and F can be italic
  line1 <- expression("95% joint confidence region",
                      "95% confidence intervals for",
                      "Null value here: neither " * italic(t) * " test",
                      "Null value here: the " * italic(F) * " test does")
  line2 <- expression("(" * italic(F) * " test of both coefficients)",
                      "each coefficient (" * italic(t) * " tests)",
                      "rejects, but the " * italic(F) * " test rejects",
                      "not reject, but a " * italic(t) * " test rejects")
  for (i in 1:4) {
    text(0.18, y[i] + 0.045, line1[i], adj = c(0, 0.5), cex = 0.8)
    text(0.18, y[i] - 0.045, line2[i], adj = c(0, 0.5), cex = 0.8)
  }
})
