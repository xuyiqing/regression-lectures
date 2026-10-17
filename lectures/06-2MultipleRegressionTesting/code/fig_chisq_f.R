# fig_chisq_f.R --- chi-square and F densities.
# Lecture 6.2, slides "The chi^2 Distribution" and "The F distribution".
# Draws  figs/chisq_densities.pdf, figs/f_densities.pdf
# and prints the tail probabilities quoted on the slide
# "Sampling Distribution of the Wald Statistic".
#
# Run from the deck folder:  Rscript code/fig_chisq_f.R

source("../course_figures.R")

cols <- unname(pal[c("blue", "cardinal", "green", "orange")])

# ---- chi-square densities (same degrees of freedom as the 2007 figure) -------
chisq_df <- c(1, 4, 15)
fig_pdf("chisq_densities", width = 4.6, height = 2.1,
        mar = c(3.0, 3.6, 0.9, 0.6), {
  plot(NA, xlim = c(0, 40), ylim = c(0, 0.5), xaxs = "i", yaxs = "i",
       xlab = expression(x), ylab = "Density")
  for (i in seq_along(chisq_df)) {
    curve(dchisq(x, chisq_df[i]), from = 0.001, to = 40, n = 801, add = TRUE,
          lwd = 2, col = cols[i])
  }
  legend("topright", bty = "n", lwd = 2, col = cols[seq_along(chisq_df)],
         legend = c(expression(chi[1]^2), expression(chi[4]^2),
                    expression(chi[15]^2)))
})

# ---- F densities (same degrees of freedom as the 2011 figure) ----------------
f_df <- list(c(1, 2), c(5, 5), c(30, 20), c(500, 200))
fig_pdf("f_densities", width = 4.2, height = 1.85,
        mar = c(3.0, 3.6, 0.9, 0.6), {
  plot(NA, xlim = c(0, 2), ylim = c(0, 3.6), xaxs = "i", yaxs = "i",
       xlab = expression(x), ylab = "Density")
  for (i in seq_along(f_df)) {
    curve(df(x, f_df[[i]][1], f_df[[i]][2]), from = 0.001, to = 2, n = 801,
          add = TRUE, lwd = 2, col = cols[i])
  }
  legend("topright", bty = "n", lwd = 2, col = cols, cex = 0.9,
         legend = c(expression(italic(F)[list(1, 2)]),
                    expression(italic(F)[list(5, 5)]),
                    expression(italic(F)[list(30, 20)]),
                    expression(italic(F)[list(500, 200)])))
})

# ---- numbers on the Wald-statistic slide --------------------------------------
cat("pf(3.1, 2, 500, lower.tail = FALSE)   =", format(pf(3.1, 2, 500, lower.tail = FALSE), digits = 7), "\n")
cat("pchisq(2 * 3.1, 2, lower.tail = FALSE) =", format(pchisq(2 * 3.1, 2, lower.tail = FALSE), digits = 7), "\n")
cat("pf(3.1, 2, 50000, lower.tail = FALSE) =", format(pf(3.1, 2, 50000, lower.tail = FALSE), digits = 7), "\n")
