# fig_sampling.R --- Lecture 2-1 (Point Estimation).
#
# Draws every figure built on the lecture's running example (a simulated
# population of household incomes) and writes the R transcripts shown on the
# "Simulating Sampling Distributions" slides.
#
# Run from the deck folder:   Rscript code/fig_sampling.R
#
# Writes
#   figs/income_pop.pdf, figs/income_pop_mean.pdf     the population (slides 5-6)
#   figs/samdist_n20.pdf                              four estimators, n = 20 (slide 15)
#   figs/samdist_n20_mean.pdf                         ... with averages marked (slide 20)
#   figs/samdist_n20_sd.pdf                           ... with SDs marked (slide 24)
#   figs/samdist_n20_normal.pdf                       ... with normal approximations (slide 46)
#   figs/consist_n20.pdf, _n100, _n3000               averages at n = 20, 100, 3000
#                                                     (slides 34-36: one run, same size)
#   figs/mean_concentration.pdf                       sample mean, n = 25 ... 1600
#   figs/clt_scaled_mean.pdf                          sqrt(n)(Xbar - mu), n = 1, 2, 5, 20
#   code/fig_sampling_output.txt                      transcripts + every number
#                                                     quoted on the slides
#
# The population replaces the 1996 ANES income-bracket codes (1-24) used
# through 2025. It is a simulated population of N = 10,000 household incomes
# in $1,000s whose histogram follows a right-skewed gamma distribution
# (shape 4, scale 18; mean 72, SD 36): the incomes are the N quantiles of that
# distribution, rounded to the nearest $100 and put in random order. (Using the
# quantiles instead of N random draws keeps random bumps out of the population
# histogram; rounding to $100 rather than $1,000 keeps the sample means from
# piling up on a coarse grid, which would make the histograms look combed.)
# Samples are drawn with replacement, so every draw has mean mu and variance
# sigma^2 = (1/N) sum_i (Y_i - mu)^2, as defined on the slides.

source("../course_figures.R")

out_file <- file.path("code", "fig_sampling_output.txt")
report <- character()
say <- function(...) report <<- c(report, sprintf(...))
comma <- function(x) formatC(x, format = "d", big.mark = ",")

## ---- population -----------------------------------------------------------
set.seed(450)
N <- 10000
incomes <- round(sample(qgamma(ppoints(N), shape = 4, scale = 18)), 1)
mu     <- mean(incomes)
sigma2 <- mean((incomes - mu)^2)          # population variance (divide by N)
sigma  <- sqrt(sigma2)

say("POPULATION  N = %d  mu = %.4f  sigma^2 = %.4f  sigma = %.4f  median = %g  min = %g  max = %g",
    N, mu, sigma2, sigma, median(incomes), min(incomes), max(incomes))

## ---- colors -----------------------------------------------------------------
col_bar   <- "#B9B9B9"                     # histogram bars
col_truth <- col_cardinal                  # the estimand mu
col_avg   <- pal[["blue"]]                 # average of the simulated estimates
col_norm  <- pal[["orange"]]               # normal approximation

## ---- slides 5-6: the population ----------------------------------------------
pop_breaks <- seq(-2.5, 302.5, by = 5)     # $5k bins centered on 0, 5, 10, ...
stopifnot(min(incomes) > min(pop_breaks), max(incomes) < max(pop_breaks))
xlim_inc   <- c(0, 305)                    # x-axis of every income-scale figure

# A histogram drawn as one filled outline, so no seams show between bars.
hist_fill <- function(breaks, heights, col = col_bar) {
  polygon(rep(breaks, each = 2), c(0, rep(heights, each = 2), 0), col = col, border = NA)
}
pop_hist   <- hist(incomes, breaks = pop_breaks, plot = FALSE)

draw_population <- function(mark_mean) {
  par(mar = c(3.2, 4.2, 1.6, 0.8))
  ymax <- max(pop_hist$counts) * 1.08
  plot.new()
  plot.window(xlim = xlim_inc, ylim = c(0, ymax), xaxs = "i", yaxs = "i")
  rect(head(pop_breaks, -1), 0, tail(pop_breaks, -1), pop_hist$counts,
       col = col_bar, border = "white", lwd = 0.5)
  axis(1, at = seq(0, 300, by = 50))
  axis(2)
  box(bty = "l")
  title(xlab = "Household income ($1,000s)")
  title(ylab = "Number of households", line = 3)
  mtext(sprintf("Simulated population of N = %s households", comma(N)),
        side = 3, line = 0.3, adj = 0, cex = 0.95, col = col_dark)
  if (mark_mean) {
    segments(mu, 0, mu, ymax * 0.93, col = col_truth, lty = 2, lwd = 1.4)
    points(mu, 0, pch = 17, cex = 1.9, col = col_truth, xpd = NA)
    text(mu + 4, ymax * 0.90, adj = c(0, 0.5), col = col_truth,
         bquote("population mean" ~ mu == .(sprintf("%.1f", mu)) ~ "(the balance point)"))
  }
}
fig_pdf("income_pop",      draw_population(FALSE))
fig_pdf("income_pop_mean", draw_population(TRUE))

## ---- the four candidate estimators -------------------------------------------
m <- 100000                                # samples per simulation

simulate_estimators <- function(n, seed) {
  set.seed(seed)
  draws <- vapply(seq_len(m), function(j) {
    y <- sample(incomes, size = n, replace = TRUE)
    c(y[1], sum(y))
  }, numeric(2))
  list(n = n,
       est = list(draws[1, ],               # mu1 = Y_1
                  rep(4, m),                # mu2 = 4
                  draws[2, ] / n,           # mu3 = sample mean
                  draws[2, ] / (n + 5)))    # mu4 = sum / (n + 5)
}

theory_mean <- function(n) c(mu, 4, mu, n / (n + 5) * mu)
theory_sd   <- function(n) c(sigma, 0, sigma / sqrt(n), sqrt(n) / (n + 5) * sigma)

est_labels <- list(
  quote(hat(mu)[1] == Y[1]),
  quote(hat(mu)[2] == 4),
  quote(hat(mu)[3] == bar(Y)[n]),
  quote(hat(mu)[4] == tilde(Y)[n]))

# Four stacked rows on one x-axis. Heights are rescaled within each row;
# the x-axis is identical in every figure of this family.
# compact = TRUE draws the shorter version used for the slides 34-36 run.
draw_strips <- function(sim, show = c("none", "mean", "sd", "normal"), compact = FALSE) {
  show <- match.arg(show)
  n <- sim$n
  par(mfrow = c(4, 1), oma = if (compact) c(2.1, 0, 1.3, 0) else c(2.4, 0, 1.6, 0),
      mar = if (compact) c(0.2, 4.4, 0.2, 0.6) else c(0.3, 4.6, 0.3, 0.8))
  par(cex = 1)                                    # mfrow shrinks text; undo that
  for (k in 1:4) {
    x <- sim$est[[k]]
    plot.new()
    if (k == 2) {                                   # the constant: all mass at 4
      plot.window(xlim = xlim_inc, ylim = c(0, 1.15), xaxs = "i", yaxs = "i")
      segments(4, 0, 4, 1, lwd = 2.2, col = "#8F8F8F")
      points(4, 1, pch = 16, cex = 0.9, col = "#8F8F8F")
      text(9, 0.55, "always 4", adj = c(0, 0.5), cex = 0.8, col = col_dark)
    } else {
      br <- if (k == 1) pop_breaks else
        seq(floor(min(x)) - 0.5, ceiling(max(x)) + 0.5, by = 1)
      h  <- hist(x, breaks = br, plot = FALSE)
      top <- max(h$density)
      if (show == "normal" && k >= 3) top <- max(top, dnorm(0, 0, theory_sd(n)[k]))
      plot.window(xlim = xlim_inc, ylim = c(0, top * 1.12), xaxs = "i", yaxs = "i")
      if (k == 1) {                                 # the population: separate $5k bars
        rect(head(br, -1), 0, tail(br, -1), h$density, col = col_bar, border = "white", lwd = 0.4)
      } else {                                      # estimates: $1k bins, one outline
        hist_fill(br, h$density)
      }
      if (show == "normal" && k >= 3) {
        xs <- theory_mean(n)[k] + theory_sd(n)[k] * seq(-4.5, 4.5, length.out = 400)
        lines(xs, dnorm(xs, theory_mean(n)[k], theory_sd(n)[k]), col = col_norm, lwd = 1.6)
      }
    }
    abline(h = 0, col = col_dark, lwd = 0.9)
    if (show != "none") abline(v = mu, col = col_truth, lty = 2, lwd = 1.4)
    # A spike narrower than one bin (n = 3000) would hide under the dashed
    # line: redraw it on top in dark ink.
    if (show != "none" && k != 2 && sd(x) < 1) hist_fill(br, h$density, col = col_dark)
    if (show == "mean") {
      points(mean(x), 0, pch = 17, cex = 1.4, col = col_avg, xpd = NA)
      text(298, par("usr")[4] * 0.55, sprintf("average = %.1f", mean(x)),
           adj = c(1, 0.5), cex = 0.85, col = col_avg)
    }
    if (show == "sd") {
      text(298, par("usr")[4] * 0.55, sprintf("SD = %.1f", sd(x)),
           adj = c(1, 0.5), cex = 0.85, col = col_dark)
    }
    mtext(as.expression(est_labels[[k]]), side = 2, line = 0.5, las = 1,
          adj = 1, cex = 0.95, col = col_dark)
    if (k == 4) axis(1, at = seq(0, 300, by = 50), mgp = if (compact) c(2, 0.45, 0) else c(2.1, 0.6, 0))
  }
  mtext("Estimate of mean income ($1,000s)", side = 1, outer = TRUE,
        line = if (compact) 1.05 else 1.3, cex = 0.9)
  hdr <- sprintf("%s samples of n = %s", comma(m), comma(n))
  hl <- if (compact) 0.2 else 0.35
  mtext(hdr, side = 3, outer = TRUE, line = hl, adj = 0.01, cex = 0.85, col = col_dark)
  key <- switch(show,
    none   = NULL,
    mean   = list(c("dashed line: true mean", "triangle: average of the estimates"),
                  c(col_truth, col_avg)),
    sd     = list(c("dashed line: true mean", "SD: standard deviation of the estimates"),
                  c(col_truth, col_dark)),
    normal = list(c("dashed line: true mean", "curve: normal approximation"),
                  c(col_truth, col_norm)))
  if (!is.null(key)) {
    mtext(key[[1]][1], side = 3, outer = TRUE, line = hl, adj = 0.52, cex = 0.8, col = key[[2]][1])
    mtext(key[[1]][2], side = 3, outer = TRUE, line = hl, adj = 0.99, cex = 0.8, col = key[[2]][2])
  }
}

sims <- list(simulate_estimators(20,   seed = 2020),
             simulate_estimators(100,  seed = 2100),
             simulate_estimators(3000, seed = 5000))

for (s in sims) {
  say("ESTIMATORS  n = %d  m = %d", s$n, m)
  for (k in 1:4) {
    say("  mu%d: average = %8.3f (theory %8.3f)   SD = %7.3f (theory %7.3f)",
        k, mean(s$est[[k]]), theory_mean(s$n)[k], sd(s$est[[k]]), theory_sd(s$n)[k])
  }
}

fig_pdf("samdist_n20",        draw_strips(sims[[1]], "none"))
fig_pdf("samdist_n20_mean",   draw_strips(sims[[1]], "mean"))
fig_pdf("samdist_n20_sd",     draw_strips(sims[[1]], "sd"))
fig_pdf("samdist_n20_normal", draw_strips(sims[[1]], "normal"))
# slides 34-36 are one run: identical axes and size, shorter so that slide 36
# keeps room for its summary table
for (s in sims) fig_pdf(sprintf("consist_n%d", s$n), draw_strips(s, "mean", compact = TRUE),
                        width = 5.6, height = 2.45)

## ---- slide 31: the sample mean concentrates around mu ------------------------
eps  <- 5
ns   <- c(25, 100, 400, 1600)
cols <- c(pal[["orange"]], pal[["green"]], pal[["blue"]], col_dark)
set.seed(3100)
xbars <- lapply(ns, function(n)
  vapply(seq_len(m), function(j) mean(sample(incomes, size = n, replace = TRUE)), numeric(1)))
miss <- vapply(xbars, function(x) mean(abs(x - mu) >= eps), numeric(1))
for (i in seq_along(ns)) say("CONCENTRATION  n = %4d  SD = %.3f  share |xbar - mu| >= %g : %.4f",
                             ns[i], sd(xbars[[i]]), eps, miss[i])

fig_pdf("mean_concentration", {
  par(mar = c(3.2, 3.6, 0.6, 0.8))
  dens <- lapply(xbars, density, adjust = 1.2, n = 1024)
  xl <- c(40, 104)
  plot.new()
  plot.window(xlim = xl, ylim = c(0, max(sapply(dens, function(d) max(d$y))) * 1.05),
              xaxs = "i", yaxs = "i")
  rect(mu - eps, 0, mu + eps, par("usr")[4], col = alpha_col(col_truth, 0.10), border = NA)
  for (i in seq_along(ns)) lines(dens[[i]], col = cols[i], lwd = 1.8)
  abline(v = mu, col = col_truth, lty = 2, lwd = 1.4)
  axis(1); axis(2); box(bty = "l")
  title(xlab = expression("Sample mean " * bar(X)[n] * " ($1,000s)"), ylab = "Density")
  text(mu - eps - 0.6, par("usr")[4] * 0.97, adj = c(1, 1), cex = 0.85, col = col_truth,
       bquote("shaded band:" ~ mu %+-% epsilon * "," ~ epsilon == .(eps)))
  # a share below 0.01 shows as "< 0.01", never as 0.00 or 0.01 (Yiqing, 2026-10-11; as the 2-1 video)
  labs <- lapply(seq_along(ns), function(i)
    if (miss[i] < 0.01)
      bquote(n == .(comma(ns[i])) * ":" ~~ Pr(group("|", bar(X)[n] - mu, "|") >= epsilon) < 0.01)
    else
      bquote(n == .(comma(ns[i])) * ":" ~~
               Pr(group("|", bar(X)[n] - mu, "|") >= epsilon) %~~% .(sprintf("%.2f", miss[i]))))
  legend("topright", legend = as.expression(labs), col = cols, lwd = 1.8,
         bty = "n", cex = 0.85, inset = c(0.0, 0.01), y.intersp = 1.25)
})

## ---- slide 44: central limit theorem -----------------------------------------
clt_n <- c(1, 2, 5, 20)
set.seed(4400)
scaled <- lapply(clt_n, function(n)
  vapply(seq_len(m), function(j) sqrt(n) * (mean(sample(incomes, size = n, replace = TRUE)) - mu),
         numeric(1)))
rng <- range(unlist(scaled))
clt_breaks <- seq(5 * floor((rng[1] + 2.5) / 5) - 2.5,     # $5k bins centered on
                  5 * ceiling((rng[2] - 2.5) / 5) + 2.5,   # multiples of 5, spanning
                  by = 5)                                  # every simulated value
clt_h <- lapply(scaled, hist, breaks = clt_breaks, plot = FALSE)
for (i in seq_along(clt_n)) say("CLT  n = %2d  mean = %7.3f  SD = %.3f  (limit: mean 0, SD %.3f)",
                                clt_n[i], mean(scaled[[i]]), sd(scaled[[i]]), sigma)

fig_pdf("clt_scaled_mean", {
  par(mfrow = c(2, 2), oma = c(2.0, 2.2, 1.6, 0), mar = c(1.6, 1.8, 1.2, 0.8))
  par(cex = 1)                                      # mfrow shrinks text; undo that
  ytop <- max(c(sapply(clt_h, function(h) max(h$density)), dnorm(0, 0, sigma))) * 1.08
  xs <- seq(-120, 240, length.out = 1000)
  for (i in seq_along(clt_n)) {
    plot.new()
    plot.window(xlim = c(-120, 240), ylim = c(0, ytop), xaxs = "i", yaxs = "i")
    h <- clt_h[[i]]
    hist_fill(clt_breaks, h$density)
    lines(xs, dnorm(xs, 0, sigma), col = col_truth, lwd = 1.6)
    at <- seq(-100, 200, by = 100)
    axis(1, at = at, labels = sub("^-", "\u2212", at))   # true minus sign, not a hyphen
    axis(2, at = pretty(c(0, ytop), 3), labels = if (i %% 2 == 1) TRUE else FALSE)
    box(bty = "l")
    mtext(bquote(n == .(clt_n[i])), side = 3, line = 0.05, adj = 0.03, cex = 0.95)
  }
  mtext(expression(sqrt(n) * (bar(X)[n] - mu)), side = 1, outer = TRUE, line = 0.9, cex = 0.95)
  mtext("Density", side = 2, outer = TRUE, line = 1.0, las = 0, cex = 0.9)
  mtext(bquote("bars:" ~ .(comma(m)) ~ "simulated samples from the income population;" ~~
                 "curve: the limit" ~ N(0, sigma^2) * "," ~ sigma == .(sprintf("%.1f", sigma))),
        side = 3, outer = TRUE, line = 0.3, adj = 0.01, cex = 0.85, col = col_dark)
})

## ---- slides 12-14: R transcripts ---------------------------------------------
# Each chunk is run in its own environment, as one continuous R session, and
# echoed the way the console shows it. The output is pasted into the slides.
echo_run <- function(code, env) {
  exprs <- parse(text = code, keep.source = TRUE)
  src   <- strsplit(code, "\n", fixed = TRUE)[[1]]
  refs  <- attr(exprs, "srcref")
  out   <- character()
  last  <- 0L
  for (i in seq_along(exprs)) {
    first <- refs[[i]][1]; final <- refs[[i]][3]
    if (first > last + 1L) {
      between <- src[(last + 1L):(first - 1L)]
      out <- c(out, paste0("> ", between[nzchar(trimws(between))]))
    }
    if (final > last) {                  # a line holding several expressions is echoed once
      body <- src[max(first, last + 1L):final]
      out  <- c(out, paste0(c("> ", rep("+ ", length(body) - 1L)), body))
    }
    res  <- withVisible(eval(exprs[[i]], envir = env))
    if (res$visible) out <- c(out, capture.output(print(res$value)))
    last <- max(last, final)
  }
  out
}

session <- new.env()
session$incomes <- incomes
grDevices::pdf(NULL)                     # hist() below draws to a null device
old_width <- options(width = 72)

chunk12 <- "# population: simulated incomes of 10,000 households ($1,000s)
length(incomes)
incomes[1:30]
set.seed(2026)
# take one random sample
income.sample <- sample(x=incomes, size=20, replace=TRUE)
income.sample
# estimator for sample mean
mean.estimator <- function(x){
  out <- sum(x)/length(x)
  return(out)
}
# apply estimator to sample
mean.estimator(income.sample)"

chunk13 <- "# set parameters
m <- 100000; n <- 20
# set up storage matrix for means
store.means <- matrix(NA, m, 1)
store.means[1:6, 1]
# simulation starts here
for(i in 1:m){
  income.sample <- sample(x=incomes, size=n, replace=TRUE)
  store.means[i, 1] <- mean.estimator(income.sample)
}
store.means[1:6]
hist(store.means)"

chunk14 <- "# take m samples of size n
income.samples <- replicate(m,
                            sample(x=incomes, size=n, replace=TRUE)
                            )
dim(income.samples)
income.samples[1:3, 1:4]
# compute estimator for each sample
store.means <- apply(income.samples, 2, mean.estimator)
store.means[1:6]"

tr <- list(slide12 = echo_run(chunk12, session),
           slide13 = echo_run(chunk13, session),
           slide14 = echo_run(chunk14, session))
options(old_width)
invisible(grDevices::dev.off())

for (nm in names(tr)) report <- c(report, "", paste0("TRANSCRIPT ", nm), tr[[nm]], paste0("END ", nm))
writeLines(report, out_file)
cat(report, sep = "\n")
