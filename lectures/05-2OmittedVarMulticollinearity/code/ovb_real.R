# ovb_real.R --- Lecture 5-2 v2, omitted variable bias on real data.
#
# Two cases for the frames after the OVB identity  b1_short = b1_long + b2_long * delta:
#
# (A) The Lecture 5-1 running example (same file, same sample, same numbers as 5-1 v2).
#     Data: code/data/countries_xsec.csv, a copy of lectures/_data/countries/countries_xsec.csv
#     (V-Dem v16 + WDI, 2024; built in lectures/_data/countries/). Sample: complete on execorr100, courtind,
#     democracy and log_gdppc (N = 168), the 5-1 sample (Yiqing, 2026-10-11).
#       execorr100 = 100 x V-Dem v2x_execorr, executive corruption (0 = clean, 100 = most corrupt)
#       courtind   = V-Dem v2juhcind_osp, high court independence (0 = never independent, 4 = always)
#       short: execorr100 ~ courtind          long: execorr100 ~ courtind + democracy
#       aux:   democracy ~ courtind           (democracy = 1 if V-Dem polyarchy > 0.5)
#
# (B) Schaffner, MacWilliams & Nteta (2018, PSQ), Table 4 (CCES 2016): the college gap among white
#     voters in the two-party Trump vote. Data: code/data/schaffner2018_cces2016.csv, built in
#     lectures/_data/papers/schaffner2018/ from the authors' Dataverse file (doi:10.7910/DVN/KPZQAC, CC0).
#     White non-Hispanic respondents (the do-file's race == 1 after its Hispanic recode; fixed 2026-10-11,
#     final audit) with a Trump (1) or Clinton (0) vote; weighted OLS with the survey weight, as in
#     the paper. racism_scaled = denial of racism, sexism_scaled = hostile sexism (IRT scores, 0-1).
#
# Writes every number the slides print to code/ovb_real_output.txt. No figures.
# Run from the deck folder (v2/):  Rscript code/ovb_real.R

out <- "code/ovb_real_output.txt"
sink(out)

# ---------------------------------------------------------------------------- (A)
d <- read.csv("code/data/countries_xsec.csv")
d <- d[complete.cases(d[, c("execorr100", "courtind", "democracy", "log_gdppc")]), ]
stopifnot(nrow(d) == 168)

short <- lm(execorr100 ~ courtind, data = d)
long  <- lm(execorr100 ~ courtind + democracy, data = d)
aux   <- lm(democracy ~ courtind, data = d)
b_s <- unname(coef(short)["courtind"]); b_l <- unname(coef(long)["courtind"])
b2  <- unname(coef(long)["democracy"]); dl <- unname(coef(aux)["courtind"])
# same fits as lectures/_data/countries/gates_court_output.txt
stopifnot(abs(b_s - (-17.663679)) < 5e-7, abs(b_l - (-10.630106)) < 5e-7, abs(b2 - (-21.510121)) < 5e-7,
          abs(dl - 0.326989) < 5e-7)

cat("ovb_real.R --- Lecture 5-2 v2, OVB on real data\n\n")
cat("== (A) Executive corruption on high court independence (V-Dem v16, 2024), N =", nrow(d), "(the Lecture 5-1 sample) ==\n")
cat(sprintf("short  execorr100 ~ courtind              : b(courtind)  = %.10f\n", b_s))
cat(sprintf("long   execorr100 ~ courtind + democracy  : b(courtind)  = %.10f\n", b_l))
cat(sprintf("                                            b(democracy) = %.10f\n", b2))
cat(sprintf("aux    democracy ~ courtind               : delta        = %.10f\n", dl))
cat(sprintf("bias = b(democracy) * delta = %.10f\n", b2 * dl))
cat(sprintf("long + bias = %.10f ; short - (long + bias) = %.3e  %s\n", b_l + b2 * dl,
            b_s - (b_l + b2 * dl), ifelse(abs(b_s - (b_l + b2 * dl)) < 1e-10, "IDENTITY HOLDS (< 1e-10)", "FAIL")))
cat(sprintf("share of the short slope that is bias: %.1f%%\n", 100 * b2 * dl / b_s))
cat("\nAs typed on the slide:\n")
cat(sprintf("  short %.4f | long %.4f | b(democracy) %.4f | delta %.6f\n", b_s, b_l, b2, dl))
cat(sprintf("  %.4f + (%.4f)(%.6f) = %.4f + (%.4f) = %.4f\n", b_l, b2, dl, b_l, round(b2, 4) * round(dl, 6), b_l + round(b2, 4) * round(dl, 6)))
cat(sprintf("  delta = %.3f: one more point of court independence goes with %.1f percentage points more democracies\n", dl, 100 * dl))
cat("\nGroup means (the sign of delta):\n")
print(round(aggregate(cbind(courtind, execorr100) ~ democracy, data = d, FUN = mean), 2), row.names = FALSE)
cat("countries: democracies", sum(d$democracy == 1), "| autocracies", sum(d$democracy == 0), "\n")

# ---------------------------------------------------------------------------- (B)
s0 <- read.csv("code/data/schaffner2018_cces2016.csv")
w <- s0[s0$white == 1 & !is.na(s0$prezvote) & !is.na(s0$college), ]
w <- w[complete.cases(w[, c("racism_scaled", "sexism_scaled", "weight")]), ]
stopifnot(nrow(w) == 1648)   # Table 4's N

cat("\n\n== (B) Schaffner, MacWilliams & Nteta (2018), CCES 2016, white Trump/Clinton voters, weighted OLS ==\n")
cat("N =", nrow(w), " (college graduates:", sum(w$college == 1), "| no degree:", sum(w$college == 0), ")\n")
specs <- list("college alone"              = prezvote ~ college,
              "+ hostile sexism"            = prezvote ~ college + sexism_scaled,
              "+ racism denial"             = prezvote ~ college + racism_scaled,
              "+ racism denial + sexism"    = prezvote ~ college + racism_scaled + sexism_scaled)
paper <- c(-0.178, -0.090, -0.044, -0.031)   # Table 4, CCES panel (N = 1,648)
fits <- lapply(specs, function(f) lm(f, data = w, weights = w$weight))
cat(sprintf("%-28s %10s %8s %10s\n", "model", "b(college)", "N", "paper"))
for (i in seq_along(fits))
  cat(sprintf("%-28s %10.4f %8d %10.3f   diff %+.4f\n", names(specs)[i], coef(fits[[i]])["college"],
              nobs(fits[[i]]), paper[i], coef(fits[[i]])["college"] - paper[i]))
cat(sprintf("max |ours - paper| = %.4f\n", max(abs(sapply(fits, function(f) coef(f)["college"]) - paper))))
cat("slide table (3 decimals, rounded once from full precision):",
    sprintf("%.3f", sapply(fits, function(f) unname(coef(f)["college"]))), "\n")

shortB <- fits[[1]]; longB <- fits[[3]]
auxB <- lm(racism_scaled ~ college, data = w, weights = w$weight)
bs <- unname(coef(shortB)["college"]); bl <- unname(coef(longB)["college"])
g  <- unname(coef(longB)["racism_scaled"]); dB <- unname(coef(auxB)["college"])
cat("\nOVB identity, omitted variable = racism denial (same N, weighted):\n")
cat(sprintf("short b(college) = %.6f | long b(college) = %.6f | long b(racism) = %.6f | delta = %.6f\n", bs, bl, g, dB))
cat(sprintf("bias = %.6f ; long + bias = %.6f ; short - (long + bias) = %.3e  %s\n", g * dB, bl + g * dB,
            bs - (bl + g * dB), ifelse(abs(bs - (bl + g * dB)) < 1e-10, "IDENTITY HOLDS (< 1e-10)", "FAIL")))
cat(sprintf("share of the college gap accounted for by racism denial: %.1f%%\n", 100 * g * dB / bs))
cat(sprintf("both attitudes: gap %.4f -> %.4f, i.e. %.1f%% of the gap remains\n",
            bs, coef(fits[[4]])["college"], 100 * coef(fits[[4]])["college"] / bs))
cat("\nAs typed on the slide:\n")
cat(sprintf("  %.4f = %.4f + (%.3f)(%.4f) = %.4f + (%.4f)\n", bs, bl, g, dB, bl, g * dB))
cat("\nWeighted means by college (the sign of delta):\n")
mm <- do.call(rbind, lapply(split(w, w$college), function(x)
  data.frame(college = x$college[1], n = nrow(x), racism_denial = weighted.mean(x$racism_scaled, x$weight),
             trump = weighted.mean(x$prezvote, x$weight))))
print(round(mm, 3), row.names = FALSE)
sink()
cat(readLines(out), sep = "\n")
