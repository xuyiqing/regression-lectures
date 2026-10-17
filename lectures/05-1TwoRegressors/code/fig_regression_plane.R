# fig_regression_plane.R --- Lecture 5-1: the data in three dimensions and the
# fitted regression plane (base R persp()). Y = executive corruption, X1 = high court
# independence (0-4), X2 = democracy or log10 GDP per capita.
#
# Data: code/data/countries_xsec.csv, 2024, N = 168 (see fig_parallel_lines.R).
#
# Figures (projection-deck frames):
#   dem_3d_court    corruption ~ court independence + democracy, seen from the court side
#   dem_3d_side     same fit, seen from the democracy side
#   dem_3d_plane    + the plane that joins the two lines
#   gdp_3d_points   corruption, court independence, log10 GDP per capita
#   gdp_3d_plane    + fitted plane
#   gdp_3d_rotated  plane seen with the court axis running left to right, so its
#                   downward tilt in the court direction shows
#   gdp_3d_resid    + residuals as vertical segments to the plane
# Numbers printed on the slides (plane coefficients, the Chile and China
# predictions for Worksheet Q2, the floor near 0): code/regression_plane_output.txt
#
# Points below the plane are drawn before the (transparent) plane and points
# above it after, so the plane hides what lies behind it.
#
# Run from the deck folder (v2/):  Rscript code/fig_regression_plane.R

source("../course_figures.R")

d <- read.csv("code/data/countries_xsec.csv")
d <- d[complete.cases(d[, c("execorr100", "courtind", "democracy", "gdppc")]), ]
d$lgdp <- log10(d$gdppc)
stopifnot(nrow(d) == 168)
fit_dem <- lm(execorr100 ~ courtind + democracy, data = d)
fit_gdp <- lm(execorr100 ~ courtind + lgdp, data = d)
bd <- coef(fit_dem); bg <- coef(fit_gdp)
stopifnot(abs(bd - c(78.3236, -10.6301, -21.5101)) < 5e-5)

out <- "code/regression_plane_output.txt"
sink(out)
cat("Lecture 5-1 v2: regression plane. V-Dem v16 + WDI, 2024, N =", nrow(d), "\n\n")
cat("-- execorr100 ~ courtind + log10 GDP pc\n"); print(round(summary(fit_gdp)$coef, 4))
cat(sprintf("R2 = %.4f\n", summary(fit_gdp)$r.squared))
cat(sprintf("rounded: yhat = %.1f + (%.2f) courtind + (%.1f) log10 GDP\n", bg[1], bg[2], bg[3]))
cat(sprintf("range courtind %.3f-%.3f | range log10 GDP %.2f-%.2f ($%s-$%s)\n",
            min(d$courtind), max(d$courtind), min(d$lgdp), max(d$lgdp),
            format(round(min(d$gdppc)), big.mark = ","), format(round(max(d$gdppc)), big.mark = ",")))
cat(sprintf("cor(courtind, log10 GDP) = %.3f\n", cor(d$courtind, d$lgdp)))

cat("\nWorksheet Q2: Chile and China (inputs rounded as on the slide)\n")
cc <- d[d$country %in% c("Chile", "China"), c("country", "execorr100", "courtind", "democracy", "gdppc", "lgdp")]
cc$fitted_exact <- predict(fit_gdp, newdata = cc)
print(cc, row.names = FALSE, digits = 6)
r <- unname(c(round(bg[1], 1), round(bg[2], 2), round(bg[3], 1)))
x <- list(Chile = c(2.96, 4.48), China = c(0.21, 4.38))
yh <- sapply(x, function(v) r[1] + r[2] * v[1] + r[3] * v[2])
cat(sprintf("rounded coefficients (%.1f, %.2f, %.1f)\n", r[1], r[2], r[3]))
cat(sprintf("Chile: %.1f + (%.2f)(2.96) + (%.1f)(4.48) = %.4f\n", r[1], r[2], r[3], yh["Chile"]))
cat(sprintf("China: %.1f + (%.2f)(0.21) + (%.1f)(4.38) = %.4f\n", r[1], r[2], r[3], yh["China"]))
cat(sprintf("difference Chile - China = %.4f = (%.2f)(%.2f) + (%.1f)(%.2f) = %.4f + %.4f\n",
            yh["Chile"] - yh["China"], r[2], 2.96 - 0.21, r[3], 4.48 - 4.38,
            r[2] * (2.96 - 0.21), r[3] * (4.48 - 4.38)))

cat("\nFloor near 0: the ten least corrupt countries, with plane fit and residual\n")
d$fit <- fitted(fit_gdp); d$res <- residuals(fit_gdp)
lo <- d[order(d$execorr100), ][1:10, c("country", "execorr100", "courtind", "democracy", "gdppc", "fit", "res")]
print(lo, row.names = FALSE, digits = 3)
cat(sprintf("plane: fitted values below 0: %d | min fitted %.2f\n", sum(d$fit < 0), min(d$fit)))
top <- d$democracy == 1 & d$gdppc >= quantile(d$gdppc, 0.75)
cat(sprintf("democracies in the top GDP quartile: N = %d, median execorr100 %.1f, share below 5: %.2f, mean residual %.2f\n",
            sum(top), median(d$execorr100[top]), mean(d$execorr100[top] < 5), mean(d$res[top])))
sink()

col_dem <- unname(pal["blue"])
col_aut <- col_cardinal
w <- 6 / 2.54; h <- 5.4 / 2.54          # 6 cm column on the slides
mar3 <- c(0.3, 0.9, 0.1, 0.3)

box3d <- function(xr, yr, zr, theta, phi, xlab, ylab, zlab, nt = 4) {
  par(mgp = c(1.5, 0.4, 0))
  persp(x = xr, y = yr, z = matrix(zr[1], 2, 2), xlim = xr, ylim = yr, zlim = zr,
        theta = theta, phi = phi, r = 4, d = 1.2, expand = 0.8,
        col = NA, border = NA, box = TRUE, axes = TRUE, ticktype = "detailed",
        nticks = nt, xlab = xlab, ylab = ylab, zlab = zlab,
        cex.axis = 0.7, cex.lab = 0.85, lwd = 0.6, col.axis = col_dark)
}
p3 <- function(x, y, z, pm, ...) points(trans3d(x, y, z, pm), ...)
l3 <- function(x, y, z, pm, ...) lines(trans3d(x, y, z, pm), ...)
plane3d <- function(f, xr, yr, pm, n = 5, fill = alpha_col(col_grey, 0.22),
                    grid = alpha_col(col_grey, 0.8)) {
  cx <- c(xr[1], xr[2], xr[2], xr[1]); cy <- c(yr[1], yr[1], yr[2], yr[2])
  polygon(trans3d(cx, cy, f(cx, cy), pm), col = fill, border = grid, lwd = 0.6)
  for (x in seq(xr[1], xr[2], length.out = n))
    l3(c(x, x), yr, f(c(x, x), yr), pm, col = grid, lwd = 0.5)
  for (y in seq(yr[1], yr[2], length.out = n))
    l3(xr, c(y, y), f(xr, c(y, y)), pm, col = grid, lwd = 0.5)
}

crt_r <- c(0, 4); cor_r <- c(0, 100)

# ---- democracy model ----------------------------------------------------------
yhat_dem <- function(court, dem) bd[1] + bd[2] * court + bd[3] * dem
grp_col  <- ifelse(d$democracy == 1, col_dem, col_aut)
crt_line <- range(d$courtind)

fig_pdf("dem_3d_court", width = w, height = h, pointsize = 9, mar = mar3, {
  pm <- box3d(crt_r, c(0, 1), cor_r, theta = -25, phi = 10,   # low view: the back (democracy) line is not lifted above the front one
              "Court indep.", "Democracy", "Corruption")
  p3(d$courtind, d$democracy, d$execorr100, pm, pch = 16, cex = 0.55,
     col = alpha_col(grp_col, 0.75))
  l3(crt_line, c(0, 0), yhat_dem(crt_line, 0), pm, col = col_aut, lwd = 1.8)
  l3(crt_line, c(1, 1), yhat_dem(crt_line, 1), pm, col = col_dem, lwd = 1.8)
})

dem_side <- function(with_plane) {
  f <- function(dem, court) yhat_dem(court, dem)       # x = democracy, y = court independence
  pm <- box3d(c(0, 1), crt_r, cor_r, theta = -35, phi = 30,   # from above, so the plane is not seen edge-on
              "Democracy", "Court indep.", "Corruption", nt = 3)
  above <- d$execorr100 >= f(d$democracy, d$courtind)
  if (with_plane)
    p3(d$democracy[!above], d$courtind[!above], d$execorr100[!above], pm, pch = 16,
       cex = 0.55, col = alpha_col(grp_col[!above], 0.75))
  if (with_plane) plane3d(f, c(0, 1), crt_r, pm)
  sel <- if (with_plane) above else rep(TRUE, nrow(d))
  p3(d$democracy[sel], d$courtind[sel], d$execorr100[sel], pm, pch = 16, cex = 0.55,
     col = alpha_col(grp_col[sel], 0.75))
  l3(c(0, 0), crt_line, f(c(0, 0), crt_line), pm, col = col_aut, lwd = 1.8)
  l3(c(1, 1), crt_line, f(c(1, 1), crt_line), pm, col = col_dem, lwd = 1.8)
}
fig_pdf("dem_3d_side",  width = w, height = h, pointsize = 9, mar = mar3, dem_side(FALSE))
fig_pdf("dem_3d_plane", width = w, height = h, pointsize = 9, mar = mar3, dem_side(TRUE))

# ---- log GDP model --------------------------------------------------------------
f_gdp <- function(court, lgdp) bg[1] + bg[2] * court + bg[3] * lgdp
gdp_r <- c(3, 5.2)
res <- residuals(fit_gdp)
gdp3d <- function(theta, plane, resid, phi = 18) {
  pm <- box3d(crt_r, gdp_r, cor_r, theta = theta, phi = phi,
              "Court indep.", "GDP (log10)", "Corruption", nt = 3)   # 3 ticks: the GDP label is not crowded
  pt <- function(sel) p3(d$courtind[sel], d$lgdp[sel], d$execorr100[sel], pm,
                         pch = 16, cex = 0.5, col = alpha_col(col_dark, 0.8))
  seg <- function(sel) {
    a <- trans3d(d$courtind[sel], d$lgdp[sel], d$execorr100[sel], pm)
    b <- trans3d(d$courtind[sel], d$lgdp[sel], fitted(fit_gdp)[sel], pm)
    segments(a$x, a$y, b$x, b$y, col = alpha_col(col_cardinal, 0.7), lwd = 0.7)
  }
  if (!plane) return(pt(rep(TRUE, nrow(d))))
  below <- res < 0
  if (resid) seg(below)
  pt(below)
  plane3d(f_gdp, crt_r, gdp_r, pm)
  if (resid) seg(!below)
  pt(!below)
}
fig_pdf("gdp_3d_points",  width = w, height = h, pointsize = 9, mar = mar3, gdp3d(-30, FALSE, FALSE))
fig_pdf("gdp_3d_plane",   width = w, height = h, pointsize = 9, mar = mar3, gdp3d(-30, TRUE, FALSE))
fig_pdf("gdp_3d_rotated", width = w, height = h, pointsize = 9, mar = mar3, gdp3d(-15, TRUE, FALSE, phi = 12))
fig_pdf("gdp_3d_resid",   width = w, height = h, pointsize = 9, mar = mar3, gdp3d(-30, TRUE, TRUE))
cat("fig_regression_plane: done; numbers in", out, "\n")
