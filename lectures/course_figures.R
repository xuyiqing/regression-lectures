# course_figures.R --- shared figure style for the POLI 450A lecture decks.
#
# Every figure script lives in a deck's code/ folder and is run FROM THE DECK
# FOLDER, so paths are relative to it:
#
#   cd 04-1SimpleRegressionMechanics && Rscript code/fig_beo.R
#
# A script starts with
#
#   source("../course_figures.R")
#
# and draws each figure inside fig_pdf() (base graphics) or saves a ggplot
# with fig_gg(). Both write figs/<name>.pdf with the slide font embedded.
#
#   fig_pdf("sampling_dist", {
#     hist(x, col = pal["cardinal"], border = "white")
#   })
#
# Sizes. Slides are 16:9; the text block is about 5.9in x 3.4in. The default
# 6in x 3.4in figure fills a slide under its title when included with
# \includegraphics[height=0.82\textheight]{name}. Use width = 3.2 for a
# figure that shares the slide with a column of text.
#
# Keep a run of animation steps on identical axes (same xlim/ylim, same size).

col_cardinal <- "#A8003B"   # primary accent, matches course_theme.tex
col_dark     <- "#2B2B2B"   # text and axes
col_grey     <- "#8A8A8A"   # secondary lines, reference lines
col_light    <- "#D6D6D6"   # background points, grid

# Okabe-Ito colorblind-safe colors, cardinal first.
pal <- c(cardinal   = "#A8003B",
         blue       = "#0072B2",
         green      = "#009E73",
         orange     = "#E69F00",
         sky        = "#56B4E9",
         purple     = "#CC79A7",
         vermillion = "#D55E00")

# Transparent version of a color, for overlapping points.
alpha_col <- function(col, alpha = 0.35) grDevices::adjustcolor(col, alpha.f = alpha)

# The slides use Computer Modern Sans; CMU Sans Serif is the matching TrueType font.
fig_font <- local({
  fams <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
  if ("CMU Sans Serif" %in% fams) "CMU Sans Serif" else "sans"
})

fig_dir <- "figs"

# Base-graphics figure. `expr` is evaluated with the device open.
fig_pdf <- function(name, expr, width = 6, height = 3.4, pointsize = 11,
                    mar = c(3.2, 3.6, 1.0, 0.8)) {
  dir.create(fig_dir, showWarnings = FALSE)
  path <- file.path(fig_dir, paste0(name, ".pdf"))
  grDevices::cairo_pdf(path, width = width, height = height,
                       pointsize = pointsize, family = fig_font)
  graphics::par(mar = mar, mgp = c(2.1, 0.6, 0), tcl = -0.3, las = 1,
                bty = "l", fg = col_dark, col.axis = col_dark,
                col.lab = col_dark, col.main = col_dark, cex.axis = 0.9)
  tryCatch(force(expr), finally = grDevices::dev.off())
  invisible(path)
}

# ggplot2 theme and saver.
theme_450a <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = fig_font) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#EDEDED", linewidth = 0.3),
      axis.line        = ggplot2::element_line(colour = col_dark, linewidth = 0.3),
      axis.ticks       = ggplot2::element_line(colour = col_dark, linewidth = 0.3),
      axis.text        = ggplot2::element_text(colour = col_dark),
      axis.title       = ggplot2::element_text(colour = col_dark),
      legend.position  = "bottom",
      plot.margin      = ggplot2::margin(4, 8, 4, 4)
    )
}

fig_gg <- function(name, plot, width = 6, height = 3.4) {
  dir.create(fig_dir, showWarnings = FALSE)
  path <- file.path(fig_dir, paste0(name, ".pdf"))
  ggplot2::ggsave(path, plot, width = width, height = height,
                  device = grDevices::cairo_pdf)
  invisible(path)
}
