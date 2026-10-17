# fig_sankey.R --- Lecture 1, second "Social Science Objectives" frame.
#
# Redraws Figure 2 ("Sequential Classification of Type of Studies") of
# Torreblanca, Dinneen, Grossman & Xu (2026), "The Credibility Revolution in
# Political Science," arXiv:2601.11542, in the course style: the same flow
# diagram, node labels, counts, and percentages, from the paper's version of
# August 15, 2026.
#
# Model-based papers are not papers that use a model (almost every study
# does): they have no design-based source of identifying variation, so their
# causal claims rest on modeling assumptions. The paper does not treat the use
# of linear regression as a sign of model-based work.
#
# The classification data behind the counts is not in the course folder, so
# this script cannot recount them. It checks that every split adds up.
# Percentages use all 91,632 studies as the denominator, as in the paper.
# The lecture 1 video (videos/src/lec1-introduction/prep.py) reads the id and
# n vectors below, so keep their names.
#
# Run from the deck folder:  Rscript code/fig_sankey.R
# Writes figs/sankey_goals.pdf.

source("../course_figures.R")

# ---- counts (Figure 2 of the paper) ------------------------------------------
nodes <- data.frame(
  id    = c("papers", "nonquant", "quant",
            "describe", "predict", "explain",
            "model", "design"),
  label = c("All studies",
            "Qualitative,\nnormative, formal,\nreview, or\nmethodological",
            "Empirical\nquantitative",
            "Descriptive", "Predictive", "Explanatory",
            "Model-based", "Design-based"),
  n     = c(91632, 56283, 35349,
            5115, 247, 29987,
            22495, 7492),
  level = c(0, 1, 1, 2, 2, 2, 3, 3),
  stringsAsFactors = FALSE
)
rownames(nodes) <- nodes$id

# Each node's parent. Children are listed top to bottom.
parent <- c(nonquant = "papers", quant = "papers",
            describe = "quant", predict = "quant", explain = "quant",
            model = "explain", design = "explain")

# 56,283 + 35,349 = 91,632; 5,115 + 247 + 29,987 = 35,349;
# 22,495 + 7,492 = 29,987.
for (p in unique(parent)) {
  kids <- names(parent)[parent == p]
  stopifnot(sum(nodes[kids, "n"]) == nodes[p, "n"])
}

# Share of all studies, printed as in the paper: "(61%)", and "(<1%)" below 1%.
pct_all <- function(n) {
  s <- 100 * n / nodes["papers", "n"]
  if (s < 1) "(<1%)" else sprintf("(%d%%)", round(s))
}

# ---- layout (inches; the device is 7 x 3.4) ----------------------------------
# At height = 0.80\textheight (0.82 until the source line was added on
# 2026-09-28) the figure is 5.6in wide, inside the 5.9in text block.
fig_w <- 7
fig_h <- 3.4
k     <- 2.75 / nodes["papers", "n"]      # inches per paper, same for every node
bar_w <- 0.10                             # node bar width
x_lev <- c(1.00, 2.20, 4.30, 5.55)        # bar centers for levels 0-3

# Top of each node's bar, in inches from the bottom of the figure. Descriptive
# and Predictive sit high, so their ribbons climb to the right of the
# four-line label of the qualitative node, not through it. Explanatory sits
# higher than Empirical quantitative, so their labels do not share a row.
top <- c(papers   = 3.05,
         nonquant = 3.25, quant    = 3.25 - nodes["nonquant", "n"] * k - 0.35,
         describe = 3.15, predict  = 2.45, explain  = 1.45,
         model    = 2.45, design   = 1.10)
nodes$x   <- x_lev[nodes$level + 1]
nodes$yhi <- top[nodes$id]
nodes$ylo <- nodes$yhi - nodes$n * k

# One color per level, as in the 2025 figure (root, two branches, goals,
# methods). A ribbon takes the color of the node it leaves.
lev_col   <- c(col_grey, pal[["sky"]], col_cardinal, pal[["green"]])
nodes$col <- lev_col[nodes$level + 1]

# ---- drawing helpers ---------------------------------------------------------
ribbon <- function(xa, alo, ahi, xb, blo, bhi, col, n = 80) {
  t  <- seq(0, 1, length.out = n)
  s  <- (1 - cos(pi * t)) / 2                       # smooth S from 0 to 1
  xx <- xa + (xb - xa) * t
  lo <- alo + (blo - alo) * s
  hi <- ahi + (bhi - ahi) * s
  polygon(c(xx, rev(xx)), c(hi, rev(lo)), col = alpha_col(col, 0.32), border = NA)
  if (ahi - alo < 0.01)                             # keep the tiny Predictive flow visible
    lines(xx, (lo + hi) / 2, col = alpha_col(col, 0.8), lwd = 0.6)
}

node_label <- function(id, side) {
  nd  <- nodes[id, ]
  gap <- bar_w / 2 + 0.06
  xx  <- if (side == "left") nd$x - gap else nd$x + gap
  ha  <- if (side == "left") 1 else 0
  yy  <- (nd$ylo + nd$yhi) / 2
  text(xx, yy + 0.035, nd$label, adj = c(ha, 0), font = 2, cex = 0.95, col = col_dark)
  text(xx, yy - 0.035, paste(format(nd$n, big.mark = ","), pct_all(nd$n)),
       adj = c(ha, 1), cex = 0.9, col = col_dark)
}

# ---- figure ------------------------------------------------------------------
fig_pdf("sankey_goals", width = fig_w, height = fig_h, mar = c(0, 0, 0, 0), {
  plot.new()
  plot.window(xlim = c(0, fig_w), ylim = c(0, fig_h), xaxs = "i", yaxs = "i")

  # ribbons: a node's outflows are stacked top to bottom in child order
  for (p in unique(parent)) {
    kids <- names(parent)[parent == p]
    cur  <- nodes[p, "yhi"]
    for (kid in kids) {
      h <- nodes[kid, "n"] * k
      ribbon(nodes[p, "x"] + bar_w / 2, cur - h, cur,
             nodes[kid, "x"] - bar_w / 2, nodes[kid, "ylo"], nodes[kid, "yhi"],
             col = nodes[p, "col"])
      cur <- cur - h
    }
  }

  # node bars (a minimum height keeps Predictive visible)
  for (id in nodes$id) {
    nd  <- nodes[id, ]
    mid <- (nd$ylo + nd$yhi) / 2
    hh  <- max(nd$yhi - nd$ylo, 0.02)
    rect(nd$x - bar_w / 2, mid - hh / 2, nd$x + bar_w / 2, mid + hh / 2,
         col = nd$col, border = NA)
  }

  # labels go where no ribbon passes, except Empirical quantitative and
  # Explanatory, which sit on light ribbons
  node_label("papers", "left")
  for (id in c("nonquant", "quant", "describe", "predict")) node_label(id, "right")
  node_label("explain", "left")
  for (id in c("model", "design")) node_label(id, "right")
})
