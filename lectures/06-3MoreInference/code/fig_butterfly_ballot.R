# fig_butterfly_ballot.R --- lecture 6-3, "The Butterfly Ballot".
#
# Redraws the layout of the presidential page of the Palm Beach County ballot
# (general election, November 7, 2000): two columns of candidates with one
# column of punch holes between them, numbered 3 to 13 from the top. Odd holes
# belong to the left column, even holes to the right. Gore is second on the
# left, but the second hole (4) is Buchanan's.
#
# Replaces fig7.pdf, a page of Wand et al. (2001, APSR) whose Figure 1 is an
# AP photo of the ballot. Candidate order, parties and hole numbers are read
# from that photo; the drawing is a schematic, not a copy of the ballot.
#
# Run from the deck folder:  Rscript code/fig_butterfly_ballot.R

source("../course_figures.R")

left <- data.frame(
  hole  = c(3, 5, 7, 9, 11, 13),
  party = c("REPUBLICAN", "DEMOCRATIC", "LIBERTARIAN", "GREEN",
            "SOCIALIST WORKERS", "NATURAL LAW"),
  pres  = c("George W. Bush", "Al Gore", "Harry Browne", "Ralph Nader",
            "James Harris", "John Hagelin"),
  vp    = c("Dick Cheney", "Joe Lieberman", "Art Olivier", "Winona LaDuke",
            "Margaret Trowe", "Nat Goldhaber"))
right <- data.frame(
  hole  = c(4, 6, 8, 10),
  party = c("REFORM", "SOCIALIST", "CONSTITUTION", "WORKERS WORLD"),
  pres  = c("Pat Buchanan", "David McReynolds", "Howard Phillips", "Monica Moorehead"),
  vp    = c("Ezola Foster", "Mary Cal Hollis", "J. Curtis Frazier", "Gloria La Riva"))

y_of <- function(h) 80 - (h - 3) * 6.6     # hole 3 at the top, 13 at the bottom
hl <- function(h) if (h %in% c(4, 5)) col_cardinal else col_dark

fig_pdf("butterfly_ballot", width = 6, height = 3.4, mar = c(0, 0, 0, 0), {
  plot.new()
  plot.window(xlim = c(0, 100), ylim = c(0, 100), xaxs = "i", yaxs = "i")
  # the two pages and the punch-hole strip
  rect(1, 6, 45.5, 99, border = col_grey, lwd = 0.6)
  rect(54.5, 6, 99, 99, border = col_grey, lwd = 0.6)
  rect(47, 6, 53, 99, col = "#F2F2F2", border = col_grey, lwd = 0.6)
  hdr <- "OFFICIAL BALLOT, GENERAL ELECTION\nPALM BEACH COUNTY, FLORIDA, NOVEMBER 7, 2000"
  text(23.25, 97.5, hdr, cex = 0.5, col = col_grey, pos = 1)
  text(76.75, 97.5, hdr, cex = 0.5, col = col_grey, pos = 1)
  # holes 3 to 13; hole 12 has no candidate
  for (h in 3:13) {
    y <- y_of(h)
    points(50, y, pch = 21, cex = 1.15, bg = "white", col = col_dark, lwd = 0.6)
  }
  # left column: candidate block, then the arrow pointing right to its hole
  for (i in seq_len(nrow(left))) {
    h <- left$hole[i]; y <- y_of(h); cl <- hl(h)
    text(3.5, y + 2.4, paste0("(", left$party[i], ")"), adj = 0, cex = 0.6, col = col_grey)
    text(3.5, y - 2, paste0(left$pres[i], " / ", left$vp[i]), adj = 0, cex = 0.8,
         col = cl, font = if (h == 5) 2 else 1)
    text(41.6, y, h, adj = 1, cex = 0.8, col = cl, font = 2)
    arrows(42.4, y, 48.6, y, length = 0.05, col = cl, lwd = if (h == 5) 1.4 else 0.7)
  }
  # right column: arrow pointing left to its hole, then the candidate block
  for (i in seq_len(nrow(right))) {
    h <- right$hole[i]; y <- y_of(h); cl <- hl(h)
    arrows(57.6, y, 51.4, y, length = 0.05, col = cl, lwd = if (h == 4) 1.4 else 0.7)
    text(58.4, y, h, adj = 0, cex = 0.8, col = cl, font = 2)
    text(62, y + 2.4, paste0("(", right$party[i], ")"), adj = 0, cex = 0.6, col = col_grey)
    text(62, y - 2, paste0(right$pres[i], " / ", right$vp[i]), adj = 0, cex = 0.8,
         col = cl, font = if (h == 4) 2 else 1)
  }
  text(62, y_of(12) + 0.2, "WRITE-IN CANDIDATE", adj = 0, cex = 0.6, col = col_grey)
})
