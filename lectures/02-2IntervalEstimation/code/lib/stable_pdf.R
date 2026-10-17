# stable_pdf.R --- rewrite a figure file only when the picture changes.
#
# Sourced by this deck's code/fig_*.R scripts, after ../course_figures.R.
#
# Why: cairo writes the current time (/CreationDate) into every PDF, so each
# run of a figure script changes figs/<name>.pdf even when the drawing is the
# same. latexmk then rebuilds the deck, and on the Dropbox file provider the
# rebuilt lec2_2.synctex.gz has its timestamp rewritten a few seconds later,
# which the overnight gate script reports as a stray write by whichever
# figure script is running at that moment.
#
# stable_pdf() takes the same arguments as fig_pdf(). It draws with fig_pdf()
# into a temporary folder, compares the result with figs/<name>.pdf while
# ignoring the time stamp, and copies it into figs/ only if they differ.

pdf_fingerprint <- function(path) {
  x <- readBin(path, "raw", file.size(path))
  open_tag <- ">>\nstream\n"
  starts <- grepRaw(open_tag, x, fixed = TRUE, all = TRUE)
  ends <- grepRaw("\nendstream", x, fixed = TRUE, all = TRUE)
  if (length(starts) == 0 || length(starts) != length(ends)) return(list(x))
  keep <- rep(TRUE, length(x))
  streams <- vector("list", length(starts))
  prev <- 1
  for (i in seq_along(starts)) {
    s <- starts[i] + nchar(open_tag)
    e <- ends[i] - 1
    before <- x[prev:(starts[i] + 1)]          # dictionaries since the last stream
    keep[s:e] <- FALSE
    prev <- e + 1
    # the cross-reference stream holds only byte offsets: skip it
    if (length(grepRaw("/Type /XRef", before, fixed = TRUE))) next
    dec <- tryCatch(memDecompress(x[s:e], type = "gzip"), error = function(err) x[s:e])
    hit <- grepRaw("/CreationDate (D:", dec, fixed = TRUE)
    if (length(hit) == 1) {
      p <- hit + nchar("/CreationDate (D:")
      dec[p:(p + 13)] <- charToRaw("00000000000000")   # YYYYMMDDHHMMSS
    }
    streams[[i]] <- dec
  }
  # outside the streams, ignore stream lengths and the xref position,
  # which move when the compressed time stamp changes length
  meta <- tryCatch({
    txt <- rawToChar(x[keep])
    txt <- gsub("[0-9]+ 0 obj\n *[0-9]+\nendobj", "LENGTH", txt, useBytes = TRUE)
    txt <- gsub("/Length [0-9]+", "/Length N", txt, useBytes = TRUE)
    gsub("startxref\n[0-9]+", "startxref N", txt, useBytes = TRUE)
  }, error = function(err) x[keep])
  c(list(meta), streams)
}

stable_pdf <- function(name, expr, ...) {
  final <- file.path("figs", paste0(name, ".pdf"))
  tmp <- tempfile("fig_")
  old_dir <- fig_dir
  fig_dir <<- tmp                       # fig_pdf() writes into fig_dir
  on.exit({ fig_dir <<- old_dir; unlink(tmp, recursive = TRUE) }, add = TRUE)
  drawn <- fig_pdf(name, expr, ...)
  if (!file.exists(final) ||
      !identical(pdf_fingerprint(drawn), pdf_fingerprint(final))) {
    dir.create("figs", showWarnings = FALSE)
    if (!file.copy(drawn, final, overwrite = TRUE)) stop("could not write ", final)
    message("wrote ", final)
  } else {
    message("unchanged ", final)
  }
  invisible(final)
}
