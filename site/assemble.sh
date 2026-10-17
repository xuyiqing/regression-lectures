#!/bin/sh
# Assemble the published site in $1: the rendered pages from docs/ plus the PDFs they link to
# (pdf/lecN_handout.pdf, pdf/worksheet_lecN.pdf), copied from where the repository
# keeps them, so no PDF is stored twice. Run from the repository root. Used by
# .github/workflows/pages.yml and for local previews.
set -e
out="${1:?usage: site/assemble.sh <output-dir>}"
rm -rf "$out"
mkdir -p "$out/pdf"
cp -R docs/. "$out/"
cp lectures/_handouts/*_handout.pdf "$out/pdf/"
for f in worksheets/lec*.pdf; do cp "$f" "$out/pdf/worksheet_$(basename "$f")"; done
echo "assembled $(ls "$out/pdf" | wc -l | tr -d ' ') PDFs in $out"
