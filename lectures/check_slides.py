#!/usr/bin/env python3
"""Flag slides whose content starts higher than the rest of the deck (2026-10-11).

A slide that is too full gets squeezed: LaTeX shrinks the space above its content, so the
content starts higher than on its neighbours and a figure jumps when the class moves from one
slide to the next. course_preamble.tex records where every slide's content starts (\\slidetop in
the .aux file). This script reads those records and lists every slide that starts higher than
the deck's normal position. The list must be empty; fix a flagged slide by making its content
smaller (a lower figure height, one line less), never by moving things by hand.

Slides are named by the number shown on the slide (the frame number in the footer), with the
overlay step when a frame has several, never by the PDF page.

Usage, from a deck folder after a build:  python3 ../check_slides.py lec4_1 [lec4_1_handout]
(for a deck in a v2/ folder: python3 ../../check_slides.py lec4_1). Exit status 1 if any slide is flagged.
"""
import re
import sys
from collections import Counter
from pathlib import Path

TOL_PT = 0.2  # positions within this many points of normal count as normal
SP_PER_PT = 65536


def check(job: str) -> int:
    aux = Path("build") / f"{job}.aux"
    if not aux.exists():
        aux = Path(f"{job}.aux")
    if not aux.exists():
        print(f"{job}: no .aux file (build the deck first)")
        return 1
    # y = content top; room = text height left below the title bar and the pad. A taller title
    # lowers y and shrinks room by the same amount, so y - room is the same on every unsqueezed
    # slide. Records come in shipout order, so their position gives the PDF page. Plain frames
    # (title page, roadmaps) are skipped.
    raw = re.findall(r"\\slidetop\{(\d+)\}\{(\d+)\}\{(-?\d+)\}\{(-?\d+)\}\{([PN])\}",
                     aux.read_text(errors="replace"))
    recs = [(i + 1, int(f), int(y) - int(room))
            for i, (_, f, y, room, kind) in enumerate(raw) if kind == "N"]
    if not recs:
        print(f"{job}: no \\slidetop records (is course_preamble.tex current?)")
        return 1
    normal = Counter(y for _, _, y in recs).most_common(1)[0][0]
    step = Counter()
    bad = []
    for page, frame, y in recs:
        step[frame] += 1
        hi = (y - normal) / SP_PER_PT
        if hi > TOL_PT:
            bad.append((frame, step[frame], page, hi))
    steps = Counter(f for _, f, _ in recs)
    jumps = figure_jumps(aux.read_text(errors="replace"))
    for a, b, dx, dy in jumps:
        print(f"{job}: figure jumps between {a} and {b}: {dx:+.2f}pt across, {dy:+.2f}pt up/down "
              "(same-size figure on consecutive slides must sit in the same place)")
    if not bad and not jumps:
        print(f"{job}: OK ({len(recs)} slides; no squeezed slide, no figure jumps)")
        return 0
    if not bad:
        return 1
    print(f"{job}: {len(bad)} slide(s) start higher than normal (too full; shrink their content):")
    for frame, k, page, hi in bad:
        name = f"slide {frame}" + (f", step {k} of {steps[frame]}" if steps[frame] > 1 else "")
        print(f"  {name}: {hi:.2f}pt higher   (pdf page {page}, for locating only)")
    return 1


def figure_jumps(text: str):
    """Consecutive pages that each show exactly one figure must place it at the same spot (within
    TOL_PT) if the two figures have the same size (within 0.5pt), and must show it at the same size
    and spot if it is the same file (added 2026-10-11 after 6-3 slides 10-11). Records come in shipout order: a
    \\slidetop line opens a page, \\figpos lines after it belong to that page."""
    pages = []  # (label, [(x, y, w, h)])
    steps = Counter()
    for m in re.finditer(r"\\(slidetop|figpos)\{(\d+)\}\{(\d+)\}\{(-?\d+)\}\{(-?\d+)\}(?:\{(-?\d+)\}\{(-?\d+)\}(?:\{([^}]*)\})?)?", text):
        kind = m.group(1)
        if kind == "slidetop":
            frame = int(m.group(3)); steps[frame] += 1
            pages.append([f"slide {frame}" + (f" (step {steps[frame]})" if steps[frame] > 1 else ""), []])
        elif pages:
            x, y, w, h = (int(m.group(i)) / SP_PER_PT for i in (4, 5, 6, 7))
            pages[-1][1].append((x, y, w, h, m.group(8) or ""))
    out = []
    for (la, fa), (lb, fb) in zip(pages, pages[1:]):
        if len(fa) == 1 and len(fb) == 1:
            (xa, ya, wa, ha, na), (xb, yb, wb, hb, nb) = fa[0], fb[0]
            same_file = bool(na) and na == nb
            same_size = abs(wa - wb) < 0.5 and abs(ha - hb) < 0.5
            moved = abs(xa - xb) > TOL_PT or abs(ya - yb) > TOL_PT
            if (same_size and moved) or (same_file and (moved or not same_size)):
                out.append((la, lb, xb - xa, yb - ya))
    return out


if __name__ == "__main__":
    jobs = sys.argv[1:]
    if not jobs:
        sys.exit(__doc__)
    sys.exit(max(check(j) for j in jobs))
