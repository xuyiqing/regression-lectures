# POLI 450A: Political Methodology I (Regression)

Lecture slides, R code, and data for the first course in Stanford's political science PhD methods
sequence: LaTeX Beamer sources, PDFs, the R scripts that draw every figure and produce every number
on the slides, and the public datasets they use. Short preview and review videos accompany the lectures
([YouTube playlist](https://www.youtube.com/playlist?list=PLbHvZ-k41x9s)). The course also suits advanced undergraduates in economics, political
science, and other social sciences.
Version: Fall 2026.

Course website: <https://xuyiqing.github.io/regression-lectures/>

Each lecture entered this repository after a release review: numbers rerun from the code, citations
checked against their sources, and independent line-by-line accuracy reads, with the fixes applied.
The scripts and data behind lecture 5-4's replication figures will be added later.

The course was originally designed by Jens Hainmueller and is maintained and updated by Yiqing Xu
(Department of Political Science, Stanford University).

| Lecture | Topic | Slides | Video | Worksheet |
|---|---|---|---|---|
| 1 | Introduction | [`lec1_handout.pdf`](lectures/_handouts/lec1_handout.pdf) | [5:29](https://www.youtube.com/watch?v=iF-UXSAJUKI&list=PLbHvZ-k41x9s) | [`lec1.pdf`](worksheets/lec1.pdf) |
| 2-1 | Point estimation | [`lec2_1_handout.pdf`](lectures/_handouts/lec2_1_handout.pdf) | [5:34](https://www.youtube.com/watch?v=Swxf5lM_u24&list=PLbHvZ-k41x9s) | [`lec2-1.pdf`](worksheets/lec2-1.pdf) |
| 2-2 | Interval estimation | [`lec2_2_handout.pdf`](lectures/_handouts/lec2_2_handout.pdf) | [5:29](https://www.youtube.com/watch?v=7T9ndJHs5_I&list=PLbHvZ-k41x9s) | [`lec2-2.pdf`](worksheets/lec2-2.pdf) |
| 2-3 | Hypothesis testing | [`lec2_3_handout.pdf`](lectures/_handouts/lec2_3_handout.pdf) | [5:24](https://www.youtube.com/watch?v=WODM9NLojLQ&list=PLbHvZ-k41x9s) | [`lec2-3.pdf`](worksheets/lec2-3.pdf) |
| 2-4 | Bootstrap and clusters | [`lec2_4_handout.pdf`](lectures/_handouts/lec2_4_handout.pdf) | [6:15](https://www.youtube.com/watch?v=hGvSrkmQy1I&list=PLbHvZ-k41x9s) | [`lec2-4.pdf`](worksheets/lec2-4.pdf) |
| 3 | What is regression? | [`lec3_handout.pdf`](lectures/_handouts/lec3_handout.pdf) | [7:36](https://www.youtube.com/watch?v=wllni_KnmjQ&list=PLbHvZ-k41x9s) | [`lec3.pdf`](worksheets/lec3.pdf) |
| 4-1 | Simple regression: mechanics of OLS | [`lec4_1_handout.pdf`](lectures/_handouts/lec4_1_handout.pdf) | [6:30](https://www.youtube.com/watch?v=7flAEtlI_Co&list=PLbHvZ-k41x9s) | [`lec4-1.pdf`](worksheets/lec4-1.pdf) |
| 4-2 | Simple regression: statistical properties | [`lec4_2_handout.pdf`](lectures/_handouts/lec4_2_handout.pdf) | [7:34](https://www.youtube.com/watch?v=tieM_0nF3uE&list=PLbHvZ-k41x9s) | [`lec4-2.pdf`](worksheets/lec4-2.pdf) |
| 4-3 | Simple regression: inference and interpretation | [`lec4_3_handout.pdf`](lectures/_handouts/lec4_3_handout.pdf) | [7:52](https://www.youtube.com/watch?v=aslK8sGa63Y&list=PLbHvZ-k41x9s) | [`lec4-3.pdf`](worksheets/lec4-3.pdf) |
| 5-1 | Regression with two regressors | [`lec5_1_handout.pdf`](lectures/_handouts/lec5_1_handout.pdf) | [7:28](https://www.youtube.com/watch?v=8ATj0tzDJnI&list=PLbHvZ-k41x9s) | [`lec5-1.pdf`](worksheets/lec5-1.pdf) |
| 5-2 | Omitted variables and multicollinearity | [`lec5_2_handout.pdf`](lectures/_handouts/lec5_2_handout.pdf) | [7:38](https://www.youtube.com/watch?v=4Sn7V8B4gXU&list=PLbHvZ-k41x9s) | [`lec5-2.pdf`](worksheets/lec5-2.pdf) |
| 5-3 | Dummy variables | [`lec5_3_handout.pdf`](lectures/_handouts/lec5_3_handout.pdf) | [7:08](https://www.youtube.com/watch?v=5Gq1oJ76daU&list=PLbHvZ-k41x9s) | [`lec5-3.pdf`](worksheets/lec5-3.pdf) |
| 5-4 | Interaction terms and polynomials | [`lec5_4_handout.pdf`](lectures/_handouts/lec5_4_handout.pdf) | [11:33](https://www.youtube.com/watch?v=6XabG7d0sTs&list=PLbHvZ-k41x9s) | [`lec5-4.pdf`](worksheets/lec5-4.pdf) |
| 6-1 | Multiple regression in matrix form | [`lec6_1_handout.pdf`](lectures/_handouts/lec6_1_handout.pdf) | [5:20](https://www.youtube.com/watch?v=Joh2vmuyrUc&list=PLbHvZ-k41x9s) | [`lec6-1.pdf`](worksheets/lec6-1.pdf) |
| 6-2 | Testing in multiple regression | [`lec6_2_handout.pdf`](lectures/_handouts/lec6_2_handout.pdf) | [7:32](https://www.youtube.com/watch?v=5bI7HBBLsMU&list=PLbHvZ-k41x9s) | [`lec6-2.pdf`](worksheets/lec6-2.pdf) |
| 6-3 | Heteroskedasticity, clustering and serial correlation | [`lec6_3_handout.pdf`](lectures/_handouts/lec6_3_handout.pdf) | [8:37](https://www.youtube.com/watch?v=Jgi9q1KpqK4&list=PLbHvZ-k41x9s) | [`lec6-3.pdf`](worksheets/lec6-3.pdf) |

## Layout

Each lecture folder under `lectures/` holds its slide source at the top. The slides are released as
handouts, one page per slide, all in `lectures/_handouts/`. The version with the step-by-step
reveals, for presenting, is not stored here; build it from the source (below).

| Path | Holds |
|---|---|
| `lec<N>.tex` | slide source |
| `code/*.R` | scripts that draw the figures and print the numbers on the slides |
| `code/*.txt` | the scripts' printed output, which the slides quote |
| `code/data/` | the data the scripts read |
| `figs/` | every figure the slides include |

The in-class worksheets sit in `worksheets/`: `lec<N>.tex` and its PDF, one per lecture (student
versions; the solutions are not in this repository). The course website's source is in `site/`;
`quarto render` there writes the pages to `docs/`, and `site/assemble.sh` adds the PDFs when the
site is published.

All lectures share one template: `lectures/course_preamble.tex` (theme, layout, packages,
notation) and `lectures/course_theme.tex` (colors and slide furniture). Every R script starts with
`source("../course_figures.R")`, the shared figure style.

Lecture 4 comes in three decks, one per class meeting (4-1, 4-2, 4-3). Two scripts serve two of them and have a
trimmed copy in each (`fig_sampling.R`, `fig_errors.R`), and 4-3's `fig_tdist.R` reads the data in
`lectures/04-1SimpleRegressionMechanics/code/data/`.

Lecture 1 leaves out the logistics of a particular term (meeting times, exam dates, rooms, staff): its
source sets `\releasetrue` near the top. Lecture 5-4's replication figures (`figs/hmx_*.pdf`,
`figs/malesky_me_linear.pdf`) are included, but the scripts and data that draw them will be added
later.

## Building

You need TeX Live 2023 or later with `latexmk` and `pdflatex`, and R 4.x.

```bash
cd lectures/02-4MoreInference
latexmk lec2_4.tex                                    # version with the reveals, for presenting
latexmk -jobname=lec2_4_handout \
  -usepretex='\PassOptionsToClass{handout}{beamer}' lec2_4.tex   # handout, moved to ../_handouts/
Rscript code/fig_cluster_coverage.R                   # redraw figures; run from the lecture folder
```

R packages used by the scripts: `ggplot2`, `sandwich`, `lmtest`, `foreign`, `carData`, `car`, `mgcv`,
`interflex` (1.4.1 or later).
`lectures/check_slides.py` (Python 3) checks after a build that no slide is squeezed: run
`python3 ../check_slides.py lec2_4 lec2_4_handout` from the lecture folder.

## Videos

Each lecture has a video of five to twelve minutes (Manim animation, in 4K, with captions as YouTube subtitles).
It presents the main ideas, one or two real examples, and the numbers from the lecture's own R code.
The videos are designed to be conceptual and intuitive; the details of the mathematical derivations
are covered in class.
We recommend that students watch a video before the lecture to preview the ideas and again
afterwards to review them.

In class, we play a video one part at a time and explain each concept, sometimes pausing mid-part to
settle a conceptual question. We then turn to the slides for the details and work through worksheet
questions with the students at the whiteboard. A silent page between two parts names the slide where
the next part starts, so that a viewer can pause there and open the slides.

The narration is a synthetic voice (Kokoro-82M), and the scripts and animations were drafted with
Claude (Anthropic). Every number on screen comes from the lecture's own code and data. The videos
are released as finished files, not as sources, in one [YouTube playlist](https://www.youtube.com/playlist?list=PLbHvZ-k41x9s); the table
above links each one.

## License

Except where otherwise noted:

* The slides, their LaTeX sources, the worksheets, original figures, and the videos are licensed under the
  [Creative Commons Attribution-NonCommercial 4.0 International License](https://creativecommons.org/licenses/by-nc/4.0/)
  (CC BY-NC 4.0). See [`LICENSE`](LICENSE). You may copy, adapt and teach with them, with credit,
  but not for commercial purposes.
* The R code is licensed under the [MIT License](LICENSE-CODE).
* Data files keep their own terms, listed in [`SOURCES.md`](SOURCES.md), with the original source
  for each.
