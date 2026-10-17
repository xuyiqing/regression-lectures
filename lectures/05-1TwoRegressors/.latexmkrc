$pdf_mode = 1;
$aux_dir = "build";
$emulate_aux = 1;
$pdflatex = "pdflatex -synctex=1 -interaction=nonstopmode %O %S";
# Handouts go to ../_handouts/ (Yiqing, 2026-10-11): after a successful handout build, move the PDF there.
$success_cmd = 'case %R in *_handout) mkdir -p ../_handouts && mv -f %D ../_handouts/ ;; esac';
