# vim: set et ft=gnuplot sw=4 :

set terminal tikz color size 4in,2.6in font ",8"
set output "gen-graph-togian-brock400_1-speedup.tex"
set xlabel "Number of threads"
set ylabel "Speedup"

set xtics 2, 2, 2
set xtics add (8, 16, 24, 32, 40, 48, 56, 64)
set xtics nomirror
set xrange [0:64]
set key at screen 0.15, screen 0.8 left

set title "brock400\\_1, 32 modules / 64 cores"

plot \
    "graph-togian-brock400_1-speedup-nodonation.data" u 1:2 with lines lc 1 lt 1 lw 3 ti " Distance 1", \
    "graph-togian-brock400_1-speedup-nodonation-depth2.data" u 1:2 with lines lc 2 lt 1 lw 3 ti " Distance 2", \
    "graph-togian-brock400_1-speedup-nodonation-depth3.data" u 1:2 with lines lc 5 lt 1 lw 3 ti " Distance 3", \
    "graph-togian-brock400_1-speedup-d3.data" u 1:2 with lines lc 7 lt 1 lw 3 ti " Resplitting"


