all : tables graphs
	latexmk -pdf -pdflatex='pdflatex -interaction=nonstopmode %O %S' talk

TABLES =

GRAPHS = gen-graph-togian-brock400_1-speedup.tex

tables : $(TABLES)

graphs : $(GRAPHS)

gen-graph-%.tex : graph-%.gnuplot
	gnuplot $<

