all : tables graphs
	latexmk -pdf -pdflatex='pdflatex -interaction=nonstopmode %O %S' max_clique_tutorial

TABLES =

GRAPHS =

tables : $(TABLES)

gen-table-%.tex : make-table-%.bash goldilocks.list
	bash `echo $^ | sed -e 's,gen-table,make-table,' -e 's,\.tex,.bash,'` > $@

graphs : $(GRAPHS)

gen-graph-%.tex : graph-%.gnuplot
	gnuplot $<

