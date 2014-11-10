summary.pdf: summary.md summary_template.tex
	pandoc --template=summary_template.tex \
		     --latex-engine=xelatex \
	  		 --variable monofont=Menlo \
				 summary.md -o summary.pdf
