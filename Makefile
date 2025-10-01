.PHONY: help doc load reload r

RSCRIPT=Rscript --quiet --vanillaz
help:
	@echo "make doc     - Run devtools::document()"
	@echo "make load    - Load pkg in a fresh R session (won't affect your current one)"
	@echo "make reload  - doc + load (fresh session)"d
	@echo "make r       - Start R; your .Rprofile can auto-load the pkg"

doc:
	@$(RSCRIPT) -e 'if (!requireNamespace("devtools", quietly=TRUE)) install.packages("devtools"); devtools::document()'

load:
	@$(RSCRIPT) -e 'if (!requireNamespace("devtools", quietly=TRUE)) install.packages("devtools"); devtools::load_all(quiet=TRUE)'

reload: doc load

r:
	@R --quiet --no-save
	
context:
	files-to-prompt .  --ignore ./_archive -e qmd -e R  -e md -e Rmd --cxml -o notebooks/targets.txt