#!/usr/bin/env sh

for f in *.Rmd; do Rscript -e "rmarkdown::render('$f')"; done
