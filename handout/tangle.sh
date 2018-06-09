#!/usr/bin/env bash

for f in *.Rmd; do Rscript -e "knitr::purl('$f')"; done
