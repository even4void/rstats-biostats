#!/usr/bin/env sh

Rscript -e "rmarkdown::render('handout.Rmd', c('html_document', 'pdf_document'))"
