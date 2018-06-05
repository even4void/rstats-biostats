TARGET := $(patsubst %.Rmd,%.html,$(wildcard *.Rmd))

.PHONY: all clean

all: $(TARGET)

%.html : %.Rmd
	Rscript -e "rmarkdown::render('$^')"
    

tangle:
    Rscript -e "knitr::purl('$^')"

clean:
	rm -f $(TARGET)
