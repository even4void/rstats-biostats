TARGET := $(patsubst %.Rmd,%.html,$(wildcard *.Rmd))

.PHONY: all clean

all: $(TARGET)

%.html : %.Rmd
	Rscript -e "rmarkdown::render('$^')"
    
clean:
	rm -f $(TARGET)
