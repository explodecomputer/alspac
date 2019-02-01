## directory of ALSPAC data
ALSPACDIR=R:/Data

## number of processors that are available
MCCORES=10 

all: report.html ../data/current.rdata  

../data/current.rdata: 
	Rscript create_dictionary_from_stata.R ${ALSPACDIR} ${MCCORES}

report.html: report.rmd ../data/current.rdata 
	Rscript -e "rmarkdown::render('report.rmd', output_format='all')"

clean:
	rm report.html ../data/current.rdata ../data/useful.rdata
