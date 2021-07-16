all: outputs/extractVars.txt outputs/extractDataset.txt

outputs/extractDataset.txt: inputs/variables.csv inputs/ACEHDBFG.txt

outputs/%.txt: src/%.r
	/usr/bin/Rscript -e "source('src/$*.r', echo=T)" > $@

clean: 
	rm -f outputs/*
