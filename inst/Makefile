R := Rscript
ALSPACDIR?=/Volumes/ALSPAC-Data
SOURCES := $(shell find $(ALSPACDIR)/Current -iname '*.dta')
SOURCES += $(shell find $(ALSPACDIR)/Useful_data -iname '*.dta')
vpath %.dta $(sort $(dir $(SOURCES)))

OBJECTS := $(notdir $(SOURCES))
OBJECTS := $(OBJECTS:%.dta=scratch/%.rdata)

# $(info   objects: $(OBJECTS))
# $(info   sources: $(SOURCES))

all: ../data/Current.RData ../data/Useful_data.RData

../data/Current.RData ../data/Useful_data.RData: $(OBJECTS)
	$(R) collate.r $(OBJECTS)

scratch/%.rdata: %.dta
	$(R) process_dta.r $< $@ $(ALSPACDIR)

dirs:
	@mkdir -p scratch

report: report.html

%.html: %.rmd
	./render $<
