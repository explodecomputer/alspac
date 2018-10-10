library(dplyr)
suppressWarnings(library(readstata13))

process_dta <- function(fn)
{
	temp <- suppressWarnings(read.dta13(fn))
	dat <- data_frame(
		name = colnames(temp),
		lab = attributes(temp)$var.labels,
		counts = sapply(temp, function(x) sum(!is.na(x) & x != -10 & x != -11)),
		type = sapply(temp, function(x) class(x)[1]),
		obj = basename(fn)
	)
	return(dat)
}

args <- commandArgs(T)

dta <- args[1]
out <- args[2]
alspacdir <- args[3]

dat <- process_dta(dta)
dat$path <- gsub(alspacdir, "", dta)

save(dat, file=out)
