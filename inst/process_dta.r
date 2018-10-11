suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(readstata13)))

countCharOccurrences <- function(char, s)
{
	s2 <- gsub(char,"",s)
	return (nchar(s) - nchar(s2))
}


trimWhitespace <- function(x)
{
	if(is.numeric(x))
	{
		return(x)
	}
	flag <- is.factor(x)
	x <- gsub("^\\s+|\\s+$", "", x)
	if(flag) 
	{
		return(as.factor(x))
	} else {
		return(x)
	}
}


createFileTable <- function(fls, alspacdir)
{
	fls_dn <- dirname(fls)
	fls_bn <- basename(fls)
	fls_n <- gsub(".dta", "", fls_bn, ignore.case=T)
	fls_d <- gsub(alspacdir, "", fls_dn)
	fls_d <- gsub("^/", "", fls_d)
	nfield <- max(countCharOccurrences("/", fls_d)) + 1
	stopifnot(! any(duplicated(fls_n)))

	sp <- strsplit(fls_d, split="/")
	sp <- lapply(sp, function(x) {
		y <- rep(NA, nfield)
		y[1:length(x)] <- x
		return(y)
	})
	dat <- data.frame(do.call("rbind", sp), stringsAsFactors=FALSE)
	names(dat) <- paste("cat", 1:nfield, sep="")
	dat$obj <- fls_bn
	dat$path <- fls_d
	return(dat)
}

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

dat <- merge(
	process_dta(dta),
	createFileTable(dta, alspacdir),
	by="obj"
)

save(dat, file=out)
