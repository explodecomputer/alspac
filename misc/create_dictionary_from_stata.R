library(plyr)
library(readstata13)


getFileList <- function(alspacdir)
{
	cmd1 <- paste("find ", alspacdir, " -iname '*.dta' > temp1", sep="")
	system(cmd1)
	fls <- scan("temp1", what="character", sep="\n")
	system("rm temp1")

	fls_bn <- basename(fls)
	fls_n <- gsub(".dta", fls_bn, ignore.case=T, replace="")

	if(any(duplicated(fls_n)))
	{
		msg <- paste0("Some duplicated filenames!\n",
			paste(fls_n[duplicated(fls_n)], collapse="\n")
		)
		message(msg)
	}

	fls <- fls[!duplicated(fls_n)]
	return(fls)
}


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

makeDictionaryStata <- function(path, subdir)
{
	fullpath <- file.path(path, subdir)
	fls <- getFileList(fullpath)
	files <- createFileTable(fls, path)
	l <- list()
	n <- length(fls)
	for(i in 1:n)
	{
		message(i, ": ", fls[i])
		temp <- read.dta13(fls[i])
		dat <- data.frame(
			name = colnames(temp),
			lab = attributes(temp)$var.labels,
			counts = sapply(temp, function(x) sum(!is.na(x) | x == -10 | x == -11)),
			type = sapply(temp, function(x) class(x)[1]),
			obj = basename(fls[i]),
			stringsAsFactors=FALSE
		)
		l[[i]] <- dat
	}
	vars <- rbind.fill(l)
	vars <- merge(vars, files, by="obj")
	return(vars)
}


##

arguments <- commandArgs(T)
alspacdir <- arguments[1]

current <- makeDictionaryStata(alspacdir, "Current")
save(current, file="../data/current.RData")

useful <- makeDictionaryStata(alspacdir, "Useful_data")
save(useful, file="../data/useful.RData")

