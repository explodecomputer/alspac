library(plyr)
library(foreign)


getFileList <- function(alspacdir)
{
	cmd1 <- paste("find ", alspacdir, " -iname '*.sav' > temp1", sep="")
	system(cmd1)
	fls <- scan("temp1", what="character", sep="\n")
	system("rm temp1")

	fls_dn <- dirname(fls)
	fls_bn <- basename(fls)
	fls_n <- gsub(".sav", fls_bn, ignore.case=T, replace="")
	fls_d <- gsub(alspacdir, "", fls_dn)
	nfield <- max(countCharOccurrences("/", fls_d)) + 1

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
	gsub("^\\s+|\\s+$", "", x)
}


createFileTable <- function(fls, alspacdir)
{
	fls_dn <- dirname(fls)
	fls_bn <- basename(fls)
	fls_n <- gsub(".sav", fls_bn, ignore.case=T, replace="")
	fls_d <- gsub(alspacdir, "", fls_dn)
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
	dat$objname <- fls_n
	return(dat)
}


createVarLabels <- function(fls, obj)
{
	fls <- gsub(" ", "\\ ", fls)
	cmd1 <- paste("./spssread.pl -r '", fls, "' > temp1", sep="")
	system(cmd1)
	dat <- read.table("temp1", he=T, sep="\t", colClasses=c("character", "factor", "character"), quote="")
	system("rm temp1")
	nom <- cbind(gsub(" ", "", tolower(names(obj))), gsub(" ", "", tolower(dat$name)))
	index <- nom[, 1] == nom[, 2]
	if(!all(index))
	{
		print(nom[!index,])
		print("problem with variable names?")
	}
	dat$code <- trimWhitespace(dat$name)
	dat$name <- names(obj)
	dat$label <- trimWhitespace(dat$label)
	dat$type <- trimWhitespace(dat$type)
	return(dat)
}


createDataObjectsSpss <- function(fls, rdir)
{
	require(plyr)
	require(foreign)
	index <- file.exists(fls)
	fls_bn <- basename(fls)
	fls_n <- gsub(".sav", fls_bn, ignore.case=T, replace="")
	fls.out <- paste(rdir, "/", fls_n, ".RData", sep="")

	dat <- data.frame(fls, fls_bn, fls_n, fls.out, exists=index, stringsAsFactors=FALSE)
	dat$success1 <- NA
	dat$success2 <- NA
	n <- nrow(dat)
	l <- list()
	j <- 1
	for(i in 1:n)
	{
		cat(i, ":", dat$fls_n[i], "\n")
		obj <- try(read.spss(dat$fls[i], to.data.frame=TRUE, trim.factor.names=TRUE, trim_values=TRUE))
		if(class(obj) == "try-error")
		{
			dat$success1 <- FALSE
		} else {
			dat$success1 <- TRUE
			assign(dat$fls_n[i], obj)
			save(list=dat$fls_n[i], file=dat$fls.out[i])
			labs <- try(createVarLabels(dat$fls[i], obj))
			labs$counts <- apply(obj, 2, function(x) sum(!is.na(x)))
			if(class(labs) == "try-error")
			{
				dat$success2 <- FALSE
			} else {
				dat$success2 <- TRUE
				labs$objname <- dat$fls_n[i]
				l[[j]] <- labs
				j <- j + 1
			}
		}
	}
	vars <- rbind.fill(l)
	names(vars) <- c("name", "type", "label", "code", "counts", "objname")
	return(list(vars = vars, files=dat))
}


createDictionary <- function(alspacdir, rdir)
{
	fls <- getFileList(alspacdir)
	files <- createFileTable(fls, alspacdir)
	a <- createDataObjectsSpss(fls, rdir)
	file_check <- a$files
	vars <- a$vars
	d <- merge(vars, files, by="objname", all=TRUE)
	return(d)
}


makeNote <- function(rdir)
{
	note <- paste("This folder contains contains all the data that is currently in R:\\Data\\Current and R:\\Data\\Useful_data (as of ", Sys.time(), "), except it has been extracted from SPSS files and converted into RData files. This data is here for use by the local R package 'alspac', which provides an interface to search and extract variables from the ALSPAC data dictionary.\n\nAll files are in a single level instead of in a heirarchical structure.", sep="")
	write.table(note, file=paste(rdir, "/README", sep=""), row=F, col=F, qu=F)
}


##

arguments <- commandArgs(T)
alspacdir <- arguments[1]
rdir <- basename(alspacdir)

current_rdir <- file.path(alspacdir, "Current")
useful_rdir <- file.path(alspacdir, "Useful_data")

rdir <- "Current_R"
dir.create(rdir, showWarnings = FALSE)

##

current <- createDictionary(current_rdir, "Current_R")
current$cat1 <- "Current"
useful <- createDictionary(useful_rdir, "Useful_data")
useful$cat1 <- "Useful_data"

save(current, file="../data/current.RData")
save(useful, file="../data/useful.RData")
makeNote(rdir)





#### OLD CODE


# createVarLabels_old1 <- function(fl, rdir)
# {
# 	require(plyr)
# 	fl_bn <- basename(fl)
# 	fl_n <- gsub(".dta", fl_bn, ignore.case=T, replace="")
# 	fl.csv <- paste(rdir, "inst/raw_data/", fl_bn, ".csv", sep="")
# 	fl.pt <- paste(rdir, "inst/raw_data/", fl_bn, ".parsed.txt", sep="")
# 	fl.out <- paste(rdir, "data/", fl_n, ".RData", sep="")
# 	existingfiles <- file.exists(fls) & file.exists(fl.pt)

# 	dat <- data.frame(fl_bn, fl_n, fls, fl.csv, fl.pt, fl.out, stringsAsFactors=FALSE)[existingfiles, ]
# 	print(head(dat))

# 	n <- nrow(dat)
# 	l <- list()

# 	for(i in 1:n)
# 	{
# 		cat(i, ":", dat$fl_n[i], "\n")
# 		l[[i]] <- read.table(dat$fl.pt[i], he=F, sep="\t", colCla="character")
# 		l[[i]]$name <- dat$fl_n[i]
# 	}

# 	labs <- rbind.fill(l)
# 	names(labs) <- c("name", "label", "file")
# 	return(labs)
# }


# createVarLabels_old2 <- function(fls, obj)
# {
# 	nom <- names(obj)
# 	n <- ncol(obj)
# 	cmd1 <- paste("strings < ", fls, " | head -n ", n*2 + 2, " | tail -n ", n*2, " > temp1")
# 	system(cmd1)
# 	a <- scan("temp1", what="character", sep="\n")
# 	system("rm temp1")
# 	v <- gsub("[^[:alnum:] ]", "", a[seq(1, n*2, 2)])
# 	l <- a[seq(2, n*2, 2)]
# 	dat <- data.frame(names = nom, names2 = v, labels = l)
# 	print(head(dat, 2)[,1:2])
# 	print(tail(dat, 2)[,1:2])
# 	return(dat)
# }

