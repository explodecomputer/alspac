loadDictionaries <- function() {        
    path <- file.path(system.file(package = "alspac"), "data")
    assign("globals", new.env(), envir=parent.env(environment()))
    for (file in list.files(path, "rdata$", full.names=T))
        load(file, globals)
}

retrieveDictionary <- function(name) {
    if (name %in% ls(envir=globals))
        get(name, envir=globals)
    else
        stop("dictionary '", name, "' does not exist")
}

saveDictionary <- function(name, dictionary) {
    assign(name, dictionary, globals)
    path <- file.path(system.file(package="alspac"), "data")
    if (!file.exists(path))
        dir.create(path)
    save(list=name,
         file=file.path(path, paste(name, "rdata", sep=".")),
         envir=globals)
}


#' Create a dictionary from ALSPAC STATA files
#'
#' @param datadir ALSPAC data subdirectory from which to create the index
#' (Default: "Current").  It could be "Current" or "Useful_data".
#' @param name If not \code{NULL}, then the resulting dictionary
#' will be saved to a file in the R package for use next time the package
#' is loaded. The dictionary will be available with the given name.
#'
#' The function uses multiple processors using \code{\link{mclapply}()}.
#' Use multiple processors by setting \code{mc.cores} option using
#' \code{options()}.
#' 
#' @export
#' @return Data frame dictionary listing available variables.
createDictionary <- function(datadir="Current", name=NULL) {
    stopifnot(datadir %in% c("Current","Useful_data"))
    
    alspacdir <- options()$alspac_data_dir
    datadir <- file.path(alspacdir, datadir)
    files <- list.files(datadir,  
                        pattern="dta$",
                        full.names=T,
                        recursive=T,
                        ignore.case=T)

    dictionary <- mclapply(files, function(file) {
        cat(date(), "loading", file, "\n")
        merge(processDTA(file),
              createFileTable(file, alspacdir),
              by="obj")
    }) %>% bind_rows

    if (!is.null(name))
        saveDictionary(name, dictionary)
    
    invisible(dictionary)
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

processDTA <- function(fn)
{
	temp <- suppressWarnings(readstata13::read.dta13(fn))
	# temp <- haven::read_dta(fn)
	dat <- data_frame(
		name = colnames(temp),
		lab = attributes(temp)$var.labels,
		# lab = sapply(temp, function(x) attr(x, "label")),
		counts = sapply(temp, function(x) sum(!is.na(x) & x != -10 & x != -11)),
		type = sapply(temp, function(x) class(x)[1]),
		obj = basename(fn)
	)
	return(dat)
}

