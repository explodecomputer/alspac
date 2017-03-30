# Functions for querying the ALSPAC variables data dictionary
# Two main operations:
# 1. Find all variables for a particular search term
# 2. Create dataframe for a list of chosen variables

load(system.file("data", "current.RData", package="alspac"))
load(system.file("data", "useful.RData", package="alspac"))


#' Guess the default data directory
#'
#' The R drive will be mounted in different paths for different systems.
#' This function guesses the path to be used as default in setDataDir
#' 
#' @export
#' @return NULL
#' @examples \dontrun{
#'
#'}
getDefaultDataDir <- function()
{

	d <- switch(Sys.info()['sysname'],
		Darwin = "/Volumes/data/",
		Linux = "~/.gvfs/data on is-socmed.isys.bris.ac.uk/",
		Windows = "R:/Data/"
	)
	return(d)
}


#' Set the data directory
#'
#' This function is automatically called upon loading the package through `library(alspac)`
#' It creates a global option called `alspac_data_dir` which is used by the \code{extractVars}
#' function to locate the alspac data files.
#' This function guesses the path to be used as default in setDataDir. The defaults are:
#' \itemize{
#' \item{Windows: R:/Data/Useful_data/current_R/}
#' \item{Mac: /Volumes/data/Useful_data/current_R/}
#' \item{Linux: ~/.gvfs/data on is-socmed.isys.bris.ac.uk/Useful_data/current_R/}
#' }
#' 
#' @param datadir The directory where the ALSPAC data can be found
#' 
#' @export
#' @return Null. Assigns the option alspac_data_dir
#' @examples \dontrun{
#' setDataDir() # This sets the path based on the operating system's default
#' setDataDir("/some/other/path/") # This is how to supply a path manually
#'}
setDataDir <- function(datadir=getDefaultDataDir())
{
	test <- file.exists(datadir)
	if(test)
	{
		cat("The data directory has been recognised\n")
	} else {
		cat("The data directory", datadir, "has NOT been found.\nIt is normally located on the remote R drive, R:/Data/.\nYou will be able to search for variables from the dictionary but unable to extract them from the data.\nPlease check that the R: drive has been mounted onto your computer through the UoB VPN\nRun setDataDir(<directory name>) to try again.\n")
	}
	options(alspac_data_dir=datadir)
}


#' Find variables
#'
#' Provide a list of search terms to find variables in the data dictionary. 
#'
#' @details The resulting data frame will have the following columns:
#' \itemize{
#' 	\item{obj - The name of the data file}
#' 	\item{name - The name of the variable}
#' 	\item{type - The type of data for the variable}
#' 	\item{lab - A description of the label}
#' 	\item{code - The ALSPAC dictionary code for the variable}
#' 	\item{counts - The number of non-NA values in the variable}
#' 	\item{cat1-4 - These columns correspond to the folder names that the objects were found in}
#' }
#' 
#' @param ... Search terms
#' @param logic Conditions for the search strings, can be "all", "any", or "none". Set to "any" by default.
#' @param ignore.case Should search terms be case sensitive? Defaults to TRUE.
#' @param perl logical.  Should perl-compatible regexps be used? Defaults to FALSE.
#' @param fixed logical.  If 'TRUE', 'pattern' is a string to be matched as is.  Overrides all conflicting arguments. Defaults to FALSE.
#' @param whole.word If 'TRUE' search term "word" will be changed to "\\bword\\b" to only match whole words. Defaults to FALSE.
#' @param dictionary Data frame of data dictionary. Defaults to "current", but "useful" is also an option - this contains the data dictionary for the R:/Data/Useful_data directory
#'
#' @export
#' @return A data frame containing a list of the variables, the files they originate from, and some descripton about the files
#' @examples \dontrun{
#' # Find variables with BMI or height in the description (this will return a lot of results!)
#' bmi_variables <- findVars("bmi", "height", logic="any", ignore.case=TRUE)
#'}
findVars <- function(..., logic="any", ignore.case=TRUE, perl=FALSE, fixed=FALSE, whole.word=FALSE, dictionary=current)
{
	l <- list(...)
	stopifnot(length(l) > 0)
	stopifnot(logic %in% c("any", "all", "none"))
	invert <- ifelse("none", TRUE, FALSE)
	if(whole.word)
	{
		l <- lapply(l, function(x) paste("\\b", x, "\\b", sep=""))
	}

	g <- list()
	n <- 1:nrow(dictionary)

	# Search for patterns in label
	for(i in 1:length(l))
	{
		g[[i]] <- grep(l[[i]], dictionary$lab, ignore.case = ignore.case, perl = perl, fixed = fixed, invert = invert)
	}

	if(logic == "any")
	{
		g <- unique(unlist(g))
	} else if(logic == "all") {
		g <- Reduce(intersect, g)
	} else if(logic == "none") {
		a <- unique(unlist(g))
		g <- n[!n %in% a]
	}
	index <- n %in% g
	return(dictionary[index, ])
}


#' Extract variables from data
#'
#' Take the output from `findVars` as a list of variables to extract from ALSPAC data
#'
#' @details There are about 130 ALSPAC data files. Given output from `findVars`, this function will 
#' retrieve all the variables from these files and collapse them into a single data frame. 
#' It will return columns for all the variables, plus columns for `aln`, `qlet` and `mult_mum` 
#' or `mult_dad` if they were present in any of the files.
#'
#' Suppose we extract a four variables, one for each of mothers, children, fathers and partners. This will return the variables requested, along with some other columns -
#'
#' - `aln` - This is the pregnancy identifier. NOTE - this is **not** an individual identifier. For example, notice that row 4 has entries for the father variable `ff1a005a`, the mother variable `fm1a010a`, and the partner variable `pc013`.
#'
#' - `qlet` - This is the child ID for the specific pregnancy. It will take values from A-D. **All** children will have a qlet, and **only** children will have a qlet. Therefore **if qlet is not NA, that row represents an individual child**.
#'
#' - `alnqlet` - this is the ALN + QLET. If the individual is a child (e.g. row 8) then they will have a different `alnqlet` compared to the `aln`. Otherwise, the `aln` is the same as the `alnqlet`
#'
#' - `mult_mum` and `mult_dad` - Sometimes the same mother (or father) had more than one pregnancy in the 18 month recruitment period. Those individuals have two ALNs. If either of these columns is "Yes" then that means you can drop them from the results if you want to avoid individuals being duplicated. This is the guidance from the FOM2 documentation:
#'
#'    1.7 Important Note for all data users:
#'    Please be aware that some women may appear in the release file more than once. This is due to the way in which women were originally enrolled into the study and were assigned IDs. ALSPAC started by enrolling pregnant women and the main study ID is a pregnancy based ID. Therefore if a women enrolled with two different pregnancies (both having an expected delivery date within the recruitment period [April 1991-December 1992]), she will have two separate IDs to uniquely identify these women and their pregnancies. An indicator variable has been included in the file, called mult_mum to identify these women. If you are carrying out mother based research that does not require you to consider repeat pregnancies for which we have data then please select mult_mum == 'No' to remove the duplicate entries. This will keep one pregnancy and randomly drop the other pregnancy. If you are matching the data included in this file to child based data or have been provided with a dataset that includes the children of the ALSPAC pregnancies, as well as the mother-based data, you need not do anything as each pregnancy (and hence each child from a separate pregnancy) has a unique identifier and a mothers’ data has been included/repeated here for each of her pregnancies where appropriate.
#'
#' The speed at which this function runs is dependent upon how fast your connection is to the R drive
#' and how many variables you are extracting at once.
#'
#' @param x Output from `findVars`
#'
#' @export
#' @return A data frame with all the variable specified in `x`
#' @examples \dontrun{
#' # Find all variables with BMI in the description
#' bmi_variables <- findVars("bmi", ignore.case=TRUE)
#' # Extract all the variables into a data.frame:
#' bmi <- extractVars(bmi_variables)
#' # Alternatively just extract the variables for adults
#' bmi <- extractVars(subset(bmi_variables, cat3 %in% c("Mother", "Adult")))
#'}
extractVars <- function(x)
{
	# require(plyr)
	# require(readstata13)
	cat("Starting extraction\n")
	variable_names <- 
	dat <- plyr::dlply(x, c("obj"), .progress="text", function(x)
	{
		x <- plyr::mutate(x)
		# Read in data
		fn <- paste0(options()$alspac_data_dir, "/", x$path[1], "/", x$obj[1])
		obj <- suppressWarnings(readstata13::read.dta13(fn))

		# Make sure aln and qlet variables are lower case
		alnc <- grep("ALN", names(obj), ignore.case=TRUE)
		if(length(alnc) == 1)
		{
			names(obj)[alnc] <- "aln"
		} else {
			stop("No ALN code in ", x$objname[1])
		}
		qletc <- grep("QLET", names(obj), ignore.case=TRUE)
		if(length(qletc) != 0)
		{
			names(obj)[qletc] <- "qlet"
		}
		# Get aln, mult and qlet variables
		ivars <- grep("(aln|mult|qlet)", names(obj), ignore.case=FALSE, value=T)
		index <- x$name %in% names(obj)
		if(!all(index == TRUE))
		{
			print(x$name)
			cat("Missing vars from ", x$objname, ":", x$name[!index], "\n")
		}
		# extract requested variables
		cvars <- names(obj)[names(obj) %in% x$name]
		vars <- unique(c(ivars, cvars))
		obj <- subset(obj, select=vars)
		# Create aln and aln2 variables
		obj$aln2 <- obj$aln
		if("qlet" %in% vars)
		{
			obj$qlet <- convertQlet(obj$qlet)
			obj$aln <- paste(obj$aln, obj$qlet, sep="")
		}
		return(obj)
	})
	cat("Collapsing data\n")
	x <- dat[[1]]
	if(length(dat) > 1)
	{
		for(i in 2:length(dat))
		{
			if("qlet" %in% names(dat[[i]]) & "qlet" %in% names(x))
			{
				x <- merge(x, dat[[i]], by=c("aln", "qlet", "aln2"), all=TRUE)
			} else {
				x <- merge(x, dat[[i]], by=c("aln", "aln2"), all=TRUE)
			}
		}
	}
	names(x)[names(x) == "aln"] <- "alnqlet"
	names(x)[names(x) == "aln2"] <- "aln"
	return(x)
}


convertQlet <- function(qlet)
{
	if(!is.factor(qlet))
	{
		qlet <- as.factor(qlet)
	}
	if(!all(levels(qlet) %in% c("A", "B", "C", "D")))
	{
		levels(qlet) <- c("A", "B", "C", "D")		
	}
	return(qlet)
}
