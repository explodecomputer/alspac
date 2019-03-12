# Functions for querying the ALSPAC variables data dictionary
# Two main operations:
# 1. Find all variables for a particular search term
# 2. Create dataframe for a list of chosen variables


whole.word.regex <- function(x) paste("\\b", x, "\\b", sep="")

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
#' @param dictionary Data frame or name of a data dictionary. Dictionaries available by default are
#' "current" (from the R:/Data/Current folder), "useful" (from the R:/Data/Useful_data folder) and "both" (combined "current" and "useful").
#' New dictionaries can be created using the \code{\link{createDictionary}()} function. (Default: "both").
#'
#' @export
#' @return A data frame containing a list of the variables, the files they originate from, and some descripton about the files
#' @examples \dontrun{
#' # Find variables with BMI or height in the description (this will return a lot of results!)
#' bmi_variables <- findVars("bmi", "height", logic="any", ignore.case=TRUE)
#'}
findVars <- function(..., logic="any", ignore.case=TRUE, perl=FALSE, fixed=FALSE, whole.word=FALSE, dictionary="both")
{
        if (is.character(dictionary))
            dictionary <- retrieveDictionary(dictionary)
        
	l <- unlist(list(...))
	stopifnot(length(l) > 0)
	stopifnot(logic %in% c("any", "all", "none"))
	invert <- ifelse("none", TRUE, FALSE)
	if(whole.word)
	{
		l <- lapply(l, whole.word.regex)
	}

	n <- 1:nrow(dictionary)

	# Search for patterns in label
        g <- lapply(l, function(l){
            c(grep(l, dictionary$lab, ignore.case = ignore.case, perl = perl, fixed = fixed, invert = invert),
              grep(whole.word.regex(l), dictionary$name, ignore.case = ignore.case, perl = perl, fixed = fixed, invert = invert))
        })

	if(logic == "any")
	{
		g <- unique(unlist(g))
	} else if(logic == "all") {
		g <- Reduce(intersect, g)
	} else if(logic == "none") {
		a <- unique(unlist(g))
		g <- n[!n %in% a]
	}

	out <- dictionary[g, ]
	rownames(out) <- NULL

        ## in case two patterns identify exactly the same variable ...
        out <- unique(out)

        dictionaryGood(out)

        var.freq <- table(out$name)
        if (any(var.freq > 1))
            warning("One or more variables have the same name (fix with filterVars()): ",
                    paste(names(var.freq)[which(var.freq > 1)], collapse=", "))
	return(out)
}

#' Filter duplicate variables from findVars
#'
#' @details \code{\link{findVars}()} may identify multiple
#' variables with the same name.  This function can be used
#' to select among these duplicates.
#'
#' @param x Output from \code{\link{findVars}()}.
#' @param ... Filter terms.  The name corresponds to the variable
#' name for which to remove duplicates.  Each term is a named vector
#' whose names correspond to columns in `x`.
#' The values provide patterns for the given column to match.
#' @export
#' @return The subset of `x` that satisfies the supplied filters
#' or that were not provided a filter.
#' @examples\dontrun{
#' varnames <- c("kz021","kz011b","ype9670", "c645a")
#' vars <- findVars(varnames)
#' vars <- subset(vars, subset=tolower(name) %in% varnames)
#' vars <- filterVars(vars, kz021=c(obj="^kz"), kz011b=c(obj="^cp", lab="Participant"), c645a=c(cat2="Quest")) 
#' }
filterVars <- function(x, ...) {
    filter.list <- list(...)

    ## check that each filter name corresponds to a variable in x
    none.idx <- which(!names(filter.list) %in% x$name)
    if (length(none.idx) != 0)
        stop("Filter name(s) do not correspond to a variable name: ",
             paste(names(filter.list)[none.idx], collapse=", "))

    ## check that the column names correspond to columns in x
    columns <- unlist(lapply(filter.list, function(filter) names(filter)))
    columns <- unique(setdiff(columns, colnames(x)))
    if (length(columns) > 0)
        stop("Filter column name(s) (", paste(columns, collapse=", "), ") ", 
             "do not match columns in x (", paste(colnames(x), collapse=", "), ")")

    ## identify variables that are being filtered
    filter.idx <- which(x$name %in% names(filter.list))
    if (length(filter.idx) == 0)
        stop("None of the filter names matches a variable name")

    ## apply each variable filter
    filtered.x <- lapply(names(filter.list), function(varname) {
        ## add the variable name to the filter
        filter <- c(name=paste0("^", varname, "$"), filter.list[[varname]])
        ## identify which variable(s) satisfy the filter
        matches <- sapply(names(filter), function(column) {
            grepl(filter[[column]], x[[column]])
        })
        if (length(filter) > 1)
            matches <- apply(matches, 1, all)
        satisfies.idx <- which(matches)
        ## if no variable satisfies the filter, then issue a warning
        if (length(satisfies.idx) == 0)
            warning("Filter for ", varname, " does not match any variable")
        ## if multiple variables satisfy the filter, then issue a warning
        if (length(satisfies.idx) > 1)
            warning("Filter for ", varname, " matches multiple variables")
        ## return matching variable(s)
        x[which(matches),]
    })
    filtered.x <- do.call(rbind, filtered.x)
    rbind(x[-filter.idx,], filtered.x)
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
#' @param exclude_withdrawn Whether to automatically exclude withdrawn consent IDs. Default is TRUE.
#' This is conservative, removing all withdrawn consant ALNs from all datasets. Only use FALSE here
#' if you have a more specific list of withdrawn consent IDs for your specific variables.
#' @param core_only Whether to automatically exclude data from participants
#' not in the core ALSPAC dataset (Default: TRUE).
#' This should give the same samples as the STATA/SPSS scripts in the R:/Data/Syntax folder.
#' @param adult_only Apply the 'adult only' core restriction (i.e. 'mz001 == 1') if `adult_only==TRUE`,
#' otherwise the child core restriction is applied (i.e. 'in_alsp==1' and 'tripquad==2'). 
#' Ignored if `core_only==FALSE`. The default is `FALSE`. 
#' 
#' @export
#' @return A data frame with all the variable specified in `x`. If \code{exclude_withdrawn} was \code{TRUE}, then columns
#' named \code{withdrawn_consent_*} indicate which samples were excluded.
#' @examples \dontrun{
#' # Find all variables with BMI in the description
#' bmi_variables <- findVars("bmi", ignore.case=TRUE)
#' # Extract all the variables into a data.frame:
#' bmi <- extractVars(bmi_variables)
#' # Alternatively just extract the variables for adults
#' bmi <- extractVars(subset(bmi_variables, cat3 %in% c("Mother", "Adult")))
#'}
extractVars <- function(x, exclude_withdrawn = TRUE, core_only=TRUE, adult_only=FALSE) {
    dictionaryGood(x)

    x <- unique(x)
    if (core_only) 
        x <- extractVarsCore(x, adult_only=adult_only) 
    else
        x <- extractVarsFull(x)
    
    if(exclude_withdrawn) {
        x <- removeExclusions(x)
        message("Automatically removing all withdrawn consent ALNs from the entire dataset")
    } else {
        warning("Withdrawn consent individuals have NOT been removed. Re-run with the default option or remove the relevant IDs manually before proceeding with analysis.")
    }
    x
}

## restrict data extracted as in the SPSS/STATA
## scripts in R:\Data\Syntax\
extractVarsCore <- function(x, adult_only) {
    dat <- extractVarsFull(x)

    ##based on R:\Data\Syntax\syntax_template_12Apr18.do
    core.filters <- list(mz001=c(obj="mz_[0-9]+[a-z]+"),
                      mz010=c(obj="mz_[0-9]+[a-z]+"),
                      mz010a=c(obj="mz_[0-9]+[a-z]+"),
                      mz013=c(obj="mz_[0-9]+[a-z]+"),
                      mz014=c(obj="mz_[0-9]+[a-z]+"),
                      mz028b=c(obj="mz_[0-9]+[a-z]+"),
                      a006=c(obj="a_[0-9]+[a-z]+"),
                      a525=c(obj="a_[0-9]+[a-z]+"),
                      b032=c(obj="b_[0-9]+[a-z]+"),
                      b650=c(obj="b_[0-9]+[a-z]+"),
                      b663=c(obj="b_[0-9]+[a-z]+"),
                      b665=c(obj="b_[0-9]+[a-z]+"),
                      b667=c(obj="b_[0-9]+[a-z]+"),
                      c645a=c(obj="c_[0-9]+[a-z]+"),
                      c755=c(obj="c_[0-9]+[a-z]+"),
                      c765=c(obj="c_[0-9]+[a-z]+"),
                      c800=c(obj="c_[0-9]+[a-z]+"),
                      c801=c(obj="c_[0-9]+[a-z]+"),
                      c802=c(obj="c_[0-9]+[a-z]+"),
                      c803=c(obj="c_[0-9]+[a-z]+"),
                      c804=c(obj="c_[0-9]+[a-z]+"),
                      bestgest=c(obj="bestgest"))

    child.filters <- list(kz011b=c(obj="kz_[0-9]+[a-z]+"),
                       kz021=c(obj="kz_[0-9]+[a-z]+"),
                       kz030=c(obj="kz_[0-9]+[a-z]+"),
                       tripquad=c(obj="cp_[0-9]+[a-z]+"),
                       in_core=c(obj="cp_[0-9]+[a-z]+"),
                       in_alsp=c(obj="cp_[0-9]+[a-z]+"),
                       in_phase2=c(obj="cp_[0-9]+[a-z]+"),
                       in_phase3=c(obj="cp_[0-9]+[a-z]+"))

    if (!adult_only)
        core.filters <- c(core.filters, child.filters)
    
    suppressWarnings(core.vars <- findVars(names(core.filters), dictionary="both"))
    core.vars <- core.vars[which(core.vars$name %in% names(core.filters)),]
    core.vars <- do.call(filterVars, c(list(x=core.vars), core.filters))
   
    missing.vars <- setdiff(names(core.filters), core.vars$name)
    if (length(missing.vars) > 0)
        stop("Variables required to identify core ALSPAC participants not available. ",
             "Missing variables: ", 
             paste(missing.vars, collapse=", "))

    core.dat <- extractVarsFull(core.vars)

    if (!adult_only) 
        core.dat <- core.dat[which(core.dat$in_alsp == 1 & core.dat$tripquad == 2),]
    else  ## adult only dataset
        core.dat <- core.dat[which(core.dat$mz001 == 1),]

    if ("qlet" %in% colnames(core.dat) && "qlet" %in% colnames(dat))
        dat <- dat[match(core.dat$alnqlet, dat$alnqlet),]
    else
        dat <- dat[match(core.dat$aln, dat$aln),]
    
    id.vars <- c("aln","qlet","alnqlet")
    cbind(core.dat[,intersect(colnames(core.dat), id.vars), drop=F],
          dat[,setdiff(colnames(dat), id.vars), drop=F],
          core.dat[,setdiff(colnames(core.dat), c(colnames(dat), id.vars)), drop=F])
}


#' Remove data for participants who have withdrawn consent.
#'
#' The exclusion lists for mothers and children are stored in .do
#' files in the R: drive. This function obtains ALNs to exclude
#' from these files and then sets the variable
#' values to missing for the appropriate participants
#' and adds indicator variables for these participants ("withdrawn_consent_*").
#'
#' @param x Data frame output from \code{\link{extractVars}()}.
#' 
#' @export
#' @return The input data frame but with appropriate values set to missing
#' with additional variables ("withdrawn_consent_*") identifying participants
#' who have withdrawn consent.
removeExclusions <- function(x) {
    ## colnames(x) <- tolower(colnames(x))
    
    stopifnot("aln" %in% names(x))
    
    withdrawals <- readExclusions()
        
    ##exclude aln and qlet and cohort variables from removal,
    ##these are used for sample size selection (e.g. subset data)
    keep <- c("aln","qlet","alnqlet","in_alsp", "in_core",
              "in_phase2", "in_phase3", "tripquad", "kz011b",
              "kz021","mz001")

    varnames <- colnames(x)
    dictionary <- retrieveDictionary("both")

    ## some variables appear multiple times.
    ## when it appears in 'Current' and 'Useful_data',
    ## assume that the variable is 'Current' is implied.
    dictionary <- dictionary[order(sign(dictionary$cat1 != "Current")),,drop=F]
    dictionary <- dictionary[match(varnames, dictionary$name),]    
    
    paths <- list(mother_clinic=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Sample Definition",
                      "Other/Samples/Mother",
                      "Other/Obstetric",
                      "Clinic/Adult"),
                  mother_quest=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Sample Definition",
                      "Other/Social_Class",
                      "Quest/Mother"),
                  partner_quest=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Social_Class",
                      "Quest/Father"),
                  partner_clinic=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Samples/Father",
                      "Clinic/Adult"),		  
		  partner=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Social_Class",
                      "Quest/Father",
                      "Other/Samples/Father",
                      "Clinic/Adult"),		  
                  child_based=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Sample Definition",
                      "Other/Obstetric",
                      "Other/Samples/Child",
                      "Quest/Child Based",
                      "Quest/Puberty",
                      "Clinic/Child"),
                  child_completed=c("Useful_data",
                      "Other/Cohort Profile",
                      "Other/Sample Definition",
                      "Other/Obstetric",
                      "Other/Samples/Child",
                      "Quest/Child Completed",
                      "Quest/Puberty",
                      "Clinic/Child"))
    stopifnot(all(names(withdrawals) %in% names(paths)))

    varnames <- lapply(paths, function(paths) {
        idx <- unlist(lapply(paths, function(path) grep(path, dictionary$path)))
        setdiff(dictionary$name[unique(idx)], keep)
    })

    for (group in names(varnames)) {
        sample.idx <- which(x$aln %in% withdrawals[[group]])
        if (length(sample.idx) == 0) next
        
        var.idx <- which(colnames(x) %in% varnames[[group]])
        x[sample.idx, var.idx] <- NA
        x[[paste("withdrawn","consent",group,sep=".")]] <- x$aln %in% withdrawals[[group]]
    }
    x
}

#' Get list of ALNs to exclude
#'
#' The exclusion lists for mothers and children are stored in .do
#' files in the R: drive. This function reads all of these .do files
#' and then parses out the ALNS for withdrawn consent.
#' 
#' @export
#' @return List of ALNs for each .do file.
readExclusions <- function() {
    do.files <- list.files(file.path(options()$alspac_data_dir, "Syntax/Withdrawal of consent"),
                           pattern=".do$",
                           full.names=T)
    if (length(do.files) == 0)
        stop("do files in Syntax/Withdrawal of consent/ appear to be missing")
    
    names(do.files) <- sub("_WoC.do", "", basename(do.files))
    lapply(do.files, function(file) {
        scan(file, what=character(), quiet=TRUE) %>%
        paste(collapse=" ") %>%
        stringr::str_extract_all("aln *== *[0-9]+") %>%
        unlist() %>%
        stringr::str_replace_all(" ", "") %>%
        stringr::str_replace_all("aln==", "") %>%
        as.integer %>% unique
    })
}



extractVarsFull <- function(x)
{
	# require(plyr)
	# require(readstata13)
	message("Starting extraction from ", length(unique(x$obj)), " files in the ALSPAC data directory")
	dat <- plyr::dlply(x, c("obj"), function(x) {
		x <- plyr::mutate(x)
		# Read in data
		fn <- paste0(options()$alspac_data_dir, "/", x$path[1], "/", x$obj[1])
		message("Extracting from: ", fn)
		if(!file.exists(fn))
		{
			message("PROBLEM: ", fn, " does not exist. Either the dictionary is out of date, in which case please contact the maintainer and ask for it to be updated; or the input to this function was generated in an older version of the package and will need to be regenerated.")
			message("Skipping...")
			return(NULL)
		}
		obj <- suppressWarnings(readstata13::read.dta13(fn))

		# Make sure aln and qlet variables are lower case
		alnc <- grep("^ALN$", names(obj), ignore.case=TRUE)
		if(length(alnc) == 1)
		{
			names(obj)[alnc] <- "aln"
		} else {
			message("ALN codes missing or not as expected in ", x$obj[1])
			message(names(obj)[alnc])
			message("Please contact maintainers. Skipping...")
			return(NULL)
		}
		qletc <- grep("^QLET$", names(obj), ignore.case=TRUE)
		if(length(qletc) != 0)
		{
			names(obj)[qletc] <- "qlet"
		}
		# Get aln, mult and qlet variables
		ivars <- grep("^(aln|mult|qlet)$", names(obj), ignore.case=FALSE, value=T)
		index <- x$name %in% names(obj)
		if(!all(index == TRUE))
		{
			print(x$name)
			message("Missing vars from ", x$obj, ":", x$name[!index], "\n")
		}
		# extract requested variables
		cvars <- names(obj)[names(obj) %in% x$name]
		vars <- unique(c(ivars, cvars))
		obj <- subset(obj, select=vars)
		# Create aln and aln2 variables
		obj$aln2 <- obj$aln
		if("qlet" %in% vars)
		{
			obj$qlet <- alspac:::convertQlet(obj$qlet)
			obj$aln <- paste(obj$aln, obj$qlet, sep="")
		}
		return(obj)
	})
	message("Collapsing data")
	dat <- Filter(Negate(is.null), dat)
	if(length(dat) == 0)
	{
		message("No data found")
		return(NULL)
	}

        ## create a complete id set ids=aln/aln2/[optional]qlet
        aln2 <- unique(unlist(lapply(dat, function(dat) dat$aln2)))
        aln <- unique(unlist(lapply(dat, function(dat) dat$aln)))
        if (all(aln %in% aln2))
            ## includes only mothers and/or partners
            ids <- data.frame(aln2=aln2, aln=aln2, stringsAsFactors=F)
        else {
            ## includes young people
            aln <- setdiff(aln, aln2)
            ids <- data.frame(aln2=as.integer(sub("[A-Z]+","",aln)),
                              aln=as.character(aln),
                              qlet=sub("[0-9]+","",aln),
                              stringsAsFactors=F)
            aln2 <- setdiff(aln2,ids$aln2)
            if (length(aln2) > 0)
                ## includes some mothers with no young people
                ids <- rbind(ids,
                             data.frame(aln2=aln2,
                                        aln=aln2,
                                        qlet=NA,
                                        stringsAsFactors=F))
        }
        
        ## merge ids and dat into a single data frame
        dat <- lapply(dat, function(dat) {
            if ("qlet" %in% colnames(dat))
                row.idx <- match(ids$aln, dat$aln)
            else
                row.idx <- match(ids$aln2, dat$aln2)
            col.idx <- which(!(colnames(dat) %in% c("aln","qlet","aln2")))
            dat[row.idx,col.idx,drop=F]
        })
        dat <- c(list(ids), dat)
        names(dat) <- NULL
        x <- do.call(cbind, dat)
        
	names(x)[names(x) == "aln"] <- "alnqlet"
	names(x)[names(x) == "aln2"] <- "aln"
        rownames(x) <- NULL
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


#' Extract variables exported from the ALSPAC variable lookup web app
#'
#' The variable lookup webapp allows you to browse the available
#' variables and export a list of selected variables.
#' This function will read that exported list and extract the individual
#' level data for each of the selected variables.
#' 
#' More generally, this function requires a file that has at least one 
#' column with the header 'Variable' followed by a list of variable names.
#' 
#' The R: drive must be mounted and its path set with the \code{setDataDir} function.
#'
#' @param filename Name of file exported from ALSPAC variable lookup web app
#'
#' @export
#' @return Data frame
extractWebOutput <- function(filename)
{
	input <- read.csv(filename)
	if(names(input)[1] != "Variable")
	{
		stop("The first column in ", filename, " should be names 'Variable'. Make sure this file has been exported from the ALSPAC variable lookup webapp.")
	}
	if(nrow(input) == 0)
	{
		stop("No variables present in ", filename)
	}
        
        l <- retrieveDictionary("both")
	l <- subset(l, name %in% input$Variable)

	if(nrow(l) != 0)
	{
		out <- extractVars(l)
		return(out)
	} else {
		stop("None of the variables in ", filename, " were in either the current or useful_data dictionaries")
	}
	return(l)
}




