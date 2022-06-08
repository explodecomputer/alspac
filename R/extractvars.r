

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
#'    Please be aware that some women may appear in the release file more than once. This is due to the way in which women were originally enrolled into the study and were assigned IDs. ALSPAC started by enrolling pregnant women and the main study ID is a pregnancy based ID. Therefore if a women enrolled with two different pregnancies (both having an expected delivery date within the recruitment period [April 1991-December 1992]), she will have two separate IDs to uniquely identify these women and their pregnancies. An indicator variable has been included in the file, called mult_mum to identify these women. If you are carrying out mother based research that does not require you to consider repeat pregnancies for which we have data then please select mult_mum == 'No' to remove the duplicate entries. This will keep one pregnancy and randomly drop the other pregnancy. If you are matching the data included in this file to child based data or have been provided with a dataset that includes the children of the ALSPAC pregnancies, as well as the mother-based data, you need not do anything as each pregnancy (and hence each child from a separate pregnancy) has a unique identifier and a mothers data has been included/repeated here for each of her pregnancies where appropriate.
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
#' }
#' 
extractVars <- function(x, exclude_withdrawn = TRUE, core_only=TRUE, adult_only=FALSE, spss=FALSE, haven=F) {
    dictionaryGood(x)

    x <- unique(x)
    if (core_only) 
        x <- extractVarsCore(x, adult_only=adult_only, spss=spss, haven=haven) 
    else
        x <- extractVarsFull(x, spss=spss, haven=haven)
    
    if(exclude_withdrawn) {
        message("Automatically removing data for individuals who have withdrawn consent.")
        x <- removeExclusions(x)
    } else {        
        warning("Withdrawn consent individuals have NOT been removed. ",
                "Re-run with the default option or remove the relevant ",
                "IDs manually before proceeding with analysis.")
    }
    x
}

## restrict data extracted as in the SPSS/STATA
## scripts in R:\Data\Syntax\
extractVarsCore <- function(x, adult_only, spss=FALSE, haven=haven) {
    dat <- extractVarsFull(x,spss=spss, haven=haven)

    ##based on R:\Data\Syntax\syntax_template_3June21.do
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

    child.filters <- list(kz011b=c(obj="cp_[0-9]+[a-z]+"),
                       kz021=c(obj="cp_[0-9]+[a-z]+"),
                       kz030=c(obj="kz_[0-9]+[a-z]+"),
                       tripquad=c(obj="cp_[0-9]+[a-z]+"),
                       in_core=c(obj="cp_[0-9]+[a-z]+"),
                       in_alsp=c(obj="cp_[0-9]+[a-z]+"),
                       in_phase2=c(obj="cp_[0-9]+[a-z]+"),
                       in_phase3=c(obj="cp_[0-9]+[a-z]+"),
                       in_phase4=c(obj="cp_[0-9]+[a-z]+"))

    if (!adult_only)
        core.filters <- c(core.filters, child.filters)
    
    suppressWarnings(core.vars <- findVars(names(core.filters), dictionary="both"))
    core.vars <- core.vars[which(core.vars$name %in% names(core.filters)),]
    core.vars <- do.call(filterVars, c(list(x=core.vars), core.filters))
   
    missing.vars <- setdiff(names(core.filters), core.vars$name)
    if (length(missing.vars) > 0)
        stop("Variables required to identify core ALSPAC participants not available. Please contact maintainers. ",
             "Missing variables: ", 
             paste(missing.vars, collapse=", "))

    core.dat <- extractVarsFull(core.vars, spss=spss, haven=haven)

    if (!adult_only) {
        ## versions of R around 4.0 have a bug with spss_labelled types
    	in_alsp <- as.numeric(as.character(core.dat$in_alsp))
	tripquad <- as.numeric(as.character(core.dat$tripquad))
	
        core.dat <- core.dat[which(in_alsp == 1 & tripquad == 2),]
    }
    else  { ## adult only dataset
        ## versions of R around 4.0 have a bug with spss_labelled types
    	mz001 <- as.numeric(as.character(core.dat$mz001))
	
        core.dat <- core.dat[which(mz001 == 1),]
    }

    if ("qlet" %in% colnames(core.dat) && "qlet" %in% colnames(dat))
        dat <- dat[match(core.dat$alnqlet, dat$alnqlet),]
    else
        dat <- dat[match(core.dat$aln, dat$aln),]
    
    id.vars <- c("aln","qlet","alnqlet")
    remove.vars <- c(names(core.filters)[!grepl("^(mz|best|in_|kz)", names(core.filters))],
                     "mz010","tripquad","in_alsp")
    remove.vars <- setdiff(remove.vars, x$name)
    remove.vars <- c(id.vars, remove.vars)
    
    bind_cols(core.dat[,intersect(colnames(core.dat), id.vars), drop=F],
              dat[,setdiff(colnames(dat), remove.vars), drop=F],
              core.dat[,setdiff(colnames(core.dat), c(colnames(dat), remove.vars)), drop=F])
}




extractVarsFull <- function(x, spss=F, haven=F)
{
	# require(plyr)
	# require(readstata13)
	message("Starting extraction from ", length(unique(x$obj)), " files in the ALSPAC data directory")
	dat <- plyr::dlply(x, c("obj"), function(x) {
		x <- plyr::mutate(x)
		# Read in data
		fn <- paste0(options()$alspac_data_dir, "/", x$path[1], "/", x$obj[1])
		message("Extracting from: ", fn)
		if(!file.exists(fn)) {
                    stop(
                        fn, " does not exist. ",
                        "Please run 'updateDictionaries()' and try again. ",
                        "If you are using input from 'findVars()', ",
                        "then you will need rerun that as well. ",
                        "If the problem persists, ",
                        "please send your data query and the error message ",
                        "to the maintainer.")
		}
                if (spss) {
                    fn.sav <- sub("dta$", "sav", fn)
                    obj <- suppressWarnings(haven::read_sav(fn.sav, user_na=T))
                }
                else {
                    if (haven)
                        obj <- suppressWarnings(haven::read_dta(fn))
                    else
                        obj <- suppressWarnings(readstata13::read.dta13(fn))
                }

		# Make sure aln and qlet variables are lower case
		alnc <- grep("^ALN$", names(obj), ignore.case=TRUE)
		if(length(alnc) == 1) {
                    names(obj)[alnc] <- "aln"
		} else {
                    message("ALN codes missing or not as expected in ",
                         x$obj[1])
                    message(names(obj)[alnc])
                    stop("Please contact maintainers.")
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
                ## Create in_obj_XX variable
                objname <- sub("(.*)_.*", "\\1", basename(fn))
                objname <- sub("\\..*", "", objname)
                objname <- sub(" ", "_", objname)
                obj[[paste("in_obj", objname, sep="_")]] <- 1   
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
        if (spss)
            ids <- as_tibble(ids)
        
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
        x <- do.call(bind_cols, dat)

        ## convert 1/NA to 1/0 for all "in_obj_XX" columns and rename them "in_XX"
        is_in_obj_column <- grepl("^in_obj_", colnames(x))
        if (any(is_in_obj_column)) {
            for (i in which(is_in_obj_column))
                x[[i]] <- ifelse(is.na(x[[i]]), 0, 1)
            colnames(x)[is_in_obj_column] <- sub("_obj", "", colnames(x)[is_in_obj_column])
        }
        
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
#' 
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




