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
#' This should give the same samples as the Stata/SPSS scripts in the R:/Data/Syntax folder.
#' @param adult_only No longer supported. Parent-specific restrictions are applied
#' automatically when child-based or child-completed variables are not requested.
#' @param spss Logical. Default \code{FALSE}.
#' @param haven Logical. Default \code{FALSE}.
#' @export
#' @return A data frame with all the variable specified in `x`. If \code{exclude_withdrawn} was \code{TRUE}, then columns
#' named \code{woc_*} indicate which samples were excluded.
#' @examples \dontrun{
#' # Find all variables with BMI in the description
#' bmi_variables <- findVars("bmi", ignore.case=TRUE)
#' # Extract all the variables into a data.frame:
#' bmi <- extractVars(bmi_variables)
#' # Alternatively just extract the variables for adults
#' bmi <- extractVars(subset(bmi_variables, cat3 %in% c("Mother", "Adult")))
#' }
#' 
extractVars <- function(x, exclude_withdrawn = TRUE, core_only=TRUE, adult_only=FALSE, spss=FALSE, haven=FALSE) {
    dictionaryGood(x)

    if (adult_only) 
        warning("'adult_only' is no longer supported. Parent-specific restrictions are applied automatically when child-based or child-completed variables are not requested.")

    x <- unique(x)
    if (core_only) 
        x <- extractVarsCore(x, spss=spss, haven=haven) 
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

## restrict data extracted as in the SPSS/Stata
## scripts in R:\Data\Syntax\
extractVarsCore <- function(x, spss=FALSE, haven=haven) {
    dat <- extractVarsFull(x,spss=spss, haven=haven)

    ## return TRUE for each row in x iff that row contains at least one TRUE
    any.row <- function(x) {
        rowSums(as.matrix(x),na.rm=TRUE) > 0
    }    
    var.has.mother.data <- any.row(x[,grepl("^mother",colnames(x))])
    var.has.partner.data <- any.row(x[,grepl("^partner",colnames(x))])
    var.has.child.data <- any.row(x[,grepl("^child",colnames(x))])
    
    ##based on R:\Data\Syntax\syntax_template_04Nov22.do
    mz.obj.pat <- c(obj="mz_[0-9]+[a-z]+",path="Current/Other/Cohort Profile/")
    core.filters <- list(
        mz010a=mz.obj.pat,
        preg_in_alsp=mz.obj.pat,
        preg_in_core=mz.obj.pat,
        preg_enrol_status=mz.obj.pat,
        mum_enrol_status=mz.obj.pat,
        mum_and_preg_enrolled=mz.obj.pat,
        mz005l=mz.obj.pat,
        mz005m=mz.obj.pat,
        mz013=mz.obj.pat,
        mz014=mz.obj.pat,
        mz028b=mz.obj.pat,
        ## the following no longer included by default 
        #a006=c(obj="a_[0-9]+[a-z]+"),
        #a525=c(obj="a_[0-9]+[a-z]+"),
        #b032=c(obj="b_[0-9]+[a-z]+"),
        #b650=c(obj="b_[0-9]+[a-z]+"),
        #b663=c(obj="b_[0-9]+[a-z]+"),
        #b665=c(obj="b_[0-9]+[a-z]+"),
        #b667=c(obj="b_[0-9]+[a-z]+"),
        #c645a=c(obj="c_[0-9]+[a-z]+"),
        #c755=c(obj="c_[0-9]+[a-z]+"),
        #c765=c(obj="c_[0-9]+[a-z]+"),
        bestgest=mz.obj.pat)

    mother.filters <- list(
        mum_in_alsp=mz.obj.pat,
        mum_in_core=mz.obj.pat)
    if (any(var.has.mother.data))
        core.filters <- c(core.filters, mother.filters)

    pz.obj.pat <- c(obj="pz_[0-9]+[a-z]+",path="Current/Other/Cohort Profile/")
    partner.filters <- list(
        partner_in_alspac=pz.obj.pat,
        partner_data=pz.obj.pat,
        partner_enrolled=pz.obj.pat,
        partner_in_core=pz.obj.pat,
        pz_mult=pz.obj.pat,
        pz_multid=pz.obj.pat,
        partner_changed=pz.obj.pat,
        partner_changed_when=pz.obj.pat,
        partner_age=pz.obj.pat,
        second_partner_age=pz.obj.pat)
    if (any(var.has.partner.data))
        core.filters <- c(core.filters, partner.filters)
    
    cp.obj.pat <- c(obj="cp_[0-9]+[a-z]+",path="Current/Other/Cohort Profile/")
    child.filters <- list(
        kz011b=cp.obj.pat,
        kz021=cp.obj.pat,
        kz030=cp.obj.pat,
        in_core=cp.obj.pat,
        in_alsp=cp.obj.pat,
        in_phase2=cp.obj.pat,
        in_phase3=cp.obj.pat,
        in_phase4=cp.obj.pat,
        tripquad=cp.obj.pat)
    if (any(var.has.child.data))
        core.filters <- c(core.filters, child.filters)
    
    suppressWarnings(core.vars <- findVars(names(core.filters), dictionary="current"))
    core.vars <- core.vars[which(core.vars$name %in% names(core.filters)),]
    core.vars <- do.call(filterVars, c(list(x=core.vars), core.filters))
   
    missing.vars <- setdiff(names(core.filters), core.vars$name)
    if (length(missing.vars) > 0)
        stop("Variables required to identify core ALSPAC participants not available. Please contact maintainers. ",
             "Missing variables: ", 
             paste(missing.vars, collapse=", "))

    core.dat <- extractVarsFull(core.vars, spss=spss, haven=haven)
    
    if (any(var.has.child.data)) {
    	in_alsp <- as.numeric(as.character(core.dat$in_alsp))   
	tripquad <- as.numeric(as.character(core.dat$tripquad)) 
        core.dat <- core.dat[which(in_alsp == 1 & tripquad == 2),]
    } else {
        mum_enrol_status <- as.numeric(as.character(core.dat$mum_enrol_status))
        mum_and_preg_enrolled <- as.numeric(as.character(core.dat$mum_and_preg_enrolled)) 
        core.dat <- core.dat[which(mum_enrol_status %in% 1:2 & mum_and_preg_enrolled == 1),]
    }

    if ("qlet" %in% colnames(core.dat) && "qlet" %in% colnames(dat))
        dat <- dat[match(core.dat$alnqlet, dat$alnqlet),]
    else
        dat <- dat[match(core.dat$aln, dat$aln),]

    if (any(var.has.partner.data)) {
        partner_in_alspac <- as.numeric(as.character(core.dat$partner_in_alspac)) 
        remove.idx <- which(partner_in_alspac==0)
        for (varname in x$name[var.has.partner.data])
            dat[remove.idx,varname] <- NA
    }
    
    id.vars <- intersect(c("aln","qlet","alnqlet"),colnames(core.dat))
    remove.vars <- c("tripquad","in_alsp")
    data.vars <- setdiff(colnames(dat),c(colnames(core.dat),remove.vars))
    admin.vars <- setdiff(colnames(core.dat),c(id.vars, remove.vars))

    dplyr::bind_cols(
        core.dat[,id.vars],
        dat[,data.vars],
        core.dat[,admin.vars])
}




extractVarsFull <- function(x, spss=FALSE, haven=FALSE)
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
                    obj <- suppressWarnings(haven::read_sav(fn.sav, user_na=TRUE))
                } else {
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
		ivars <- grep("^(aln|mult|qlet)$", names(obj), ignore.case=FALSE, value=TRUE)
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
			obj$qlet <- convertQlet(obj$qlet)
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
        if (all(aln %in% aln2)) {
            ## includes only mothers and/or partners
            ids <- data.frame(aln2=aln2, aln=aln2, stringsAsFactors=FALSE)
        } else {
            ## includes young people
            aln <- setdiff(aln, aln2)
            ids <- data.frame(aln2=as.integer(sub("[A-Z]+","",aln)),
                              aln=as.character(aln),
                              qlet=sub("[0-9]+","",aln),
                              stringsAsFactors=FALSE)
            aln2 <- setdiff(aln2,ids$aln2)
            if (length(aln2) > 0)
                ## includes some mothers with no young people
                ids <- rbind(ids,
                             data.frame(aln2=aln2,
                                        aln=aln2,
                                        qlet=NA,
                                        stringsAsFactors=FALSE))
        }
        if (spss)
            ids <- tibble::as_tibble(ids)
        
        ## merge ids and dat into a single data frame
        dat <- lapply(dat, function(dat) {
            if ("qlet" %in% colnames(dat))
                row.idx <- match(ids$aln, dat$aln)
            else
                row.idx <- match(ids$aln2, dat$aln2)
            col.idx <- which(!(colnames(dat) %in% c("aln","qlet","aln2")))
            dat[row.idx,col.idx,drop=FALSE]
        })
        dat <- c(list(ids), dat)
        names(dat) <- NULL
        x <- do.call(dplyr::bind_cols, dat)

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
	return(as.data.frame(x))
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
	input <- utils::read.csv(filename)
	if(names(input)[1] != "Variable")
	{
		stop("The first column in ", filename, " should be names 'Variable'. Make sure this file has been exported from the ALSPAC variable lookup webapp.")
	}
	if(nrow(input) == 0)
	{
		stop("No variables present in ", filename)
	}
        
        l <- retrieveDictionary("current")
	l <- subset(l, name %in% input$Variable)

	if(nrow(l) != 0)
	{
		out <- extractVars(l)
		return(out)
	} else {
		stop("None of the variables in ", filename, " were in the 'current' dictionary")
	}
	return(l)
}




