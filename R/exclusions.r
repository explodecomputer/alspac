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
    stopifnot("aln" %in% names(x))

    ## obtain alns for individuals that have withdrawn consent
    withdrawals <- readExclusions()
                    
    dictionary <- retrieveDictionary("both")
    ## some variables appear multiple times.
    ## when it appears in 'Current' and 'Useful_data',
    ## assume that the variable is 'Current' is implied.
    dictionary <- dictionary[order(sign(dictionary$cat1 != "Current")),,drop=F]
    
    ## subset dictionary to variables requested
    varnames <- colnames(x)
    dictionary <- dictionary[match(varnames, dictionary$name),]    

    ## list variable paths relevant to sources of ALSPAC data
    paths <- list(
        mother_clinic=c(
            "Useful_data",
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
            "Quest/Father",
            "Quest/Partner"),
        partner_clinic=c("Useful_data",
            "Other/Cohort Profile",
            "Other/Samples/Father",
            "Clinic/Adult"),		  
        partner=c("Useful_data",
            "Other/Cohort Profile",
            "Other/Social_Class",
            "Quest/Father",
            "Quest/Partner",
            "Other/Samples/Father",
            "Clinic/Adult"),		  
        child_based=c("Useful_data",
            "Other/Cohort Profile",
            "Other/Sample Definition",
            "Other/Obstetric",
            "Other/Samples/Child",
            "Quest/Child Based",
            "Quest/Puberty",
            "Quest/Schools",
            "Clinic/Child"),
        child_completed=c("Useful_data",
            "Other/Cohort Profile",
            "Other/Sample Definition",
            "Other/Obstetric",
            "Other/Samples/Child",
            "Quest/Child Completed",
            "Quest/Puberty",
            "Quest/Schools",
            "Clinic/Child"))
    
    ##exclude some variables from exclusion removal 
    keep <- list(
        mother_clinic=c("aln","mz001"),
        mother_quest=c("aln","mz001"),
        partner_quest=c("aln","mz001"),
        partner_clinic=c("aln","mz001"),
        partner=c("aln","mz001"),
        child_based=c("aln","qlet","alnqlet","in_alsp",
            "tripquad","in_phase4"),
        child_completed=c("aln","qlet","alnqlet"))
    
    ## check if paths in 'dictionary' are covered in the 'paths' list,
    is.known.path <- sapply(
        unique(na.omit(dictionary$path)),
        function(path) {
            any(sapply(unique(unlist(paths)),grepl,path))
        })
    
    ## any variable path has not been handled (i.e. in 'paths')
    ## or a withdrawal group is not handled (i.e. in 'paths' or 'keep'
    ## then fail, the code needs to be updated to handle
    ## withdrawals.
    if (!all(is.known.path)
        || !all(names(withdrawals) %in% names(paths))
        || !all(names(withdrawals) %in% names(keep)))
        warning("Withdrawal code is out of date. ",
                "Contact package authors to update withdrawal of consent.")
    
    ## make a list of variables for each source of ALSPAC data
    ## that should have values removed
    varnames <- sapply(names(paths), function(group) {
        idx <- lapply(
            paths[[group]],
            function(path) grep(path, dictionary$path))
        idx <- unique(unlist(idx))
        setdiff(dictionary$name[idx], keep[[group]])
    }, simplify=F)

    ## remove values for appropriate variables
    for (group in names(varnames)) {
        sample.idx <- which(x$aln %in% withdrawals[[group]])
        if (length(sample.idx) == 0) next
        
        var.idx <- which(colnames(x) %in% varnames[[group]])
        x[sample.idx, var.idx] <- NA
        withdrawal.name <- paste("withdrawn","consent",group,sep="_")
        x[[withdrawal.name]] <- x$aln %in% withdrawals[[group]]
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

