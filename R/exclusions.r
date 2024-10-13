#' Remove data for participants who have withdrawn consent.
#'
#' The exclusion lists for mothers and children are stored in .do
#' files in the R: drive. This function obtains ALNs to exclude
#' from these files and then sets the variable
#' values to missing for the appropriate participants
#' and adds indicator variables for these participants ("woc_*").
#'
#' @param x Data frame output from \code{\link{extractVars}()}.
#' 
#' @export
#' @return The input data frame but with appropriate values set to missing
#' with additional variables ("woc_*") identifying participants
#' who have withdrawn consent.
removeExclusions <- function(x) {
    stopifnot("aln" %in% names(x))

    ## obtain alns for individuals that have withdrawn consent
    withdrawals <- readExclusions()

    ## obtain dictionary corresponding the requested dataset
    dictionary <- retrieveDictionary("current")
    dictionary <- dictionary[match(colnames(x), dictionary$name),]

    ## check that exclusions information in the dictionary is up-to-date
    if(!all(names(withdrawals) %in% colnames(dictionary))) {
        stop(
            "New exclusion file(s) have been created but are not being handled here: ",
            paste(setdiff(names(withdrawals), colnames(dictionary)), collapse=", "))
    }

    for (group in names(withdrawals)) {
        sample.idx <- which(x$aln %in% withdrawals[[group]])
        if (length(sample.idx) == 0) next

        var.idx <- which(dictionary[[group]])
        if (length(var.idx) == 0) next

        x[sample.idx,var.idx] <- NA
        withdrawal.name <- paste("woc",group,sep="_")
        x[[withdrawal.name]] <- 1:nrow(x) %in% sample.idx
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
                           full.names=TRUE)
    if (length(do.files) == 0) {
        stop("do files in Syntax/Withdrawal of consent/ appear to be missing")
    }
    
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


#' Add data sources information to the dictionary
#' from the data/sources.csv file.
#' See generateSourcesSpreadsheet() for details about creating this file. 
#' This information is used when decide which data values
#' to remove for participants who have withdrawn consent.
#' @param dictionary The name of an existing dictionary or the dictionary itself.
addSourcesToDictionary <- function(dictionary) {
    ## obtain alns for individuals that have withdrawn consent
    withdrawals <- readExclusions()
    paths <- getPaths()
    ## variables that should not have values removed
    keep <- list(
        mother=c("aln","mum_and_preg_enrolled","mum_enrol_status","preg_enrol_status"),
        mother_clinic=c("aln","mum_and_preg_enrolled","mum_enrol_status","preg_enrol_status"),
        mother_quest=c("aln","mum_and_preg_enrolled","mum_enrol_status","preg_enrol_status"),
        partner_quest=c("aln","partner_in_alspac"),
        partner_clinic=c("aln","partner_in_alspac"),
        partner=c("aln","partner_in_alspac"),
        child_based=c("aln","qlet","alnqlet","in_alsp","tripquad"),
        child_completed=c("aln","qlet","alnqlet","in_alsp","tripquad"))
    if(!all(names(withdrawals) %in% names(keep))) {
        stop(
            "New exclusion file(s) have been created but are not being handled here:",
            paste(setdiff(names(withdrawals), names(paths)), collapse=", "))
    }

    sources <- utils::read.csv(system.file("data", "sources.csv", package = "alspac"), stringsAsFactors=FALSE)
    stopifnot(all(names(keep) %in% colnames(sources)))

    ## match 'sources' to 'dictionary' using the 'obj' column
    ## (dictionary$obj is a filename vector and sources$obj contains substrings
    ## of these filenames minus the version number and file extension, e.g. 
    ## if dictionary$obj has "c_2e.dat", then sources$obj will have "c_".)
    dict.obj <- unique(dictionary$obj)
    obj.idx <- lapply(sources$obj, function(obj) {
        idx <- grep(paste0("^", obj), dict.obj)
        if (length(idx) > 0) {
            matches <- dict.obj[idx]
            idx <- idx[which.min(nchar(matches))]
        }
        idx
    })
    n.matches <- sapply(obj.idx, length)
    if (all(n.matches==0)) {
        stop("The dictionary does not match anything in 'alspac::data/sources.csv'.")
    }
    sources <- sources[n.matches>0,]
    obj.idx <- unlist(obj.idx[n.matches>0])
    sources$obj <- dict.obj[obj.idx]
    for (group in names(keep)) {
        dictionary[[group]] <- sources[[group]][match(dictionary$obj, sources$obj)]
    }

    for (group in names(keep)) {
        idx <- which(dictionary$name %in% keep[[group]])
        if (length(idx) > 0) {
            dictionary[[group]][idx] <- FALSE
        }
    }
    dictionary
}


#' This function was used to initially create the data/sources.csv
#' spreadsheet which provides the source of data for each data file ('obj')
#' in the dictionary. This information is then used to determine 
#' which bits of data to remove to satisfy exclusion lists. 
#' Sources include mother, mother_clinic, mother_quest,
#' partner, partner_clinic, partner_quest,
#' child_based and child_completed.
#' Sources can for the most part be determined automatically from the 'path'
#' information provided for each variable in the dictionary.
#'
#' sources <- generateSourcesSpreadsheet()
#' utils::write.csv(sources, file="data/sources.csv", row.names=FALSE)
generateSourcesSpreadsheet <- function() {
    ## obtain alns for individuals that have withdrawn consent
    withdrawals <- readExclusions()
                    
    dictionary <- retrieveDictionary("current")
    
    ## list variable paths relevant to sources of ALSPAC data
    paths <- getPaths()
    if(!all(names(withdrawals) %in% names(paths))) {
        stop(
            "New exclusion file(s) have been created but are not being handled here":
            paste(setdiff(names(withdrawals), names(paths)), collapse=", "))
    }

    for (src in names(paths)) {
        dictionary[[src]] <- FALSE
        for (path in paths[[src]])
            dictionary[[src]] <- dictionary[[src]] | grepl(path, paste0(dictionary$path,dictionary$obj))
    }

    ## more complicated cases below
    ## 1. Longitudinal data
    is.longitudinal <- grepl("Other/Longitudinal", dictionary$path)
    is.mother <- is.longitudinal & grepl("^mlon", dictionary$obj, ignore.case=TRUE)
    dictionary[is.mother, grepl("mother", colnames(dictionary))] <- TRUE
    is.child <- is.longitudinal & (grepl("^clon", dictionary$obj, ignore.case=TRUE) | grepl("_yp_", dictionary$obj, ignore.case=TRUE))
    dictionary[is.child, grepl("child", colnames(dictionary))] <- TRUE
    ## 2. Covid data    
    is.covid <- grepl("Current/Quest/COVID", dictionary$path) 
    is.partner <- is.covid & grepl("(partner|G0dad)", dictionary$obj, ignore.case=TRUE)
    dictionary[is.partner, grepl("partner", colnames(dictionary))] <- TRUE
    is.mother <- is.covid & grepl("_(G0mum|mum)_", dictionary$obj, ignore.case=TRUE)
    dictionary[is.mother, grepl("mother", colnames(dictionary))] <- TRUE
    is.child <- is.covid & grepl("_yp_", dictionary$obj, ignore.case=TRUE)
    dictionary[is.child, grepl("child", colnames(dictionary))] <- TRUE
    ## 3. Useful data
    #is.useful <- grepl("Useful_data", dictionary$path)
    #dictionary$obj[!is.useful] <- sub("_[a-z0-9]+[.]{1}[a-z]+$", "_", dictionary$obj[!is.useful])

    dictionary <- unique(dictionary[,c("obj", "path", names(paths))])

    dictionary[order(dictionary$path),]
}

getPaths <- function()
{
    list(
        mother_clinic=c(
            "Other/Cohort Profile/mz",
            "Current/Other/Geodata/G0",
            "Other/Sample Definition/mz",
            "Other/Samples/Mother",
            "Other/Obstetric",
            "Clinic/Adult/FOM"),
        mother_quest=c(
            "Other/Cohort Profile/mz",
            "Current/Other/Geodata/G0",
            "Other/Sample Definition/mz",
            "Other/Social_Class",
            "Quest/Mother"),
        mother=c(
            "Other/Cohort Profile/mz",
            "Current/Other/Geodata/G0",
            "Other/Sample Definition/mz",
            "Other/Samples/Mother",
            "Other/Obstetric",
            "Clinic/Adult/FOM",
            "Other/Social_Class",
            "Quest/Mother"),        
        partner_quest=c(
            "Other/Cohort Profile/pz",
            "Other/Social_Class",
            "Quest/Father",
            "Quest/Partner"),
        partner_clinic=c(
            "Other/Cohort Profile/pz",
            "Other/Samples/Father",
            "Clinic/Adult/FOF"),          
        partner=c(
            "Other/Cohort Profile/pz",
            "Other/Social_Class",
            "Quest/Father",
            "Quest/Partner",
            "Other/Samples/Father",
            "Clinic/Adult/FOF"),          
        child_based=c(
            "Other/Cohort Profile/cp",
            "Current/Other/Geodata/G1",
            "Other/Sample Definition/cp",
            "Other/Sample Definition/kz",
            "Other/Obstetric",
            "Other/Samples/Child",
            "Quest/Child Based",
            "Quest/Puberty",
            "Quest/Schools",
            "Clinic/Child"),
        child_completed=c(
            "Other/Cohort Profile/cp",
            "Current/Other/Geodata/G1",
            "Other/Sample Definition/cp",
            "Other/Sample Definition/kz",
            "Other/Obstetric",
            "Other/Samples/Child",
            "Quest/Child Completed",
            "Quest/Puberty",
            "Quest/Schools",
            "Clinic/Child"))
}
