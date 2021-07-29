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

    ## obtain dictionary corresponding the requested dataset
    dictionary <- retrieveDictionary("both")
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
        withdrawal.name <- paste("withdrawn","consent",group,sep="_")
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


#' Add data sources information to the dictionary
#' from the data/sources.csv file.
#' See generateVariableSources() for details about creating this file. 
#' This information is used when decide which data values
#' to remove for participants who have withdrawn consent.
addSourcesToDictionary <- function(dictionary) {
    ## obtain alns for individuals that have withdrawn consent
    withdrawals <- readExclusions()

    ## variables that should not have values removed
    keep <- list(
        mother_clinic=c("aln","mz001"),
        mother_quest=c("aln","mz001"),
        partner_quest=c("aln","mz001"),
        partner_clinic=c("aln","mz001"),
        partner=c("aln","mz001"),
        child_based=c("aln","qlet","alnqlet","in_alsp",
            "tripquad","in_phase4"),
        child_completed=c("aln","qlet","alnqlet"))
    if(!all(names(withdrawals) %in% names(keep))) {
        stop(
            "New exclusion file(s) have been created but are not being handled here:",
            paste(setdiff(names(withdrawals), names(paths)), collapse=", "))
    }

    sources <- read.csv(system.file("data", "sources.csv", package = "alspac"), stringsAsFactors=F)
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
    if (all(n.matches==0))
        stop("The dictionary does not match anything in 'alspac::data/sources.csv'.")
    sources <- sources[n.matches>0,]
    obj.idx <- unlist(obj.idx[n.matches>0])
    sources$obj <- dict.obj[obj.idx]
    for (group in names(keep))
        dictionary[[group]] <- sources[[group]][match(dictionary$obj, sources$obj)]        

    for (group in names(keep)) {
        idx <- which(dictionary$name %in% keep[[group]])
        if (length(idx) > 0)
            dictionary[[group]][idx] <- FALSE
    }
    dictionary
}


#' This function was used to initially create the data/sources.csv
#' spreadsheet which provides the source of data for each data file ('obj')
#' in the dictionary. This information is then used to determine 
#' which bits of data to remove to satisfy exclusion lists. 
#' Sources include mother_clinic, mother_quest, partner, partner_clinic, partner_quest,
#' child_based and child_completed.
#' Sources can for the most part be determined automatically from the 'path'
#' information provided for each variable in the dictionary.
#' However, for variables in "Useful_data", the source isn't always clear 
#' so needs to be identified manually.
#'
#' sources <- generateSourcesSpreadsheet()
#' write.csv(sources, file="data/sources.csv", row.names=F)
generateSourcesSpreadsheet <- function() {
    ## obtain alns for individuals that have withdrawn consent
    withdrawals <- readExclusions()
                    
    dictionary <- alspac:::retrieveDictionary("both")
    
    ## list variable paths relevant to sources of ALSPAC data
    paths <- list(
        mother_clinic=c(
            "Other/Sample Definition",
            "Other/Samples/Mother",
            "Other/Obstetric",
            "Clinic/Adult"),
        mother_quest=c(
            "Other/Sample Definition",
            "Other/Social_Class",
            "Quest/Mother"),
        partner_quest=c(
            "Other/Social_Class",
            "Quest/Father",
            "Quest/Partner"),
        partner_clinic=c(
            "Other/Samples/Father",
            "Clinic/Adult"),          
        partner=c(
            "Other/Social_Class",
            "Quest/Father",
            "Quest/Partner",
            "Other/Samples/Father",
            "Clinic/Adult"),          
        child_based=c(
            "Other/Sample Definition",
            "Other/Obstetric",
            "Other/Samples/Child",
            "Quest/Child Based",
            "Quest/Puberty",
            "Quest/Schools",
            "Clinic/Child"),
        child_completed=c(
            "Other/Sample Definition",
            "Other/Obstetric",
            "Other/Samples/Child",
            "Quest/Child Completed",
            "Quest/Puberty",
            "Quest/Schools",
            "Clinic/Child"))
    if(!all(names(withdrawals) %in% names(paths))) {
        stop(
            "New exclusion file(s) have been created but are not being handled here":
            paste(setdiff(names(withdrawals), names(paths)), collapse=", "))
    }

    paths.for.all <- c(
        "Other/Cohort Profile",
        "Current/Other/Geodata")
    for (src in names(paths))
        paths[[src]] <- c(paths[[src]], paths.for.all)

    for (src in names(paths)) {
        dictionary[[src]] <- F
        for (path in paths[[src]])
            dictionary[[src]] <- dictionary[[src]] | grepl(path, dictionary$path)
    }

    is.covid <- grepl("Current/Quest/COVID", dictionary$path) 
    is.partner <- is.covid & grepl("partner", dictionary$obj, ignore.case=T)
    dictionary[is.partner, grepl("partner", colnames(dictionary))] <- T
    is.mother <- is.covid & grepl("_mum_", dictionary$obj, ignore.case=T)
    dictionary[is.mother, grepl("mother", colnames(dictionary))] <- T
    is.child <- is.covid & grepl("_yp_", dictionary$obj, ignore.case=T)
    dictionary[is.child, grepl("child", colnames(dictionary))] <- T

    is.useful <- grepl("Useful_data", dictionary$path)
    dictionary$obj[!is.useful] <- sub("_[a-z0-9]+[.]{1}[a-z]+$", "_", dictionary$obj[!is.useful])

    dictionary <- unique(dictionary[,c("obj", "path", names(paths))])

    dictionary[order(dictionary$path),]
}

