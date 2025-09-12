  loadDictionaries <- function() {        
    path <- file.path(system.file(package = "alspac"), "data")
    
    # Initialize globals in the global environment
    if (!exists("globals")) {
      assign("globals", new.env(), envir = .GlobalEnv)
    }
    
    for (file in list.files(path, "rdata$", full.names = TRUE))
      load(file, globals)
    combineDictionaries()
    checkDictionaries()
  }


combineDictionaries <- function() {
  both <- NULL
  
  # Check if "current" exists
  if (exists("current", envir=globals)) {
    both <- retrieveDictionary("current")
  } else {
    # Handle the case when "current" doesn't exist
    warning("Dictionary 'current' does not exist. Please run 'updateDictionaries()' to create it.")
    return(NULL)
  }  
  
  # Check if "custom" exists
  if (exists("custom", envir=globals)) {
    custom <- retrieveDictionary("custom")
    both <- plyr::rbind.fill(both, custom)
    assign("both", both, globals)
  } else {
    warning("Dictionary 'custom' does not exist.")
  }
}


retrieveDictionary <- function(name) {
    if (name %in% ls(envir=globals)) {
        get(name, envir=globals)
    } else {
        stop("dictionary '", name, "' does not exist")
    }
}

saveDictionary <- function(name, dictionary) {
    assign(name, dictionary, globals)
    #if (name == "current" || name == "useful")
    #    combineDictionaries()
    
    path <- file.path(system.file(package="alspac"), "data")
    if (!file.exists(path)) {
        dir.create(path)
    }
    save(list=name,
         file=file.path(path, paste(name, "rdata", sep=".")),
         envir=globals)
}

#' Checks a dictionary
#'
#' Checks if all the files referred to in the dictionary
#' are accessible given the ALSPAC data directory.
#'
#' @param dictionary The name of an existing dictionary or the dictionary itself.
#' @param max.print The maximum number of missing files to list if any are missing
#' (Default: 10).
#' @export
#' @return \code{TRUE} if all files exist, otherwise \code{FALSE} and a warning listing at most
#' \code{max.print} missing files.
#' 
dictionaryGood <- function(dictionary, max.print=10) {
    if (is.character(dictionary)) {
        dictionary <- retrieveDictionary(dictionary)
    }
    
    alspacdir <- options()$alspac_data_dir

    filenames <- unique(with(dictionary, file.path(alspacdir, path, obj)))
    missing.idx <- which(!sapply(filenames, file.exists))
    num.missing <- length(missing.idx)
    if (num.missing == 0) {
        TRUE
    } else {
        missing.idx <- missing.idx[1:min(max.print,num.missing)]
        warning("Please run 'updateDictionaries()' and try again. ",
                "If you are using input from 'findVars()', ",
                "then you will need to rerun that as well. ",
                "Dictionary refers to missing files, e.g. ",
                paste(filenames[missing.idx], collapse=", "))
        FALSE
    }
}


#' Update dictionaries
#'
#' Update the variable dictionaries for the ALSPAC dataset.
#' 
#' @export
updateDictionaries <- function() {
    createDictionary("Current", name="current", quick=FALSE)
    #createDictionary("Useful_data", name="useful", quick=FALSE)
    return(TRUE)
}


#' Create a dictionary from ALSPAC Stata files
#'
#' @param datadir ALSPAC data subdirectory from which to create the index
#' (Default: "Current"). .
#' @param name If not \code{NULL}, then the resulting dictionary
#' will be saved to a file in the R package for use next time the package
#' is loaded. The dictionary will be available with the given name (Default: \code{NULL}).
#' @param quick Logical. Default \code{FALSE}.
#'
#' The function uses multiple processors using \code{\link[parallel]{mclapply}()}.
#' Use multiple processors by setting \code{mc.cores} option using
#' \code{options()}.
#' 
#' @export
#' @return Data frame dictionary listing available variables.
createDictionary <- function(datadir="Current", name=NULL, quick=FALSE, sourcesFile = NULL) {
  stopifnot(datadir %in% c("Current", "../DataBuddy/DataRequests/Waiting Room"))
  if(is.null(sourcesFile))

    sourcesFile <- system.file("data/extdata", "sources.csv", package = "alspac")
    
    alspacdir <- options()$alspac_data_dir
    datadir <- file.path(alspacdir, datadir)
    files <- list.files(datadir,  
                        pattern="dta$",
                        full.names=TRUE,
                        recursive=TRUE,
                        ignore.case=TRUE)

    dictionary <- parallel::mclapply(files, function(file) {
      cat(date(), "loading", file, "\n")
      tryCatch({
        merge(
          processDTA(file, quick),
          createFileTable(file, alspacdir), by = "obj")
      }, error=function(e) {
        warning("Error loading", file, "\n")
        print(e)
        NULL
      })
    }) %>% dplyr::bind_rows()

    dictionary <- dictionary[which(dictionary$counts > 0),]
    
    ## add data sources information so that withdrawn consent can be 
    ## handled correctly for each variable
    dictionary <- addSourcesToDictionary(dictionary, sourcesFile)
    
    if (!is.null(name)) {
        saveDictionary(name, dictionary)
    }
    
    invisible(dictionary)
}

countCharOccurrences <- function(char, s) {
	s2 <- gsub(char,"",s)
	return(nchar(s) - nchar(s2))
}


trimWhitespace <- function(x) {
	if (is.numeric(x)) {
		return(x)
	}
	flag <- is.factor(x)
	x <- gsub("^\\s+|\\s+$", "", x)
	if (flag) {
		return(as.factor(x))
	} else {
		return(x)
	}
}


createFileTable <- function(fls, alspacdir) {
	#fls_dn <- dirname(fls) ## does some weird things with windows network paths
	fls_bn <- basename(fls)
    fls_dn <- sub(fls_bn, "", fls)
	fls_n <- gsub(".dta", "", fls_bn, ignore.case=TRUE)
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

processDTA <- function(fn, quick=FALSE) {
	if (quick) {
		temp <- suppressWarnings(readstata13::read.dta13(fn, select.rows=5))
	} else {
		temp <- suppressWarnings(readstata13::read.dta13(fn))
	# temp <- haven::read_dta(fn)
	}
	dat <- dplyr::tibble(
		name = colnames(temp),
		lab = attributes(temp)$var.labels,
		# lab = sapply(temp, function(x) attr(x, "label")),
		type = sapply(temp, function(x) class(x)[1]),
		obj = basename(fn)
	)
	if (quick) {
		dat$counts <- NA
	} else {
		dat$counts = sapply(temp, function(x) sum(!is.na(x) & x != -10 & x != -11))
	}
	return(dat)
}

#' Check dictionaries
#'
#' Check for missing values in the dictionaries to indicate WoCs are out of date
#' 
#' @export
checkDictionaries <- function() {
  #Column names in the dictionary
  columns_to_check <- c("mother", "mother_clinic", "mother_quest", "partner_quest","partner_clinic", "partner", "child_based",
                        "child_completed")
  
  globals <- get("globals", envir = .GlobalEnv) #Get the environment
  dict_names <- ls(envir = globals) # List all dictionary names
  
  if(length(dict_names)== 0){
    message("No dictionaries found in the library.")
    return(NULL)
  }
  
  for (dict_name in dict_names) {
    dict <-get(dict_name, envir = globals) #Retrieve a dictionary
    #Flag to track if any missing values were found in any column
    missing_found <- FALSE
    
  
    # Iterate through each dictionary
    for (dict_name in dict_names) {
      dict <- get(dict_name, envir = globals)  # Retrieve the dictionary
      
      # Flag to check if any missing values are found
      missing_found <- FALSE
      
      # Iterate over the specific hard-coded columns to check
      for (col in columns_to_check) {
        # Check if the column exists in the dictionary
        if (col %in% colnames(dict)) {
          # Check for missing values in the column
          num_NA <- sum(is.na(dict[[col]]))  
          
          if (num_NA > 0) {
            cat(sprintf("Dictionary '%s' has %d missing values in column '%s'.\n", dict_name, num_NA, col))
            missing_found <- TRUE
          }
        } else {
          cat(sprintf("Column '%s' not found in dictionary '%s'.\n", col, dict_name))
        }
      }
      
      # If no missing values were found, print this message once per dictionary
      if (!missing_found) {
        cat(sprintf("Dictionary '%s' has no missing values in the Withdrawal of Consent logic columns.\n", dict_name))
      }
    }
  }
}


