#' Guess the default data directory
#'
#' The R drive will be mounted in different paths for different systems.
#' This function guesses the path to be used as default in setDataDir
#' 
#' @export
#' @return NULL
getDefaultDataDir <- function() {

	d <- switch(Sys.info()['sysname'],
		Darwin = "/Volumes/ALSPAC-data/",
		Linux = "~/.gvfs/data/",
		Windows = "R:/Data"
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
#' \item{Windows: R:/Data/}
#' \item{Mac: /Volumes/data/}
#' \item{Linux: ~/.gvfs/data/}
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
setDataDir <- function(datadir=getDefaultDataDir()) {
    checkDataDir(datadir)
    options(alspac_data_dir=path.expand(datadir))
}

checkDataDir <- function(datadir) {
    test <- file.exists(datadir)
    if(test) {
        if(all(file.exists(paste0(datadir, c("/Syntax", "/Current"))))) {
            TRUE
        } else {
            stop("The specified data directory exists but it is not the correct directory. ",
                 "It should have the directories 'Syntax' and 'Current' contained within. ",
                 "It is normally located on the remote R drive, R:/Data/")
        }
    } else {
        stop("The data directory ", datadir, " has NOT been found. ",
             "It is normally located on the remote R drive, R:/Data/. ",
             "You will be able to search for variables from the dictionary but unable to extract them from the data. ",
             "Please check that the R: drive has been mounted onto your computer through the UoB VPN. ",
             "Run setDataDir(<directory name>) to try again.")
    }
    FALSE
}
