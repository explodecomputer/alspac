
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
#' @param dictionary Data frame or name of a data dictionary. Dictionary available by default is
#' "current" (from the R:/Data/Current folder).
#' New dictionaries can be created using the \code{\link{createDictionary}()} function. (Default: "current").
#'
#' @export
#' @return A data frame containing a list of the variables, the files they originate from, and some descripton about the files
#' @examples \dontrun{
#' # Find variables with BMI or height in the description (this will return a lot of results!)
#' bmi_variables <- findVars("bmi", "height", logic="any", ignore.case=TRUE)
#'}
findVars <- function(..., logic="any", ignore.case=TRUE, perl=FALSE, fixed=FALSE, whole.word=FALSE, dictionary="current")
{
        if (is.character(dictionary)) {
            dictionary <- retrieveDictionary(dictionary)
        }
        
	l <- unlist(list(...))
	stopifnot(length(l) > 0)
	stopifnot(logic %in% c("any", "all", "none"))
	invert <- ifelse("none", TRUE, FALSE)
	if (whole.word) {
		l <- lapply(l, whole.word.regex)
	}

	n <- 1:nrow(dictionary)

	# Search for patterns in label
        g <- lapply(l, function(l){
            c(grep(l, dictionary$lab, ignore.case = ignore.case, perl = perl, fixed = fixed, invert = invert),
              grep(whole.word.regex(l), dictionary$name, ignore.case = ignore.case, perl = perl, fixed = fixed, invert = invert))
        })

	if (logic == "any") {
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
        if (any(var.freq > 1)) {
            warning("One or more variables has multiple sources (fix with filterVars()): ",
                    paste(names(var.freq)[which(var.freq > 1)], collapse=", "))
        }
	return(out)
}
