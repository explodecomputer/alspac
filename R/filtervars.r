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
#' vars <- filterVars(vars, kz021=c(obj="^kz"), 
#'                    kz011b=c(obj="^cp", lab="Participant"), 
#'                    c645a=c(cat2="Quest"))
#' }
filterVars <- function(x, ...) {
    filter.list <- list(...)

    ## check that each filter name corresponds to a variable in x
    none.idx <- which(!names(filter.list) %in% x$name)
    if (length(none.idx) != 0) {
        stop("Filter name(s) do not correspond to a variable name: ",
             paste(names(filter.list)[none.idx], collapse=", "))
    }

    ## check that the column names correspond to columns in x
    columns <- unlist(lapply(filter.list, function(filter) names(filter)))
    columns <- unique(setdiff(columns, colnames(x)))
    if (length(columns) > 0) {
        stop("Filter column name(s) (", paste(columns, collapse=", "), ") ", 
             "do not match columns in x (", paste(colnames(x), collapse=", "), ")")
    }
      
    ## identify variables that are being filtered
    filter.idx <- which(x$name %in% names(filter.list))
    if (length(filter.idx) == 0) {
        stop("None of the filter names matches a variable name")
    }
      
    ## apply each variable filter
    filtered.x <- lapply(names(filter.list), function(varname) {
        ## add the variable name to the filter
        filter <- c(name=paste0("^", varname, "$"), filter.list[[varname]])
        ## identify which variable(s) satisfy the filter
        matches <- sapply(names(filter), function(column) {
            grepl(filter[[column]], x[[column]])
        })
        if (length(filter) > 1) {
            matches <- apply(matches, 1, all)
        }
        satisfies.idx <- which(matches)
        ## if no variable satisfies the filter, then issue a warning
        if (length(satisfies.idx) == 0) {
            warning("Filter for ", varname, " does not match any variable")
        }
        ## if multiple variables satisfy the filter, then issue a warning
        if (length(satisfies.idx) > 1) {
            warning("Filter for ", varname, " matches multiple variables")
        }
        ## return matching variable(s)
        x[which(matches),]
    })
    filtered.x <- do.call(rbind, filtered.x)
    rbind(x[-filter.idx,], filtered.x)
}
