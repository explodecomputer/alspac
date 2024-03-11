.onLoad <- function(libname = find.package("alspac"), pkgname = "alspac") {
    message("R/alspac version ", utils::packageVersion("alspac"))
    message("We have made several changes to how the extractVars function works.")
    message("This may lead to slightly different formatting compared to previously.")
    message("See also the new createDictionary function.")

    loadDictionaries()
    
    tryCatch(setDataDir(), error=function(e) print(e$message))
}



