.onLoad <- function(libname = find.package("alspac"), pkgname = "alspac") {
    message("R/alspac version ", packageVersion("alspac"))

    loadDictionaries()
    
    tryCatch(setDataDir(), error=function(e) print(e$message))
}



