.onLoad <- function(libname = find.package("alspac"), pkgname = "alspac") {
    message("R/alspac version ", packageVersion("alspac"))
    message("We have made several changes to how the extractVars function works.")
    message("This may lead to slightly different formatting compared to previously.")
    message("See also the new createDictionary function.")

    a <- suppressWarnings(try(readLines("https://raw.githubusercontent.com/explodecomputer/alspac/master/DESCRIPTION"), silent=TRUE))

    if(!inherits(a, 'try-error'))
    {
        latest <- gsub("Version: ", "", a[grep("Version", a)])
        current = utils::packageDescription('alspac')

        test <- utils::compareVersion(latest, current$Version)
        if(test == 1)
        {
            packageStartupMessage("\nWarning:\nYou are running an old version of the R/alspac package.\n",
                "This version:   ", current$Version, "\n",
                "Latest version: ", latest, "\n",
                "Please consider updating using remotes::install_github('explodecomputer/alspac')")
        }
    }

    loadDictionaries()
    
    tryCatch(setDataDir(), error=function(e) print(e$message))
}
