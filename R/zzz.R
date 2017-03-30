.onLoad <- function(libname = find.package("alspac"), pkgname = "alspac")
{
	message("R/alspac version ", packageVersion("alspac"))
	setDataDir()
}
