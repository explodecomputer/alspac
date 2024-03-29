% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.r
\name{dictionaryGood}
\alias{dictionaryGood}
\title{Checks a dictionary}
\usage{
dictionaryGood(dictionary, max.print = 10)
}
\arguments{
\item{dictionary}{The name of an existing dictionary or the dictionary itself.}

\item{max.print}{The maximum number of missing files to list if any are missing
(Default: 10).}
}
\value{
\code{TRUE} if all files exist, otherwise \code{FALSE} and a warning listing at most
\code{max.print} missing files.
}
\description{
Checks if all the files referred to in the dictionary
are accessible given the ALSPAC data directory.
}
