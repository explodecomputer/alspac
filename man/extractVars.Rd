% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractvars.r
\name{extractVars}
\alias{extractVars}
\title{Extract variables from data}
\usage{
extractVars(
  x,
  exclude_withdrawn = TRUE,
  core_only = TRUE,
  adult_only = FALSE,
  spss = FALSE,
  haven = FALSE
)
}
\arguments{
\item{x}{Output from `findVars`}

\item{exclude_withdrawn}{Whether to automatically exclude withdrawn consent IDs. Default is TRUE.
This is conservative, removing all withdrawn consant ALNs from all datasets. Only use FALSE here
if you have a more specific list of withdrawn consent IDs for your specific variables.}

\item{core_only}{Whether to automatically exclude data from participants
not in the core ALSPAC dataset (Default: TRUE).
This should give the same samples as the Stata/SPSS scripts in the R:/Data/Syntax folder.}

\item{adult_only}{No longer supported. Parent-specific restrictions are applied
automatically when child-based or child-completed variables are not requested.}

\item{spss}{Logical. Default \code{FALSE}.}

\item{haven}{Logical. Default \code{FALSE}.}
}
\value{
A data frame with all the variable specified in `x`. If \code{exclude_withdrawn} was \code{TRUE}, then columns
named \code{woc_*} indicate which samples were excluded.
}
\description{
Take the output from `findVars` as a list of variables to extract from ALSPAC data
}
\details{
There are about 130 ALSPAC data files. Given output from `findVars`, this function will 
retrieve all the variables from these files and collapse them into a single data frame. 
It will return columns for all the variables, plus columns for `aln`, `qlet` and `mult_mum` 
or `mult_dad` if they were present in any of the files.

Suppose we extract a four variables, one for each of mothers, children, fathers and partners. This will return the variables requested, along with some other columns -

- `aln` - This is the pregnancy identifier. NOTE - this is **not** an individual identifier. For example, notice that row 4 has entries for the father variable `ff1a005a`, the mother variable `fm1a010a`, and the partner variable `pc013`.

- `qlet` - This is the child ID for the specific pregnancy. It will take values from A-D. **All** children will have a qlet, and **only** children will have a qlet. Therefore **if qlet is not NA, that row represents an individual child**.

- `alnqlet` - this is the ALN + QLET. If the individual is a child (e.g. row 8) then they will have a different `alnqlet` compared to the `aln`. Otherwise, the `aln` is the same as the `alnqlet`

- `mult_mum` and `mult_dad` - Sometimes the same mother (or father) had more than one pregnancy in the 18 month recruitment period. Those individuals have two ALNs. If either of these columns is "Yes" then that means you can drop them from the results if you want to avoid individuals being duplicated. This is the guidance from the FOM2 documentation:

   1.7 Important Note for all data users:
   Please be aware that some women may appear in the release file more than once. This is due to the way in which women were originally enrolled into the study and were assigned IDs. ALSPAC started by enrolling pregnant women and the main study ID is a pregnancy based ID. Therefore if a women enrolled with two different pregnancies (both having an expected delivery date within the recruitment period [April 1991-December 1992]), she will have two separate IDs to uniquely identify these women and their pregnancies. An indicator variable has been included in the file, called mult_mum to identify these women. If you are carrying out mother based research that does not require you to consider repeat pregnancies for which we have data then please select mult_mum == 'No' to remove the duplicate entries. This will keep one pregnancy and randomly drop the other pregnancy. If you are matching the data included in this file to child based data or have been provided with a dataset that includes the children of the ALSPAC pregnancies, as well as the mother-based data, you need not do anything as each pregnancy (and hence each child from a separate pregnancy) has a unique identifier and a mothers data has been included/repeated here for each of her pregnancies where appropriate.

The speed at which this function runs is dependent upon how fast your connection is to the R drive
and how many variables you are extracting at once.
}
\examples{
\dontrun{
# Find all variables with BMI in the description
bmi_variables <- findVars("bmi", ignore.case=TRUE)
# Extract all the variables into a data.frame:
bmi <- extractVars(bmi_variables)
# Alternatively just extract the variables for adults
bmi <- extractVars(subset(bmi_variables, cat3 \%in\% c("Mother", "Adult")))
}

}
