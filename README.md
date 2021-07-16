# ALSPAC data interface


## Motivation

To obtain ALSPAC variables, the general procedure is:

1. search through PDFs to find the variable(s)
2. search through STATA files to extract
3. load into R files to use

This package combines the search and extraction procedure into two functions, this makes the work a bit more reproducible. It works for the curated data in the `R:/Current/` and `R:/Useful_data` directories.

The extracted data has withdrawn consent individuals removed automatically.

You can browse the variables here: [http://variables.alspac.bris.ac.uk/](http://variables.alspac.bris.ac.uk/)


## Limitations

- The data is not curated further than what is present already in the STATA `.dta` files
- The `Useful_data` folder contains only some of the known user derived variables. As these variables continue to be processed the directory will be updated, but contact the ALSPAC team if there is something you need which cannot be found here.
- This is a beta version of the package, please report errors or suggestions to [g.hemani@bristol.ac.uk](mailto:g.hemani@bristol.ac.uk)
- Variable look-ups can be performed off-line and without any restrictions, but variable extraction requires access to the R drive where the data is residing.
- ALN IDs refer to pregnancies. The same ALN is shared between mothers, their children, partners and fathers. Children can be distinguished from the others by virtue of having a QLET code. But distinguishing between mothers, fathers and partners is not straightforward. See note below on how data that includes different individuals with the same ALN is presented.


## Credits etc

Please report issues or suggestions to [Gibran Hemani](mailto:g.hemani@bristol.ac.uk). Thanks to Tom Gaunt, Matt Suderman, Andrew Simpkin, Paul Yousefi, ALSPAC team for help in developing this package.


## Installation

To install version 0.6.1:

```r
install.packages("devtools")
library(devtools)
install_github("explodecomputer/alspac")
```

The `alspac` package can then be loaded as follows:

```r
library(alspac)
```


## Finding variables

### Browsing the variables manually

There are two data objects that come with the package - `current` and `useful` - that contain all the variables available in the `R:/Current/` and `R:/Useful_data` directories, respectively. They can be searched manually after loading them directly, e.g. to load the `current` variables:

```r
data(current)
```

and to load the `useful` variables:

```r
data(useful)
```

The top 6 rows of `current` look like this:

```
   obj name                   lab counts    type    cat1  cat2   cat3 cat4         path
1 a_3c  aln  Pregnancy identifier  13545 integer Current Quest Mother <NA> Current/Quest/Mother
2 a_3c a001 Questionnaire version  13545  factor Current Quest Mother <NA> Current/Quest/Mother
3 a_3c a002    Time lived in Avon  13545  factor Current Quest Mother <NA> Current/Quest/Mother
4 a_3c a003 Years since last move  13545 integer Current Quest Mother <NA> Current/Quest/Mother
5 a_3c a004 Weeks since last move  13545 integer Current Quest Mother <NA> Current/Quest/Mother
6 a_3c a005  NO of moves in 5 YRS  13545 integer Current Quest Mother <NA> Current/Quest/Mother
```

- `obj` is the filename
- `name` is the variable name
- `lab` is the description of the variable
- `counts` is the number of non-missing values
- `type` is the data type
- `cat1`-`cat4` are the folder names in which the object is found, used here loosely as categories for the data
- `path` is the path to the `obj` file


### Using the search function

A simple search function `findVars` is a simple helper for searching `current` (default) or `useful`. Documentation can be retrieved using:

```r
?findVars
```

For example, to search for all variables with the word 'height' in the description from the `current` data:

```r
vars <- findVars("height")
```

If I want any with height OR length in the description then:

```r
vars <- findVars("height", "length", logic="any", whole.word=TRUE, ignore.case=TRUE)
```

If I want any with height AND length:

```r
vars <- findVars("height", "length", logic="and", whole.word=TRUE, ignore.case=TRUE)
```

If I want to find anything with sleep somewhere (not necessarily a whole word) I might do:

```r
vars <- findVars("sleep", "slept", logic="any", whole.word=FALSE, ignore.case=TRUE)
```

To find all variables that have the term "difficulties" from the `useful` data:

```r
vars <- findVars("difficulties", dictionary="useful")
```

Some of these arguments have defaults but just writing them out for illustration.

### Filtering a list of variables

`findVars` may identify multiple
variables with the same name.

For example, searching for variables "kz021", "kz011b" and "c645a"
will return multiple variables with the same name.
```r
varnames <- c("kz021","kz011b","ype9670", "c645a")
vars <- findVars(varnames)
```

There are two ways to select among these duplicates.

1. **Manually** select rows corresponding to variables that I want, e.g.
```r
vars <- vars[vars$name=="kz021" & startsWith(vars$obj,"cp")
             | vars$name=="kz011b" & startsWith(vars$obj,"cp")
			 | vars$name=="c645a" & vars$cat2=="Quest"
			 | vars$name=="YPE9670",]
```

In other words, I require that the "kz021" variable come from a
STATA file name starting with "kz" ("obj" column in `vars`),
"kz011b" comes from a file name starting with "cp"
and the description of the variable ("lab" column in `vars`)
include the word "Participant",
and "c645a" comes from a questionnaire ("cat2" column in `vars`).

2. Apply **`filterVars`**.

As a first clean-up step, I remove any variables whose names
do not exactly match one of the variable names I am looking for.
```r
vars <- subset(vars, subset=tolower(name) %in% varnames)
```

I then use `filterVars` to select variables
satisfying the criteria described above.
```r
vars <- filterVars(vars,
                   kz021=c(obj="^kz"),
        		   kz011b=c(obj="^cp", lab="Participant"),
		           c645a=c(cat2="Quest")) 
```

## Extracting variables

To extract the data corresponding to this selection of variables, I need to have mounted the `R:/Data` drive on my computer. When I load the package (`library(alspac)`), if I have the R drive loaded then I should get a message like this:

```
The data directory has been recognised
```

Sometimes this might not work - the package tries to guess where the R drive will be mounted but it might guess wrong. If I receive an error message instead, then I need to specify the location of the R drive manually:

```r
setDataDir("/path/to/R drive/data/")
```

Once I have received the message `The data directory has been recognised`, I am able to extract the variables I need from the R drive.

```r
results <- extractVars(vars)
```

### Important note on IDs

Suppose I extract a variable measured in each of mothers, children, fathers and partners. e.g.

```r
x <- subset(current, name %in% c("cf010", "ff1a005a", "fm1a010a", "pc013"))
y <- extractVars(x)
```

returning e.g.

```
> head(y, 10)
   alnqlet   aln qlet cf010 mult_dad ff1a005a mult_mum fm1a010a pc013
1    30001 30001 <NA>    NA     <NA>       NA     <NA>       NA     2
2    30004 30004 <NA>    NA     <NA>       NA     <NA>       NA     2
3    30006 30006 <NA>    NA     <NA>       NA     <NA>       NA     2
4    30008 30008 <NA>    NA       No        7       No        4     2
5    30010 30010 <NA>    NA     <NA>       NA       No        1    NA
6    30012 30012 <NA>    NA     <NA>       NA       No        7    NA
7    30013 30013 <NA>    NA     <NA>       NA       No       10     2
8   30013A 30013    A    18     <NA>       NA     <NA>       NA    NA
9    30017 30017 <NA>    NA     <NA>       NA       No        7    NA
10   30019 30019 <NA>    NA       No        7       No        4     1
```

This has returned the variables requested, along with some other columns -

- `aln` - This is the pregnancy identifier. NOTE - this is **not** an individual identifier. For example, notice that row 4 has entries for the father variable `ff1a005a`, the mother variable `fm1a010a`, and the partner variable `pc013`. 
- `qlet` - This is the child ID for the specific pregnancy. It will take values from A-D. **All** children will have a qlet, and **only** children will have a qlet. Therefore **if qlet is not NA, that row represents an individual child**.
- `alnqlet` this is the ALN + QLET. If the individual is a child (e.g. row 8) then they will have a different `alnqlet` compared to the `aln`. Otherwise, the `aln` is the same as the `alnqlet`
- `mult_mum` and `mult_dad` - Sometimes the same mother (or father) had more than one pregnancy in the 18 month recruitment period. Those individuals have two ALNs. If either of these columns is "Yes" then that means you can drop them from the results if you want to avoid individuals being duplicated. This is the guidance from the FOM2 documentation:
    
    > 1.7 Important Note for all data users
    > 
    > Please be aware that some women may appear in the release file more than once. This is due to the way in which women were originally enrolled into the study and were assigned IDs. ALSPAC started by enrolling pregnant women and the main study ID is a pregnancy based ID. Therefore if a women enrolled with two different pregnancies (both having an expected delivery date within the recruitment period [April 1991-December 1992]), she will have two separate IDs to uniquely identify these women and their pregnancies. An indicator variable has been included in the file, called mult_mum to identify these women. If you are carrying out mother based research that does not require you to consider repeat pregnancies for which we have data then please select mult_mum == 'No' to remove the duplicate entries. This will keep one pregnancy and randomly drop the other pregnancy. If you are matching the data included in this file to child based data or have been provided with a dataset that includes the children of the ALSPAC pregnancies, as well as the mother-based data, you need not do anything as each pregnancy (and hence each child from a separate pregnancy) has a unique identifier and a mothers’ data has been included/repeated here for each of her pregnancies where appropriate.

If you have a better way to present these data do contact me.


## Using the website to browse variables

Variables can be browsed at [https://alspac-example.shinyapps.io/alspac-dt/](https://alspac-example.shinyapps.io/alspac-dt/). This contains both the 'Current' and 'Useful_data' variables.

This site can also be used to select variables for extraction:

1. Select the variables of interest from the table, then click 'Download variable list'. This will download a csv file containing information about the variables chosen. 
2. Next, use the `extractWebOutput` function in the `alspac` package to extract the variables, specifying the name of the file that you just downloaded. For example

```
extractWebOutput("data-2017-08-22.csv")
```

To code for the shiny variable app is here: [https://github.com/explodecomputer/alspac-shiny](https://github.com/explodecomputer/alspac-shiny)


## Dictionary Maintenance

From time to time the `R:\Data\Current\` directory is updated with new files.
The variable dictionaries that the package uses can be updated using the
`createDictionary` function.

```
current <- createDictionary("Current", name="current")
useful <- createDictionary("Useful_data", name="useful")
```

These updated dictionaries will be saved within the R package
for use in later R sessions.  In other words, an update will only
need to be peformed one time.  


