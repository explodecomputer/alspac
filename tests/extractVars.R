library(alspac)
setDataDir("~/work/alspac/data")

## outputs below generated on 31/01/2019

vars <- list(partner=c("pb910","ff1a011"),
             child=c("ccaa991b","ka498","f7003a"),
             mother=c("a901","fm1a011"))
             
all.vars <- findVars("*")
vars <- lapply(vars, function(x) all.vars[match(x, all.vars$name),,drop=F])

dat <- extractVars(do.call(rbind, vars))
dim(dat)
## [1] 15643    47
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
## withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
##                               8                                 7 
## withdrawn.consent.partner_quest withdrawn.consent.child_completed 
##                               1                                19
##   withdrawn.consent.child_based 
##                               7   

dat.full <- extractVars(do.call(rbind, vars), core_only=F)
dim(dat.full)
## [1] 15467    17
length(setdiff(dat.full$aln, dat$aln))
## [1] 13

dat.full <- extractVars(do.call(rbind, c(vars,
                                         list(all.vars[match("mz001", all.vars$name),,drop=F]))),
                        core_only=F)
dim(dat.full)
## [1] 20006    18
length(setdiff(dat.full$aln, dat$aln))
## [1] 4379

dat <- extractVars(do.call(rbind, vars), adult_only=T)
## Extracting from: ~/work/alspac/data/Current/Quest/Child Completed/cca_1a.dta
## adult_only dataset requested but includes variables for ALSPAC young people:
## ccaa991b
## Skipping...
## Extracting from: ~/work/alspac/data/Current/Clinic/Child/f07_4a.dta
## adult_only dataset requested but includes variables for ALSPAC young people:
## f7003a
## Skipping...
## Extracting from: ~/work/alspac/data/Current/Quest/Child Based/ka_4c.dta
## adult_only dataset requested but includes variables for ALSPAC young people:
## ka498
## Skipping...
dim(dat)
## [1] 14541    35

dat <- extractVars(rbind(vars$partner, vars$mother), adult_only=T)
dim(dat)
## [1] 14541    35
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
## withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
##                               6                                 5 
## withdrawn.consent.partner_quest withdrawn.consent.child_completed  
##                               1                                18 
##   withdrawn.consent.child_based 
##                               5 

