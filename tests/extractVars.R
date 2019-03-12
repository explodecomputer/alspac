library(alspac)
setDataDir("~/work/alspac/data")

if (!dictionaryGood("current"))
    createDictionary("Current", "current")
if (!dictionaryGood("useful"))
    createDictionary("Useful_data", "useful")

vars <- list(partner=c("pb910","ff1a011"),
             child=c("ccaa991b","ka498","f7003a"),
             mother=c("a901","fm1a011"))
varnames <- unlist(vars)

vars <- lapply(vars, findVars)

dat <- extractVars(do.call(rbind, vars))

dim(dat)
## [1] 15643    46
sapply(dat[,varnames], function(x) sum(x > 0, na.rm=T))
## pb910  ff1a011 ccaa991b    ka498   f7003a     a901  fm1a011 
##  9653     2044     7387    12333     8273    13513     5036 
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
## withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
##                              15                                13 
## withdrawn.consent.partner_quest         withdrawn.consent.partner 
##                               1                                 2 
##   withdrawn.consent.child_based withdrawn.consent.child_completed 
##                              13                                20 


dat.full <- extractVars(do.call(rbind, vars), core_only=F)
dim(dat.full)
## [1] 15467    16
sapply(dat.full[,varnames], function(x) sum(x > 0, na.rm=T))
## pb910  ff1a011 ccaa991b    ka498   f7003a     a901  fm1a011 
##  9653     2050     7387    12333     8273    13513     5040 
length(setdiff(dat.full$aln, dat$aln))
## [1] 13

dat.full <- extractVars(rbind(do.call(rbind, vars),
                              findVars("mz001")),
                        core_only=F)
dim(dat.full)
## [1] 20006    17
table(dat.full$mz001)
##     1     2 
## 14745  5254 
length(setdiff(dat.full$aln, dat$aln))
## [1] 4379

dat <- extractVars(rbind(vars$partner, vars$mother), adult_only=T)
dim(dat)
## [1] 14541    34
sapply(dat[,colnames(dat) %in% varnames], function(x) sum(x > 0, na.rm=T))
##  a901 ff1a011 fm1a011   pb910 
## 13341    1939    4734    9533 
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
## withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
##                              12                                10 
## withdrawn.consent.partner_quest         withdrawn.consent.partner 
##                               1                                 2 
##   withdrawn.consent.child_based withdrawn.consent.child_completed 
##                              10                                19 


vars <- findVars("c645a", whole.word=T,dictionary="current")
dat <- extractVars(vars)
table(dat$c645a)
## -1    1    2    3    4    5 
## 79 2526 1229 4326 2798 1609 

vars <- findVars("c645a", "h470", "b594", "e424", "f244", "p2024",
                 "r5024", "f306", "f900", "b721", "b665", "mz028b",
                 "b032", "b300", "c064", "f242a", "g322a", "h232",
                 "h232a", "d536a", "f593",
                 dictionary="current", whole.word=T)
dat <- extractVars(vars)
table(dat$c645a)
## -1    1    2    3    4    5 
## 79 2526 1229 4326 2798 1609 

dat <- extractVars(vars, adult_only = TRUE)
table(dat$c645a)
## -1    1    2    3    4    5 
## 78 2493 1215 4270 2767 1586 





