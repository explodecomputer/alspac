library(alspac)
setDataDir("~/work/alspac/data")

## outputs below generated on 08/02/2019

vars <- list(partner=c("pb910","ff1a011"),
             child=c("ccaa991b","ka498","f7003a"),
             mother=c("a901","fm1a011"))

vars <- lapply(vars, findVars)

dat <- extractVars(do.call(rbind, vars))

dim(dat)
## [1] 15643    45
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
##   withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
##                                 9                                 8 
##   withdrawn.consent.partner_quest     withdrawn.consent.child_based 
##                                 1                                 8 
## withdrawn.consent.child_completed 
##                                20 


dat.full <- extractVars(do.call(rbind, vars), core_only=F)
dim(dat.full)
## [1] 15467    15
length(setdiff(dat.full$aln, dat$aln))
## [1] 13

dat.full <- extractVars(rbind(do.call(rbind, vars),
                              findVars("mz001")),
                        core_only=F)
dim(dat.full)
## [1] 20006    16
length(setdiff(dat.full$aln, dat$aln))
## [1] 4379

dat <- extractVars(rbind(vars$partner, vars$mother))
dim(dat)
## [1] 14541    33
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
##   withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
##                                 7                                 6 
##   withdrawn.consent.partner_quest     withdrawn.consent.child_based 
##                                 1                                 6 
## withdrawn.consent.child_completed 
##                                19 


