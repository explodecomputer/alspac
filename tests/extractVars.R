library(alspac)
setDataDir("~/work/alspac/data")

if (!dictionaryGood("current"))
    createDictionary("Current", "current")
if (!dictionaryGood("useful"))
    createDictionary("Useful_data", "useful")


## outputs below generated on 07/03/2019

vars <- list(partner=c("pb910","ff1a011"),
             child=c("ccaa991b","ka498","f7003a"),
             mother=c("a901","fm1a011"))

vars <- lapply(vars, findVars)

dat <- extractVars(do.call(rbind, vars))

dim(dat)
## [1] 15643    46
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
length(setdiff(dat.full$aln, dat$aln))
## [1] 13

dat.full <- extractVars(rbind(do.call(rbind, vars),
                              findVars("mz001")),
                        core_only=F)
dim(dat.full)
## [1] 20006    17
length(setdiff(dat.full$aln, dat$aln))
## [1] 4379

dat <- extractVars(rbind(vars$partner, vars$mother), adult_only=T)
dim(dat)
## [1] 14541    34
sapply(dat[,grep("^withdrawn", colnames(dat))], sum)
  ## withdrawn.consent.mother_clinic    withdrawn.consent.mother_quest 
  ##                              12                                10 
  ## withdrawn.consent.partner_quest         withdrawn.consent.partner 
  ##                               1                                 2 
  ##   withdrawn.consent.child_based withdrawn.consent.child_completed 
  ##                              10                                19 



