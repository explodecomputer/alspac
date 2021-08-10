library(alspac)
setDataDir("/home/alspac")

if (!dictionaryGood("current"))
    createDictionary("Current", "current")
if (!dictionaryGood("useful"))
    createDictionary("Useful_data", "useful")

vars <- list(partner=c("pb910","ff1a011"),
             child=c("ccaa991b","ka498","f7003a"),
             mother=c("a901","fm1a011"))
varnames <- unlist(vars)

vars <- lapply(vars, findVars)
vars$mother <- vars$mother[vars$mother$name != "fm1a011" | grepl("FOM1_", vars$mother$obj), ]

dat <- extractVars(do.call(rbind, vars))

dim(dat)

sapply(dat[,varnames], function(x) sum(x > 0, na.rm=T))

sapply(dat[,grep("^withdrawn", colnames(dat))], sum)



dat.full <- extractVars(do.call(rbind, vars), core_only=F)
dim(dat.full)

sapply(dat.full[,varnames], function(x) sum(x > 0, na.rm=T))

length(setdiff(dat.full$aln, dat$aln))

dat.full <- extractVars(rbind(do.call(rbind, vars),
                              findVars("mz001")),
                        core_only=F)
dim(dat.full)

table(dat.full$mz001)

length(setdiff(dat.full$aln, dat$aln))

dat <- extractVars(rbind(vars$partner, vars$mother), adult_only=T)
dim(dat)

sapply(dat[,colnames(dat) %in% varnames], function(x) sum(x > 0, na.rm=T))

sapply(dat[,grep("^withdrawn", colnames(dat))], sum)




vars <- findVars("c645a", whole.word=T,dictionary="current")
dat <- extractVars(vars)
table(dat$c645a)


vars <- findVars("c645a", "h470", "b594", "e424", "f244", "p2024",
                 "r5024", "f306", "f900", "b721", "b665", "mz028b",
                 "b032", "b300", "c064", "f242a", "g322a", "h232",
                 "h232a", "d536a", "f593",
                 dictionary="current", whole.word=T)
dat <- extractVars(vars)
table(dat$c645a)


dat <- extractVars(vars, adult_only = TRUE)
table(dat$c645a)


