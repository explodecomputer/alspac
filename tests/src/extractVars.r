library(alspac)

setDataDir("/home/alspac")
#setDataDir("alspac")

if (!dictionaryGood("current")) {
    createDictionary("Current", "current")
}

vars <- list(partner=c("pb910","ff1a011"),
             child=c("ccaa991b","ka498","f7003a"),
             mother=c("a901","fm1a011"))
varnames <- unlist(vars)

vars <- lapply(vars, findVars)
vars$mother <- vars$mother[vars$mother$name != "fm1a011" | grepl("FOM1_", vars$mother$obj), ]

dat <- extractVars(vars$child)

stopifnot(length(unique(dat$alnqlet)) == 15645)
stopifnot(sum(dat$woc_child_based) == 32)
stopifnot(sum(dat$woc_child_completed) == 30)
stopifnot(all(is.na(dat$f7003a[dat$woc_child_based])))
stopifnot(all(is.na(dat$ka498[dat$woc_child_based])))
stopifnot(all(is.na(dat$ccaa991b[dat$woc_child_completed])))

dat <- extractVars(vars$mother)

stopifnot(length(unique(dat$aln)) == 15236)
stopifnot(sum(dat$mz005l==2,na.rm=TRUE) == 14833-30)
stopifnot(sum(dat$woc_mother) == 30)
stopifnot(all(is.na(dat$a901[dat$woc_mother])))
stopifnot(all(is.na(dat$fm1a011[dat$woc_mother])))
          
dat <- extractVars(vars$partner)

stopifnot(length(unique(dat$aln)) == 15236)
stopifnot(sum(dat$partner_in_alspac>0) == 12112)
stopifnot(sum(dat$woc_partner) == 5)
stopifnot(all(is.na(dat$pb910[dat$woc_partner | dat$partner_in_alspac==0])))
stopifnot(all(is.na(dat$ff1a011[dat$woc_partner | dat$partner_in_alspac==0])))


dat <- extractVars(do.call(rbind, vars))

dim(dat)

sapply(dat[,varnames], function(x) sum(x > 0, na.rm=TRUE))

sapply(dat[,grep("^woc_", colnames(dat))], sum)

dat.full <- extractVars(do.call(rbind, vars), core_only=FALSE)
dim(dat.full)

sapply(dat.full[,varnames], function(x) sum(x > 0, na.rm=TRUE))

length(setdiff(dat.full$aln, dat$aln))

dat.full <- extractVars(rbind(do.call(rbind, vars)),
                        core_only=FALSE)
dim(dat.full)

length(setdiff(dat.full$aln, dat$aln))

dat <- extractVars(rbind(vars$partner, vars$mother))
dim(dat)

sapply(dat[,colnames(dat) %in% varnames], function(x) sum(x > 0, na.rm=TRUE))

sapply(dat[,grep("^woc_", colnames(dat))], sum)

vars <- findVars("c645a", whole.word=TRUE,dictionary="current")
dat <- extractVars(vars)
table(dat$c645a)


vars <- findVars("c645a", "h470", "b594", "e424", "f244", "p2024",
                 "r5024", "f306", "f900", "b721", "b665", "mz028b",
                 "b032", "b300", "c064", "f242a", "g322a", "h232",
                 "h232a", "d536a", "f593","kz010","fm1a011",
                 dictionary="current", whole.word=TRUE)

vars <- filterVars(
    vars,
    mz028b=list(obj="^mz_", cat3="Cohort Profile"),
    kz010=list(obj="^cp_", cat3="Cohort Profile"))
                   
dat <- extractVars(vars)

table(dat$c645a)


