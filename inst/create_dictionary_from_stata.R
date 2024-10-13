args <- commandArgs(TRUE)

alspac.dir <- args[1]

if (length(args) > 1) {
    mc.cores <- as.integer(args[2])
}

options(mc.cores=mc.cores)

library(alspac)
setDataDir(alspac.dir)

current <- createDictionary("Current", name="current")
## 40s with 10 cores

# useful <- createDictionary("Useful_data", name="useful")
## 5s with 10 cores

save(current, file="../data/current.rdata")
# save(useful, file="../data/useful.rdata")
