library(alspac)

setDataDir("~/work/alspac/data-20191104")

if (!dictionaryGood("current"))
    createDictionary("Current", "current")
if (!dictionaryGood("useful"))
    createDictionary("Useful_data", "useful")
       
dat <- extractDataset(
    variable_file="inputs/variables.csv",
    cid_file="inputs/ACEHDBFG.txt",
    output_format="sav",
    output_path="outputs",
    b_number="B0001",author="Smith")

dim(dat)

length(unique(dat$cidB0001))
table(dat$qlet)

t(sapply(dat[,startsWith(colnames(dat),"withdrawn")], table))

vars <- c('a603','b302','bestgest','cck420','ccc250','ccf127','cck400','ccl201','ccn280','ccr420','ccs2600','cct2501','ccxa210','CCXC109','cidB0001','d795','e390','fh6876','ff5270','fh6877','FJPL001','g268','h181a','in_ku','j346','kd505a','kf455','kj460a','kl475','kn4004','kq365a','kr813a','kt5000','ku693','kv5500','kw6513','kz011b','l4050','mz001','n3053','sa164a','se152','ta5003','tb5502','tc1211','YPB1231_imputeno')

for (var in vars) {
    label <- attributes(dat[[var]])$label
    labels <- attributes(dat[[var]])$labels
    labels <- paste(paste(names(labels), labels, sep="="), collapse="  ")
    freq <- table(dat[[var]])
    if (length(freq) > 20)
        freq <- quantile(dat[[var]], na.rm=T)
    freq <- paste(paste(names(freq), freq, sep="="), collapse="  ")
    cat("-----------------------------------\n",
        var, label, "\n",
        "Labels ",
        " ", labels, "\n",
        "Values ",
        " ", freq, "\n")
}

output_filename <- list.files("outputs", "Smith_B0001", full.names=T)[1]
dta.filename <- sub("sav", "dta", output_filename)
haven::write_dta(dat, path=dta.filename)
dat.dta <- haven::read_dta(dta.filename)

csv.filename <- sub("sav", "csv", output_filename)
write.csv(dat, file=csv.filename, row.names=F)
dat.csv <- read.csv(csv.filename,stringsAsFactors=F)

