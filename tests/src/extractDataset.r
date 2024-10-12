library(alspac)

setDataDir("/home/alspac")
#setDataDir("alspac")

example_path <- "."
output_path <- file.path(example_path, "outputs")

if (!dictionaryGood("current"))
    createDictionary("Current", "current")
       
dat <- extractDataset(
    variable_file=file.path(example_path, "inputs/variables.csv"),
    cid_file=file.path(example_path, "inputs/ACEHDBFG.txt"),
    output_format="sav",
    output_path=output_path, 
    b_number="B0001",author="Smith")

dim(dat)
stopifnot(nrow(dat) == 15645)

## check number of WoCs
stopifnot(sum(dat$woc_child_based) == 32)
stopifnot(sum(dat$woc_child_completed) == 30)
stopifnot(sum(dat$woc_mother) == 32)
stopifnot(sum(dat$woc_partner) == 5)

## check that all variable values for WoCs have been removed
requested.vars <- utils::read.csv(file.path(example_path, "inputs/variables.csv"))
requested.vars <- findVars(requested.vars$Name, whole.word=TRUE)
for (group in c("mother","partner","child_based","child_completed")) {
    for (varname in requested.vars$name[requested.vars[[group]]]) {
        cat(group, varname,"\n")
        if (any(!is.na(dat[[varname]][dat[[paste("woc",group,sep="_")]]]))) {
            stop(varname, "contains values for a", group, "WOC")
        }
    }
}

length(unique(dat$cidB0001))
table(dat$qlet)

t(sapply(dat[,startsWith(colnames(dat),"withdrawn")], table))

vars <- c('a603','b302','bestgest','cck420','ccc250','ccf127','cck400','ccl201','ccn280','ccr420','ccs2600','cct2501','ccxa210','CCXC109','cidB0001','d795','e390','fh6876','ff5270','fh6877','FJPL001','g268','h181a','in_ku','j346','kd505a','kf455','kj460a','kl475','kn4004','kq365a','kr813a','kt5000','ku693','kv5500','kw6513','kz011b','l4050','n3053','sa164a','se152','ta5003','tb5502','tc1211','YPB1231_imputeno')

for (var in vars) {
    label <- attributes(dat[[var]])$label
    labels <- attributes(dat[[var]])$labels
    labels <- paste(paste(names(labels), labels, sep="="), collapse="  ")
    freq <- table(dat[[var]])
    if (length(freq) > 20)
        freq <- quantile(dat[[var]], na.rm=TRUE)
    freq <- paste(paste(names(freq), freq, sep="="), collapse="  ")
    cat("-----------------------------------\n",
        var, label, "\n",
        "Labels ",
        " ", labels, "\n",
        "Values ",
        " ", freq, "\n")
}

output_filename <- list.files(output_path, "Smith_B0001", full.names=TRUE)[1]
dta.filename <- sub("sav", "dta", output_filename)
haven::write_dta(dat, path=dta.filename)
dat.dta <- haven::read_dta(dta.filename)

csv.filename <- sub("sav", "csv", output_filename)
utils::write.csv(dat, file=csv.filename, row.names=FALSE)
dat.csv <- utils::read.csv(csv.filename,stringsAsFactors=FALSE)

similar <- function(x,y) {
    harmonize <- function(x) {
        ifelse(any(c("integer", "double", "logical") %in% class(x)), as.numeric(x), x)
    }
    identical(harmonize(x), harmonize(y))
}
stopifnot(all(sapply(colnames(dat.csv), function(col) similar(dat.csv[[col]], dat[[col]]))))
stopifnot(all(sapply(colnames(dat.dta), function(col) similar(dat.dta[[col]], dat[[col]]))))
