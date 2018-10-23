library(dplyr)

filelist <- commandArgs(T)

out <- lapply(commandArgs(T), function(x)
{
	load(x)
	return(dat)
}) %>% bind_rows

current <- subset(out, cat1 == "Current")
save(current, file="../data/current.RData")

useful <- subset(out, cat1 == "Useful_data")
save(useful, file="../data/useful.RData")

