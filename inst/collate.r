library(dplyr)

filelist <- commandArgs(T)

out <- lapply(commandArgs(T), function(x)
{
	load(x)
	return(dat)
}) %>% bind_rows

current <- subset(out, cat1 == "Current", select=-c(path))
save(current, file="../data/Current.RData")

useful <- subset(out, cat1 == "Useful_data", select=-c(path))
save(useful, file="../data/Useful_data.RData")

