library(dplyr)

filelist <- commandArgs(T)

out <- lapply(commandArgs(T), function(x)
{
	load(x)
	return(dat)
}) %>% bind_rows


Current <- subset(out, grepl("^/Current", filename), select=-c(filename))
save(Current, file="../data/Current.RData")

Useful_data <- subset(out, grepl("^/Useful_data", filename), select=-c(filename))
save(Useful_data, file="../data/Useful_data.RData")

