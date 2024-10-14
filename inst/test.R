if(! "devtools" %in% installed.packages()[,"Package"]) {
	install.packages("devtools")
}

devtools::load_all()

data(current)
#data(useful)

# Get random variables
cvars <- sample(1:nrow(current), 3)
#uvars <- sample(1:nrow(useful), 3)

cdat <- extractVars(current[cvars,])
#udat <- extractVars(useful[uvars,])

str(cdat)
#str(udat)