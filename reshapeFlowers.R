#rm(list = ls(all = TRUE))

# to use: type data1 <- reshapeFlowers(data)
# you will be prompted to input the frequency for days
# press enter after input
# this is a little slow, so be patient. It may take up to 45 seconds for this to run

# ----------------- Required Libraries -----------------
if(!"dplyr" %in% installed.packages()){
	install.packages("dplyr")
}
require(dplyr)

# ----------------- Necessary Functions ----------------- 
# function for querying the day frequency
freq.fun <- function() {
	do <- readline("What is the frequency, in days, desired?  ")
	return(do)
}

# function to add in dates by id
addDates <- function(x, sample.freq = sample.freq) {
	d <- merge(x, data.frame(date = seq(min(x[, "sample.Date.t"], na.rm = T), max(x[, "sample.Date.t"], 
		na.rm = T), by = sample.freq)), by.x = "sample.Date.t", by.y = "date", all = T)
	d$id <- mean(d$id, na.rm = T)
	d <- d[!is.na(d$sample.Date.t), ]
	return(d)
}

# last observation moved forward
# replaces all NA values with last non-NA values
na.lomf <- function(x) {
	na.lomf.0 <- function(x) {
		non.na.idx <- which(!is.na(x))
		if (is.na(x[1L])) {
			non.na.idx <- c(1L, non.na.idx)
		}
		rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
	}
	dim.len <- length(dim(x))
	if (dim.len == 0L) {
		na.lomf.0(x)
	} else {
		apply(x, dim.len, na.lomf.0)
	}
}

# quick function to pull up second timestamp (and stage; aka a lag function)
lg <- function(x) c(x[2:length(x)], NA)


# ----------------- The ultimate function ----------------- 

reshapeFlowers <- function(data) {

	num <- if (interactive()) {
		freq.fun()
	} else {
		cat("What is the frequency, in days, desired?  ")
		do <- readLines("stdin")
		return(do)
		cat("\n")
	}
	# sequence input (days)
	sample.freq <- as.numeric(num)

	# detects stages and corrects for any possible errors in typing in the date:
	data[, cols <- grep("Stage.", names(data))] <- lapply(data[, cols <- grep("Stage.", names(data))], 
		as.Date, format = "%Y%m%d")
	# first, detect columns with stage
	colN <- which(grepl("Stage.", names(data)))

	# make long format
	dat <- reshape(data, varying = colN, v.names = "sample.Date.t", timevar = "stage.t", direction = "long")
	# order by id (each id = each sample row in wide format)		
	dat <- dat[order(dat$id), ]

	# merging in dates
	dat <- do.call(rbind, by(dat, dat$id, function(x) addDates(x, sample.freq = sample.freq)))

	# gap fill the stage for the new dates
	dat$stage.t <- unlist(by(dat, dat$id, function(x) na.lomf(x$stage.t)), use.names = F)
	# gap fill the numeric data (to maintain numeric structure)
	dat[, c(2:5, 16)] <- do.call(rbind, by(dat, dat$id, function(x) na.lomf(x[, c(2:5, 16)])))
	# and gap fill the who-know-what-it-means strings
	dat[, 6:15] <- do.call(rbind, by(dat, dat$id, function(x) na.lomf(x[, 6:15])))

	#clean up size (random spaces and capitalization)
	require(dplyr)
	# define Size from factor to character
	dat$Size <- as.character(dat$Size)
	# find the spaces and remove them
	dat$Size <- gsub(" ", "", dat$Size, fixed = TRUE)
	# lower-case the words
	dat$Size <- unlist(lapply(dat$Size, function(v) {
		if (is.character(v)) 
			return(tolower(v))
		else (return(v))
	}))

	# adding in that column for second timestamp
	# this step will show errors if there is only 1 observation for a site
	# errors can be ignored
	dat$sample.Date.t1 <- ave(dat$sample.Date.t, dat$id, FUN = lg)
	dat$stage.t1 <- ave(dat$stage.t, dat$id, FUN = lg)

	# These are the fields that need to be NA'd out (if = 1)	
	test1 <- unlist(by(dat, dat$id, function(x) ifelse(x$stage.t1 < tail(x$stage.t, 1), 1, NA)), use.names = F)
	# and the final clean up
	dat[!is.na(test1), 9:14] <- NA
	#re-order the columns to make for pretty
	ord.list <- c("id", names(dat[2:15]), "sample.Date.t", "sample.Date.t1", "stage.t", "stage.t1")
	return(dat[,ord.list])
}