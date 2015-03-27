best <- function(state, outcome){
	## read in data, must be in same working directory as this .R file
	full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")				
	
	## Error check for state and outcome. If state isn't in the list of states, stop. If outcome isn't one of three specified, stop.
	if (!(state %in% full_data[,7])) stop("invalid state")										
	if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
	
	## Three similar if statements, one per outcome.
	## Clean up the data by turning "Not Available" to NA, turning the outcome to numeric, and then chopping to only three columns of interest
	## Filter to only the state provided, then order the data by outcome and hospital name, and return the hospital name for the first entry (best!)
	if (outcome == "heart attack") {
		full_data<-full_data[!full_data[,11]=="Not Available",]
		full_data[,11] <- as.numeric(full_data[,11])
		h_a <- full_data[,c(2,7,11)]
		h_a <- h_a[h_a$State==state,]
		h_a <- h_a[order(h_a[,3],h_a[,1],na.last = TRUE),]
		h_a[1,1]
	} else if (outcome == "heart failure") {
		full_data<-full_data[!full_data[,17]=="Not Available",]
		full_data[,17] <- as.numeric(full_data[,17])
		h_f <- full_data[,c(2,7,17)]
		h_f <- h_f[h_f$State==state,]
		h_f <- h_f[order(h_f[,3],h_f[,1],na.last = TRUE),]
		h_f[1,1]
	} else if (outcome == "pneumonia") {
		full_data<-full_data[!full_data[,23]=="Not Available",]
		full_data[,23] <- as.numeric(full_data[,23])
		pneu <- full_data[,c(2,7,23)]
		pneu <- pneu[pneu$State==state,]
		pneu <- pneu[order(pneu[,3],pneu[,1],na.last = TRUE),]
		pneu[1,1]
	}
}	

## Could also have captured the *outcome* column in separate portion of code
## and then only had one loop that used the appropriate outcome col.