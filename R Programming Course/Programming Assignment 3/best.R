best <- function(state, outcome){
	full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	if (!(state %in% full_data[,7])) stop("invalid state")
	
	if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
		
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