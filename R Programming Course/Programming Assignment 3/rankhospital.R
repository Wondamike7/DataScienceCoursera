rankhospital <- function(state,outcome,num = "best") {
	full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	if (!(state %in% full_data[,7])) stop("invalid state")
	if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")

	if (outcome == "heart attack") {
		full_data<-full_data[!full_data[,11]=="Not Available",] 						
		full_data[,11] <- as.numeric(full_data[,11]) 								
		h_a <- full_data[,c(2,7,11)] 											
		h_a <- h_a[h_a$State==state,] 										
		h_a <- h_a[order(h_a[,3],h_a[,1],na.last = TRUE),] 							
		rank_ha <- as.integer(rownames(h_a))
		h_a_new <- cbind(h_a, rank_ha)
		h_a_obs <- nrow(h_a_new)
		if (num == "best") {
			h_a_new[1,1] 
		} else if (num == "worst") {
			h_a_new[h_a_obs,1]
		} else if (num < h_a_obs) {
			h_a_new[num,1]
		} else {
			NA
		}
	} else if (outcome == "heart failure") {
		full_data<-full_data[!full_data[,17]=="Not Available",]
		full_data[,17] <- as.numeric(full_data[,17])
		h_f <- full_data[,c(2,7,17)]
		h_f <- h_f[h_f$State==state,]
		h_f <- h_f[order(h_f[,3],h_f[,1],na.last = TRUE),]
		rank_hf <- as.integer(rownames(h_f))
		h_f_new <- cbind(h_f, rank_hf)
		h_f_obs <- nrow(h_f_new)
		if (num == "best") {
			h_f_new[1,1] 
		} else if (num == "worst") {
			h_f_new[h_f_obs,1]
		} else if (num < h_f_obs) {
			h_f_new[num,1]
		} else {
			NA
		}
	} else if (outcome == "pneumonia") {
		full_data<-full_data[!full_data[,23]=="Not Available",]
		full_data[,23] <- as.numeric(full_data[,23])
		pneu <- full_data[,c(2,7,23)]
		pneu <- pneu[pneu$State==state,]
		pneu <- pneu[order(pneu[,3],pneu[,1],na.last = TRUE),]
		rank_pneu <- as.integer(rownames(pneu))
		pneu_new <- cbind(pneu, rank_pneu)
		pneu_obs <- nrow(pneu_new)
		if (num == "best") {
			pneu_new[1,1]
		} else if (num == "worst") {
			pneu_new[pneu_obs,1]
		} else if (num < pneu_obs) {
			pneu_new[num,1]
		} else {
			NA
		}
	}
}
