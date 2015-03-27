rankall <- function(outcome, num = "best") {
	full_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## outcome error check
	if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")

	## determines outcome column to pull from dataset and also cleans data
	column <- if (outcome=="heart attack") {
				full_data <- full_data[!full_data[,11]=="Not Available",] 						
				full_data[,11] <- as.numeric(full_data[,11]) 
				11
			} else if (outcome=="heart failure") {
				full_data <- full_data[!full_data[,17]=="Not Available",]
				full_data[,17] <- as.numeric(full_data[,17])
				17
			} else {
				full_data <- full_data[!full_data[,23]=="Not Available",]
				full_data[,23] <- as.numeric(full_data[,23])
				23
			}
	
	## pulls out a list of states...don't end up using this
	state_list <- sort(unique(full_data$State))
	
	## splits the dataset by state, and crops it to three columns of interest
	state_data <- split(full_data[,c(2,7,column)],full_data$State)
	## sorts the split data by the outcome measure and hospital name (for ties)
	state_data <- lapply(state_data, function(x) x[order(x[3], x[1]),])			
	
	## If/else code for the different "num" options
	## If num is best, loops through each state and retrieves the first hospital
	if (num=="best") {
		hospital <- lapply(state_data, function(x) x[1,1])
	} else if (num=="worst") {					## if worst, grabs the last hospital, using nrow
		hospital <- lapply(state_data, function(x) x[nrow(x),1])
	} else if (is.numeric(num)) {				## if it's a number, grab that row's hospital (returns NA if nothing found)
		hospital <- lapply(state_data, function(x) x[num,1])
	}
	
	## The resulting "hospital" is a list of 54 elements (states), each with a single value (hospital name)
	## Turning this into a data frame, I unlist the hospitals specifically as column A, and use the names of the lists (states) for column B)
	## Somehow this also named each row as the state, which is what I wanted anyway
	results <- data.frame(hospital=unlist(hospital),state=names(hospital))
	results

}