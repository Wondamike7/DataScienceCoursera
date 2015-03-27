## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used
        
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases


complete <- function(directory, id = 1:332) {
 	# pull all of the files in the given directory
	files <- list.files(directory, full.names=TRUE) 
	
	# create blank data frame
	full_data <- data.frame() 

	# Loop through each file and pull the necessary data
	# need the id and the number of complete cases (nrow of subset)
	# bind all of these into the data frame established above.
	for (i in id) {
		new_data <- read.csv(files[i])
		x <- i
		y <- nrow(new_data[complete.cases(new_data),])
		full_data <- rbind(full_data, c(x,y))
	}

	# Set column names to make it pretty
	colnames(full_data) <- c("id","nobs")
	
	# finish with the data frame as the thing that is exported from the function
	full_data
}

# according to search, using rbind inside of a loop is not very efficient...
# could potentially start it as a matrix - numrows == length(id), numcols == 2
# then transform the matrix into a data frame?