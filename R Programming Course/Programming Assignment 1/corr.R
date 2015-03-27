## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
	all_data <- complete(directory,1:332)
	subset_data <- all_data[all_data$nobs>threshold,]
	id_vector <- subset_data[,1]
	
	files <- list.files(directory, full.names=TRUE)
	cor_vector <- c()
	for (i in id_vector) {
		new_data <- read.csv(files[i])
		cor_new <- cor(new_data[,2],new_data[,3],use = "complete.obs")
		cor_vector <- append(cor_vector, cor_new)
	}	
	cor_vector
}