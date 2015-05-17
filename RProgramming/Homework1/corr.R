corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  correlations <- numeric()
  
  id = 1:332
  
  for (select_id in id){
    file <- paste(directory, "/", sprintf("%03d", select_id), ".csv", sep = "")
    file_data <- read.csv(file)
    complete_data <- complete.cases(file_data)
    if (sum(complete_data) > threshold){
      correlations <- c(correlations, cor(file_data[2][t(complete_data)], file_data[3][t(complete_data)]))
    }
  }

  correlations
}