complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  nobs <- numeric()
  for (select_id in id){
    file <- paste(directory, "/", sprintf("%03d", select_id), ".csv", sep = "")
    file_data <- read.csv(file)
    nobs <- c(nobs, sum(complete.cases(file_data)))
  }
           
  data.frame(id, nobs)
}