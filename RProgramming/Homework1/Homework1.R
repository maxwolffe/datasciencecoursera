pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  setwd(directory)
  
  sum <- 0
  
  for (file in id){
    file_data <- read.csv(file)
    if (pollutant == "nitrate"){
      sum <- sum +  mean(file_data[3])
    }
    else {
      sum <- sum + mean(file[2])
    }
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  sum / nrow(data)
}
