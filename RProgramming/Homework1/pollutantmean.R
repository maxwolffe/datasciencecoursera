pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
    
  average <- numeric()
  count <- 0
  
  for (select_id in id){
    file <- paste(directory, "/", sprintf("%03d", select_id), ".csv", sep = "")
    file_data <- read.csv(file)
    if (pollutant == "nitrate"){
      average <- c(average, t(file_data[3][t(complete.cases(file_data[3]))]))
    }
    else {
      average <- c(average, t(file_data[2][t(complete.cases(file_data[2]))]))
    }  
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  print(mean(average))
}
