best <- function(state, outcome) {
  setwd("/Users/maxwolffe/Documents/Coursera/datasciencecoursera/RProgramming/Assignment3/rprog-data-ProgAssignment3-data/")
  data_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(is.element(state, data_set$State))){
    stop("invalid state")
  }
  
  if (!(is.element(outcome, c("heart attack", "heart failure", "pneumonia")))) {
    stop("invalid outcome")
  }
  
  state_rows <- data_set[data_set$State == state ,]
  sorted_state <- state_rows[order(factor(state_rows$"Hospital.Name")),]
  
  if (outcome == "heart attack"){
    best_hop <- suppressWarnings(sorted_state[which.min(sorted_state[,11]),])
  }
  else if (outcome == "heart failure"){
    best_hop <- suppressWarnings(sorted_state[which.min(sorted_state[,17]),])
  }
  else{
    best_hop <- suppressWarnings(sorted_state[which.min(sorted_state[,23]),])
  }
  
  best_hop$"Hospital.Name"
}