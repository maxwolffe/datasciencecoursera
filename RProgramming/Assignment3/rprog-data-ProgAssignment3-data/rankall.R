rankall <- function(outcome, num = "best"){
  setwd("/Users/maxwolffe/Documents/Coursera/datasciencecoursera/RProgramming/Assignment3/rprog-data-ProgAssignment3-data/")
  data_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(is.element(state, data_set$State))){
    stop("invalid state")
  }
  
  if (!(is.element(outcome, c("heart attack", "heart failure", "pneumonia")))) {
    stop("invalid outcome")
  }
  
  states <- unique(data_set$State)
  states <- states[order(factor(states))]
  hospital <- c()
  
  decrease <- FALSE
  if (num == "worst"){
    decrease <- TRUE
  }
    
  for (state in states){
    
    state_rows <- data_set[data_set$State == state ,]
    sorted_state <- state_rows[order(factor(state_rows$"Hospital.Name")),]
    
    if (outcome == "heart attack"){
      order_factor <- factor(as.numeric(sorted_state$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
      best_hop <- sorted_state[order(order_factor, decreasing = decrease),]
      best_hop <- best_hop[!is.na(as.numeric(best_hop[,11])),]
    }
    else if (outcome == "heart failure"){
      order_factor <- factor(as.numeric(sorted_state$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
      best_hop <- sorted_state[order(order_factor, decreasing = decrease),]
      best_hop <- best_hop[!is.na(as.numeric(best_hop[,17])),]
    }
    else{
      order_factor <- factor(as.numeric(sorted_state[,23]))
      best_hop <- sorted_state[order(order_factor, decreasing = decrease),]
      best_hop <- best_hop[!is.na(as.numeric(best_hop[,23])),]
    }
    
    if (num == "best" || num == "worst"){
      num <- 1
    }
    
    if (num > nrow(best_hop)) {
       hospital <- c(hospital, NA)
    }
    else {
      best_hop <- best_hop[num,]
      hospital <- c(hospital, best_hop$"Hospital.Name")
    }
  }
  
  state <- states
  data.frame(hospital, state)
}
