rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
  ## Check that state and outcome are valid
  if(missing(outcome) || (tolower(outcome) != "heart attack") && (tolower(outcome) != "heart failure") && (tolower(outcome) != "pneumonia")) {
    stop("invalid outcome")
  }
  
  col_no <- 0
  if(tolower(outcome) == "heart attack") {
    col_no <- 11
  } else if (tolower(outcome) == "heart failure") {
    col_no <- 17
  } else {
    col_no <- 23
  }
  subset_data <- data[,c(2,7,col_no)]
  states <- unique(subset_data[,2])
  states <- sort(states)
  
  pos <- 0
  if(num == "best") {
    pos <- 1
  } else {
    pos <- num
  }
  
  
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  output <- data.frame(hospital=NA, state=NA)
  for (i in 1:length(states)) {
    #print(states[i])
    state_data <- subset_data[subset_data$State == states[i], ]
    
    sorted_data <-  suppressWarnings(state_data[order(as.numeric(state_data[,3]),as.character(state_data[,1]), na.last = NA), ])
    
    if(num == "worst") {
      pos <- nrow(sorted_data)
    }
    output[i,] <- c(sorted_data[pos,1], states[i])
  }
  
  output

}