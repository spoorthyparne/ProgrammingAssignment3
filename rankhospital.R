rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
  ## Check that state and outcome are valid
  if(missing(state) || nchar(state) != 2 || (!is.character(state))) {
    stop("invalid state")
  }
  
  all_states <- data[,7]
  valid_state <- FALSE
  for (i in 1:length(all_states)) {
    if (state == all_states[i]) {
      valid_state <- TRUE
    }
  }
  if (!valid_state && num == "best") {
    stop ("invalid state")
  } 
  
  if(missing(outcome) || (tolower(outcome) != "heart attack") && (tolower(outcome) != "heart failure") && (tolower(outcome) != "pneumonia") && num == "best") {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  col_no <- 0
  if(tolower(outcome) == "heart attack") {
    col_no <- 11
  } else if (tolower(outcome) == "heart failure") {
    col_no <- 17
  } else {
    col_no <- 23
  }
  
  state_data <- data[data$State == state, ]
  subset_data <- state_data[,c(2,col_no)]
  subset_data
  sorted_data <-  suppressWarnings(subset_data[order(as.numeric(subset_data[,2]),as.character(subset_data[,1]), na.last = NA), ])

  
  if(num == "best") {
    return(sorted_data[1,1])
  } else if(num == "worst") {
    return (sorted_data[nrow(sorted_data),1])
  } else {
    return(sorted_data[num, 1])
  }
  
  if(num > nrow(sorted_data)) {
    return("NA")
  }
  ## 30-day death rate
}