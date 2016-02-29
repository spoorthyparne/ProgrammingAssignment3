best <- function(state, outcome) {
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
  if (!valid_state) {
    stop ("invalid state")
  } 
  
  if(missing(outcome) || (tolower(outcome) != "heart attack") && (tolower(outcome) != "heart failure") && (tolower(outcome) != "pneumonia")) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  col_no <- 0
  if(tolower(outcome) == "heart attack") {
    col_no <- 11
  } else if (tolower(outcome) == "heart failure") {
    col_no <- 17
  } else {
    col_no <- 23
  }
  
  state_data <- data[data$State == state,]
  subset_data <- state_data[,c(2,col_no)]
  sorted_data <-  suppressWarnings(subset_data[order(as.numeric(subset_data[,2]),subset_data[,1], na.last = TRUE), ])
  sorted_data[1,1]
    
}
  