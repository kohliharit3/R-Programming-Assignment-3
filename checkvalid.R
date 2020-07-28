checkvalid <- function(state = raw_stuff$State, outcome = diseases) {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        list_states <- data$State
        list_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        state <- as.character(state)
        outcome <- as.character(outcome)
        
        if (!(state %in% list_states)) {
                
                stop("Not a valid state!")
                
        } else if (!(outcome %in% list_outcomes)) {
                
                stop("Not a valid outcome!")
                
        }
}