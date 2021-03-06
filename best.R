best <- function(state, outcome) {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        data <- na.omit(data)
        
        ## Checking if given state and outcome is valid
        list_states <- data$State
        list_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        state <- as.character(state)
        outcome <- as.character(outcome)
        
        if (!(state %in% list_states)) {
                stop("Not a valid state!")
        }
        
        else if (!(outcome %in% list_outcomes)) {
                stop("Not a valid outcome!")
        }
        
        else {
                state_data <- data[which(data$State == state), ]
                ## We now have a data frame of the state we want
                
                
                if (outcome == list_outcomes[1]) {
                        col <- as.numeric(11)
                }
                
                else if (outcome == list_outcomes[2]) {
                        col <- as.numeric(17)
                }
                
                else {
                        col <- as.numeric(23)
                }
                
                #We now have a valid state name, and we know what column
                #we have to focus on.
                
                selected <- state_data[, c(2, col)]
                
                selected[,2] <- as.numeric(selected[,2])
                selected <- na.omit(selected)
                
                minima <- min(as.numeric(selected[,2]), na.rm = TRUE)
            
                
                final <- selected[which(selected[,2] == minima), 1]
                
                final[1]
                
                
        }
        
        
}