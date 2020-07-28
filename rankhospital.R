## RANKING

## Algorithm:
## 1) Get complete data
## 2) Obtain the arguments (state, outcome, rank)
## 3) Make a subset of complete data for the given state 
## 4) From this subset, make a subset with hos_name, and the specific outcome
## 5) Remove all NA values from this, after coercion to numeric
## 6) Sort this subset using the order() [first, by min outcome,
##    then by alphabetical] 

rankhospital <- function(state, outcome, num = "best") {
        
        raw_stuff <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Checking if given state and outcome is valid
        list_states <- raw_stuff[,7]
        list_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        state <- as.character(state)
        outcome <- as.character(outcome)
        
        if (!(state %in% list_states)) {
                
                stop("invalid state")
                
        } else if (!(outcome %in% list_outcomes)) {
                
                stop("invalid outcome")
                
        } else {
                
                state_data <- raw_stuff[which(raw_stuff[,7] == state), ]
                
                if (outcome == list_outcomes[1]) {
                        
                        out <- 11
                        
                } else if (outcome == list_outcomes[2]) {
                        
                        out <- 17
                        
                } else {
                        
                        out <- 23
                }
                ##Now we have the outcome
                
                selected <- state_data[, c(2, out)]
                #This has the relevant state-outcome data
                
                selected[,2] <- as.numeric(selected[,2])
                
                selected <- na.omit(selected)
                #Removing NAs
                
                ranked_list <- selected[order(selected[,2], selected[,1]), ]
                
                if (num == "best") {
                        
                        ranked_list[1, 1]
                        
                } else if (num == "worst") {
                        
                        ranked_list[length(ranked_list[,1]), 1]
                        
                } else if (num > length(ranked_list[,1])) {
                        
                        NA
                        
                } else {
                        
                        ranked_list[num, 1]
                }
        }
}      