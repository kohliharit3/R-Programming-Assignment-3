## 1) Obtain the 3 column data frame (hos_name, state, outcome)
## 2) Split it into individual states
## 3) For each state-data frame, find the req ranked hos_name.




rankall <- function (outcome, num = "best") {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        checkvalid(outcome = outcome)
        
        column <- find_outcome(outcome = outcome)
        print(column)
        
        selected <- data[, c(2, 7, column)]
        
        
        selected[,3] <- as.numeric(selected[,3])
        selected <- na.omit(selected)
        
        
        results <- data.frame(Name = character(), State = character())
        
        results <- rbind(results, lapply(split(selected, selected[,2]), FUN = ordered_data, num))
        
        colnames(results) <- rep(NA)
        
        final <- data.frame(Hospital_Name = character(), State = character())
        
        
        i = 1
        while (i <= 107) {
                
                row_i <- cbind(results[1, i], results[1,i+1])
                colnames(row_i) <- c("Hospital_Name", "State")
                
                final <- rbind(final, row_i)
                
                i <- i + 2
        }
        final

        
        
}