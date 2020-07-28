ordered_data <- function(x, num) {
        
        x[,3] <- as.numeric(x[,3])
        
        final <- x[order(x[,3], x[,1]), ]
        
        if (num == "best") {
                
                final[1,c(1,2)]
                
        }
        
        else if (num == "worst") {
                
                final[length(final[,1]), c(1,2)]
                
        }
        
        else if (num > length(final[,1])) {
                
                y <- as.data.frame(matrix(c(NA, final[1,2]), nrow = 1, ncol = 2))
                return (y)
        
        } else {
                
                final[num, c(1,2)]
                
        }
}