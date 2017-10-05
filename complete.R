complete <- function(directory, id = 1:332) {
        #Read all files, creating a list
        temp <- list.files(directory, full.names = TRUE)
        #Data frame to use to bind later on
        tempframe <- data.frame()
        nobs <- vector()
        #Loop to create the data frame with all NA's values omitted
        #At the same time saves the amount of rows read for the i-th ID
        for (i in id){ 
                
                nobs[i] <- nrow(na.omit(read.csv(temp[i])))
                tempframe <- rbind(tempframe, na.omit(read.csv(temp[i])))
                #Must be fixed the cases where nobs = 0 because tempframe
                #It's dropping that level, making tempframe have less rows
                #Than nobs
                #As well fixing the orer, given that id = 332:1 (decreasing)
                
        }
        #Modify the class of ID to factor to be usable
       tempframe$ID <- as.factor(tempframe$ID)
       #Create data frame that's going to be outputted
       data.frame(id = levels(tempframe$ID), nobs = na.omit(nobs))
      
}