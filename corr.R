corr <- function(directory, threshold = 0) {
        #Creates the list with all the directories of the files
        temp <- list.files(directory, full.names = TRUE)
        #Temporal data frame and vectors that are going to be used later on
        tempframe <- data.frame()
        nobs <- vector()
        corre <- vector()
        #loop with a nested if that first creates the lenght of the ith
        #so that it can be compared with the threshold
        #inside the if i bind the columns that are gonna be used to be calculate
        #the correlations, that way it calculates the correlation everysingle
        #iteration
        for (i in 1:332) { 
                nobs[i] <- nrow(na.omit(read.csv(temp[i])))
                tempframe <- rbind(tempframe, na.omit(read.csv(temp[i])))
                if (nobs[i] > threshold){ 
                        sulfa <- subset.data.frame(tempframe, tempframe[,4] == i, 2)
                        nitra <- subset.data.frame(tempframe, tempframe[,4] == i, 3)
                        corre<-c(corre,cor(sulfa,nitra))}
       else { #If the threshold is bigger than the number of observations
               #then it keeps the vector to how it was before
               corre 
               }
        }
        #Output is the vector "corre" made from the iterations of the previous loop
        corre
        
}