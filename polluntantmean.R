
pollutantmean <- function(directory, polluntant, id = 1:332) {
        ## Read all the files 
        temp <- list.files(path = directory, full.names = TRUE)
        ## create and empty data frame for work on it later on
        tempframe <- data.frame()
        
        for(i in id) { ## loop to save an unique data frame with needed data
                tempframe<- rbind(tempframe, read.csv(temp[i]))
        }
        ##Find the mean
        mean(tempframe[,which(colnames(tempframe) == polluntant)], na.rm = TRUE)
}