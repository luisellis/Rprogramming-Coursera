## coursera R Programming course, assignment number 3
## This one is based on the hospital data. 
##Documents names in the John Jockins university
## 1- Hospital_Revised_Flatfiles.pdf
## 2- hospital-data.csv
## 3- outcome-of-care-measure.csv
## Instructions are on the PDF named ProgAssignment3.pdf
## Breakdown:
setwd("C:/Users/luisv/OneDrive/Documents/R/R programming - John Jopkins")
## Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)## We see that is 46 variables making it a large data frame
## First make it numeric since we read them as characters, then histogram
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

##Finding the best hospital in a state based on two inputs the state and the 
#outcome where the outcome is one of the column variables
# i.e. (heart attack, hear failure, pneumonia)
# Noting that the best is the one with the lowest mortality

best <- function(state, outcome){
        data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        ##Columns 11(heart attac), 17(h. fail) and 23 have the rates to compare
        sub <- subset(data, data$State == state)
     
        if (outcome == "heart attack"){
                OU <- sub[,c(2,7,11)]
        }
        if (outcome == "heart failure"){
                OU <- sub[,c(2,7,17)]
        }
        if (outcome == "pneumonia") {
                OU <- sub[,c(2,7,23)]
        }
       #which.min function coulda've been a better choice
        vector<-subset(OU , OU[,3] == min(as.numeric(OU[,3]), na.rm = TRUE))
        vector[,1]
}

# Exercise 2 is to create a function that gives you the name of the hospital with
# the nth rank especified by user. Do the same with ties (thing I didn't do)

rankhospital<- function(state, outcome, num = 1){
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
        sub <- subset(data, data$State == state)
        
        if (outcome == "heart attack"){
                OU <- sub[,c(2,7,11)]
        }
        if (outcome == "heart failure"){
                OU <- sub[,c(2,7,17)]
        }
        if (outcome == "pneumonia") {
                OU <- sub[,c(2,7,23)]
        }
        OUsort <-na.omit(OU[order(as.numeric(OU[,3]),OU[,1]),])
        OUsort[num,]
#It is essencially done with some things that gotta be improved, like the sorting
#by alphabetical order and adding cases of "best" and "worst"        
}

# EXercise 3 is to create a function that given a rank number, goes through each
# state and show for every single state, the corresponding hospital for that rank

rankall <- function(outcome, num = 1) {
        #Thsi function still has to be improved to make it give the worst and the best
        #as well to decides ties in alphabetical order -- DONE, Just sort them by name
        #as a last parameter, applied to all other functions.
        library(plyr)
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
        if (outcome == "heart attack"){
                arranged <- arrange(data, State,
                                  as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                                  Hospital.Name)
        }
        if (outcome == "heart failure"){
                arranged <- arrange(data, State, 
                                  as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                                  Hospital.Name)
        }
        if (outcome == "pneumonia") {
                arranged <- arrange(data, State, 
                                    as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                                    Hospital.Name)
        }
      
          lallstates <- split(arranged,arranged$State)
          result <- do.call(rbind, lapply(lallstates, 
                                          function(x) {x[num, c(2,7)]}))
          result
}
