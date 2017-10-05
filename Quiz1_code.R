library(plyr)
test1 <- read.csv("hw1_data.csv")
colnames(test1)
test1[1:2,]
test1[152:153,]
test1$Ozone[47]
missing <- is.na(test1$Ozone)
count(missing == TRUE)
mean(test1$Ozone[!missing])
hightemp <- subset(test1, test1$Ozone > 31 & test1$Temp > 90)
month <- subset(test1, test1$Month == 6)
mean(month$Temp)
month2 <- subset(test1, test1$mMonth == 5)
which.max(month2$Ozone)
month2$Ozone[30]