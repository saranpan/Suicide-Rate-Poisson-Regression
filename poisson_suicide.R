
#  title: "Suicide rate using Poisson regression "

  
#Import data

setwd("C:/Users/Wallik/Desktop/data")
data <- read.csv('project/who_suicide_statistics.csv')

attach(data)

#Check missing value
print('Total missing value : ',sum(is.na(data)))

na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

print('missing value of each column')
print(na_count)

# (Default) Dropping the missing value
data = na.omit(data)

