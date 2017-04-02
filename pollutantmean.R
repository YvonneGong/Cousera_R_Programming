###Coursera, R programming, week2, assignement 
##Part-1
##Write a function named 'pollutantmean' that calculates the mean of a pollutant
##(sulfate or nitrate) across a specified list of monitors. 
##The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', 
##and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
##particulate matter data from the directory specified in the 'directory' argument and 
##returns the mean of the pollutant across all of the monitors, ignoring any 
##missing values coded as NA. 

setwd("/Users/YvonneGong/Documents/Spring_2017/Coursera/R_programming/Assignment/Week2")##set working directory
pollutantmean<-function(directory, pollutant, id=1:332){
    setwd(file.path(getwd(),directory))
    total=0
    observations=0
    for (i in id) ##complecate because the names of some files start with 0
    {
        if (i<10){
            data<-read.csv(paste("0","0",as.character(i),".csv",sep =""),
                           header=T,
                           na.strings = c("NA","NaN", " "))
        }
        else if (i>=10&i<100){
            data<-read.csv(paste("0",as.character(i),".csv",sep = ""),
                           header = T,
                           na.strings=c("NA","NaN"," "))
        }
        else{
            data<-read.csv(paste(as.character(i),".csv",sep=""),
                           header=T,
                           na.strings = c("NA","NaN"," "))
        }
        data=na.omit(data) ##remove all the rows with na
        observations=observations+nrow(data) ##compute the number of complete rowa
        if (pollutant=="sulfate"){total=total+sum(data$sulfate)}
        else {total=total+sum(data$nitrate)}
    }
    return (total/observations) ##calculate mean 
}

##examples

pollutantmean("specdata", "sulfate", 1:10) == 4.064

