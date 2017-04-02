###Coursera, R programming, week2, assignement 
##Part_2
##Write a function that reads a directory full of files and reports 
##the number of completely observed cases in each data file. 
##The function should return a data frame where the first column is 
##the name of the file and the second column is the number of complete cases. 
##A prototype of this function follows

setwd("/Users/YvonneGong/Documents/Spring_2017/Coursera/R_programming/Assignment/Week2")##after every run, tje wd should be reset
complete<-function(directory,id=1:332){
    setwd(file.path(getwd(),directory))
    dataframe=NULL
    for (i in id){
        if (i<10){
            data<-read.csv(paste("0","0",as.character(i),".csv", sep=""),
                           header = T,
                           na.strings=c("NA","NaN",""))
        }
        else if (i>=10&i<100){
            data<-read.csv(paste("0",as.character(i),".csv", sep=""),
                           header = T,
                           na.strings=c("NA","NaN",""))
        }
        else {
            data<-read.csv(paste(as.character(i),".csv",sep=""),
                           header=T,
                           na.strings = c("NA","NaN"," "))
        }
        number<-nrow(data[complete.cases(data),]) ##get the number of complete row
        dataframe<-rbind(dataframe,c(i,number)) ##
    }
    #names(dataframe)<-c('id','nobs')
    return (dataframe)
}

##examples
# tests
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)