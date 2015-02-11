# Assignment 1
pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:10) {
        wd<<-paste("/Users/lorenz/Documents/Coursera/Programming with R - Signature Track/", directory, sep = "")
        setwd(wd)
        vector<-NA
        # create names for the importing files
        name<-numeric(length(id))
        for(i in 1:length(id)) {
                if ( id[i] <= 9 ) {
                        name[i]<-paste("00", id[i], ".csv", sep = "")
                } else if ( id[i] > 9 & id[i] < 99) {
                        name[i]<-paste("0", id[i], ".csv", sep = "")
                }
        }

        test<-numeric(length(id))
        test<-as.list(test)
        
        # import the data to a list
        for (i in 1:length(id)) {
                test[[i]]<-read.csv(name[i], sep = ",", header = T)
        }
        
        
        for(i in 1:length(id)) {
                vector <- append(vector, test[[i]] [[pollutant]])
        }
        mean(vector, na.rm = T)
}
