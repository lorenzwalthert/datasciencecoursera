

best<-function(state, outcome) {
        setwd("/Users/lorenz/Documents/Coursera/Programming with R - Signature Track/rprog-data-ProgAssignment3-data")
        raw<-read.csv("outcome-of-care-measures.csv", header = T, sep = ",")
        outc<-array(0)
        if(outcome == "heart attack") {
                outc<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
        } else if (outcome == "heart failure") {
                outc<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
        } else if (outcome == "pneumonia") {
                outc<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               
        } else { stop('invalid outcome')
                ## Create error message
        }
        # if state is valid
        if((state %in% raw$State) == T) {
                
                lis<-subset(raw, select = c("Hospital.Name", outc, "State")) # extract relevant info
                lis<-lis[lis$State == state,] # select relevant state
                
                lis[,2]<-as.character(lis[,2])
                lis[lis == "Not Available"]<-NA # convert "NA" to NA
                
                lis[, 2]<-as.numeric(lis[, 2])
                
                best<-min(lis[, 2], na.rm = T)
                final<<-subset(lis, lis[, 2] == best, select = "Hospital.Name")
                final<<-as.character(as.array(final[,1]))
                print(final)
                
                
                
                
        } else { stop('invalid state')
                print("Error in best(state, outcome) : invalid state")
        }
}


