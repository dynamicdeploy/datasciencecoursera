rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomev <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(  outcomev[, 11] <- as.numeric(outcomev[, 11]))
    suppressWarnings(outcomev[, 17] <- as.numeric(outcomev[, 17]))
    suppressWarnings(outcomev[, 23] <- as.numeric(outcomev[, 23]))
    
    ## Check that state and outcome are valid
    outcomestrings<-c("heart attack", "heart failure", "pneumonia")
    states<-levels(factor(outcomev$State))
    if(!(state %in% states)) 
    {
        stop("invalid state")
        return()
    }
    if(!(outcome %in% outcomestrings)) 
    {
        stop("invalid outcome")
        return()
    }
    
    #find number of hospitals in the state and check if rank > number
    if(is.numeric(num))
    {
        totalhospitalsinstate<-length(subset(outcomev$Hospital.Name, outcomev$State==state))
        if(totalhospitalsinstate < num)
        {
            return (NA)
        }
    }
    returncolumns<-c("Hospital.Name", "Rate", "Rank")
    
    if(outcome == "heart attack")
    {
        outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        #filter by state and where outcome is not NA
        vals<-subset(lmn, lmn$State==state)
        vals<-subset(vals, !is.na(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        #add a rank column
        vals$Rank <- NA
        #Add ranks to the Rank column
        vals$Rank <- 1:nrow(vals)
        #Change the column name of the outcome to Rank
        names(vals)[names(vals) == 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'] <- 'Rate'
        #return the vals
        vals<-vals[returncolumns]
        if(is.numeric(num))
        {
            return (subset(vals, vals$Rank == num)$Hospital.Name)
        }else if(num == "best")
        {
            return (vals[1,1])
        }else if(num == "worst")
        {
            return(vals[length(vals$Hospital.Name), 1])
        }
        
    }else if(outcome == "heart failure")
    {
        outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        #filter by state and where outcome is not NA
        vals<-subset(lmn, lmn$State==state)
        vals<-subset(vals, !is.na(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        #add a rank column
        if(!("Rank" %in% names(vals)))
        {
            vals$Rank <- NA
        }
        #Add ranks to the Rank column
        vals$Rank <- 1:nrow(vals)
        #Change the column name of the outcome to Rank
        names(vals)[names(vals) == 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'] <- 'Rate'
        #return the vals
        vals<-vals[returncolumns]
        if(is.numeric(num))
        {
            return (subset(vals, vals$Rank == num)$Hospital.Name)
        }else if(num == "best")
        {
            return (vals[1,1]$Hospital.Name)
        }else if(num == "worst")
        {
            return(vals[length(vals$Hospital.Name), 1])
        }
        
    }else if(outcome == "pneumonia")
    {
        outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        #filter by state and where outcome is not NA
        vals<-subset(lmn, lmn$State==state)
        vals<-subset(vals, !is.na(vals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        #add a rank column
        vals$Rank <- NA
        #Add ranks to the Rank column
        vals$Rank <- 1:nrow(vals)
        #Change the column name of the outcome to Rank
        names(vals)[names(vals) == 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'] <- 'Rate'
        #return the vals
        #return the vals
        vals<-vals[returncolumns]
        if(is.numeric(num))
        {
            return (subset(vals, vals$Rank == num)$Hospital.Name)
        }else if(num == "best")
        {
            return (vals[1,1]$Hospital.Name)
        }else if(num == "worst")
        {
            return(vals[length(vals$Hospital.Name), 1])
        }
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    #transform(valsn, rank = ave(valsn$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, FUN = function(x) rank(-x, ties.method = "first")))
}
