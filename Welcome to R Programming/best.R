#The function reads the outcome-of-care-measures.csv file and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state.  
best <- function(state, outcome="heart attack") {
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
    
    ## Return hospital name in that state with lowest 30-day death
  
   
    if(outcome == "heart attack")
    {
        outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        vals<-subset(lmn, lmn$State==state)
        minval<-min(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
        lowhosps<-subset(vals$Hospital.Name, vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minval)
        
        if(length(lowhosps) > 1)
        {
            print(paste("Multiple values detected:", length(lowhosps)))
            return (lowhosps[1])
        }else
        {
            return (lowhosps)
        }
        
    }else if(outcome == "heart failure")
    {
        outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        vals<-subset(lmn, lmn$State==state)
        minval<-min(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm=TRUE)
        lowhosps<-subset(vals$Hospital.Name, vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minval)
        
        if(length(lowhosps) > 1)
        {
            print(paste("Multiple values detected:", length(lowhosps)))
            return (lowhosps[1])
        }else
        {
            return (lowhosps)
        }
     
    }else if(outcome == "pneumonia")
    {
        outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        vals<-subset(lmn, lmn$State==state)
        minval<-min(vals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm=TRUE)
        lowhosps<-subset(vals$Hospital.Name, vals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minval)
        
        if(length(lowhosps) > 1)
        {
            print(paste("Multiple values detected:", length(lowhosps)))
            return (lowhosps[1])
        }else
        {
            return (lowhosps)
        }
    }
        
       
        
  
  
    
  
    
    #sort.int(outcome_sort_ha)
    #outcome_sort_ha<-order(outcome_subset$`heart attack`, decreasing = TRUE)
    #index<-with(outcome, order(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.last = TRUE))
    #index<-with(outcome, order(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome$Hospital.Name, na.last = TRUE))
    #lmn<-outcome[index,]
    #subset(lmn$Hospital.Name, lmn$State=="TX")[1]
    #states<-levels(factor(lmn$State))
    #"CA" %in% states
    #min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
    ## rate
    #vals<-subset(lmn, lmn$State==state)
    #minval<-min(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
    #lowhosps<-subset(vals$Hospital.Name, vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minval)
}

