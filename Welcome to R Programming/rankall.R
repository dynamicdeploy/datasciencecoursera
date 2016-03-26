## This function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates.

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomev <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(  outcomev[, 11] <- as.numeric(outcomev[, 11]))
    suppressWarnings(outcomev[, 17] <- as.numeric(outcomev[, 17]))
    suppressWarnings(outcomev[, 23] <- as.numeric(outcomev[, 23]))
    
    ## Check that state and outcome are valid
    outcomestrings<-c("heart attack", "heart failure", "pneumonia")
    states<-levels(factor(outcomev$State))
    
    # states<-append(states, NA)
    if(!(outcome %in% outcomestrings)) 
    {
        stop("invalid outcome")
        return()
    }
    
    #find number of hospitals in the state and check if rank > number
    
   outcomearray<-c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
  
  
    if(outcome == "heart attack")
    {
       
        outputdf<- rankbyoutcome(outcomev, outcomearray[1], states=states, num=num)
        
    }else if(outcome == "heart failure")
    {
        outputdf<- rankbyoutcome(outcomev, outcomearray[2], states=states, num=num)
    }else if(outcome == "pneumonia")
    {
        outputdf<- rankbyoutcome(outcomev, outcomearray[3], states=states, num=num)
    }
    
    y<-outputdf[, 1:2]
    return(y)
    
}

rankbyoutcome<-function(outcomev, outcomestring, states, num)
{
    returncolumns<-c("hospital", "state", "Rank")
    outputdf<-data.frame(hospital=as.character(""), state=as.character(""), Rank=as.numeric(0), stringsAsFactors = FALSE)
    for(s in states)
    {
        if(is.numeric(num))
        {
            totalhospitalsinstate<-length(subset(outcomev$Hospital.Name, outcomev$State==s))
            if(totalhospitalsinstate < num)
            {
                #because rbind() was messing up the column names, I had to use this approach
                ##http://stackoverflow.com/questions/5231540/r-losing-column-names-when-adding-rows-to-an-empty-data-frame 
                outputdf[nrow(outputdf)+1,]<-c(NA, s, NA)
                next
            }
        }
        #for each state rank the hospitals 
        outcomev <- subset(outcomev, !is.na(outcomev[outcomestring]))
        index<-with(outcomev, order(outcomev[outcomestring], outcomev$Hospital.Name, na.last = TRUE))
        lmn<-outcomev[index,]
        #filter by state and where outcome is not NA
        vals<-subset(lmn, lmn$State==s)
        #add a rank column
        vals$Rank <- NA
        #Add ranks to the Rank column
        vals$Rank <- 1:nrow(vals)
        
        #Change the column name of the outcome to Rank
        names(vals)[names(vals) == 'Hospital.Name'] <- 'hospital'
        names(vals)[names(vals) == 'State'] <- 'state'
        names(vals)[names(vals) == outcomestring] <- 'rate'
        #return the vals
        vals<-vals[returncolumns]
        
        #get the hospitals with the specified rank
        #add it to the output dataframe
        if(is.numeric(num))
        {
            tmp<-subset(vals, vals$Rank == num)
            outputdf<-rbind(outputdf, tmp)
            
            
        }else if(num == "best")
        {
            tmp<- head(vals, 1)
            #outputdf<-append(outputdf, tmp[finalcolumns]) 
            outputdf<-rbind(outputdf, tmp)
            
            
        }else if(num == "worst")
        {
            tmp<- tail(vals, 1)
            outputdf<-rbind(outputdf, tmp)
            
        }
        
        
    }
    #because we injected a fake row while creating the dataframe
    return (outputdf<-outputdf[-1,])
}

##Some interesting commands learned
#tmpp<-by(outcome, outcome$State, function(x) x[with(x, order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, x$Hospital.Name, na.last = TRUE)),])
#tmpp<-by(outcome, outcome$State, function(x) subset(x[with(x, order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, x$Hospital.Name, na.last = TRUE)),], !is.na(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
#tmpp$AK$Rank<-NA
#tmpp$AK$Rank<-1:nrow(tmpp$AK)
#xyz<-by(outcome, outcome$State, function(x) x[which.min(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ] )
#lapply(xyz, function(x){ x[c("Hospital.Name", "State")]})