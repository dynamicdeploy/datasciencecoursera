rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomev <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    suppressWarnings(  outcomev[, 11] <- as.numeric(outcomev[, 11]))
    suppressWarnings(outcomev[, 17] <- as.numeric(outcomev[, 17]))
    suppressWarnings(outcomev[, 23] <- as.numeric(outcomev[, 23]))
    
    ## Check that state and outcome are valid
    outcomestrings<-c("heart attack", "heart failure", "pneumonia")
    states<-levels(factor(outcomev$State))
   
    states<-append(states, NA)
    if(!(outcome %in% outcomestrings)) 
    {
        stop("invalid outcome")
        return()
    }
    
    #find number of hospitals in the state and check if rank > number
   
    returncolumns<-c("hospital", "state", "Rank")
    finalcolumns<-c("hospital", "state")
    
    #outputdf = data.frame("hospital"=character(), "state" = character(), stringsAsFactors = FALSE)
    outputdf = data.frame()
    if(outcome == "heart attack")
    {
        for(s in states)
        {
            if(is.numeric(num))
            {
                totalhospitalsinstate<-length(subset(outcomev$Hospital.Name, outcomev$State==s))
                if(totalhospitalsinstate < num)
                {
                    #outputdf<-append(outputdf, c(NA, s))
                    outputdf[nrow(outputdf)+1,]<-c(NA, s)
                    next
                }
            }
            #for each state rank the hospitals 
            outcomev <- subset(outcomev, !is.na(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
            index<-with(outcomev, order(outcomev$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomev$Hospital.Name, na.last = TRUE))
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
            names(vals)[names(vals) == 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'] <- 'rate'
            #return the vals
            vals<-vals[returncolumns]
            
            #get the hospitals with the specified rank
            #add it to the output dataframe
            if(is.numeric(num))
            {
                tmp<-subset(vals, vals$Rank == num)
                
               # outputdf<-append(outputdf, tmp[finalcolumns]) 
                outputdf<-rbind(outputdf, tmp)
              
                
            }else if(num == "best")
            {
               tmp<- head(vals, 1)
               #outputdf<-append(outputdf, tmp[finalcolumns]) 
               outputdf<-rbind(outputdf, tmp)
            
                
            }else if(num == "worst")
            {
                tmp<- tail(vals, 1)
                #outputdf<-append(outputdf, tmp[finalcolumns]) 
                outputdf<-rbind(outputdf, tmp)
                
            }
            
            
        }
      
        
    }else if(outcome == "heart failure")
    {
       
    }else if(outcome == "pneumonia")
    {
       
    }
    
    y<-outputdf[, 1:2]
    return(y)
   
    #return(outputdf)
}
#tmpp<-by(outcome, outcome$State, function(x) x[with(x, order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, x$Hospital.Name, na.last = TRUE)),])
#tmpp<-by(outcome, outcome$State, function(x) subset(x[with(x, order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, x$Hospital.Name, na.last = TRUE)),], !is.na(vals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
#tmpp$AK$Rank<-NA
#tmpp$AK$Rank<-1:nrow(tmpp$AK)
#xyz<-by(outcome, outcome$State, function(x) x[which.min(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ] )
#lapply(xyz, function(x){ x[c("Hospital.Name", "State")]})

rankbyoutcome<-function(outcomedf, outcome)
{
    
}