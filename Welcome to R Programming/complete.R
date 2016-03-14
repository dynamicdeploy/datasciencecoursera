complete<-function(directory, id=1:332)
{
  
    dat<-data.frame()
   # names(dat) <- c("id", "nobs")
    for(i in id)
    {
     
        n<-completesingle(directory, i)
        dat <- rbind(dat, n)
        
    }
    dat
}

completesingle<-function(directory="specdata", id=1)
{
    #Generate file name from the directory and provided id
    filen<-paste(directory, "/", sprintf("%03s", id), ".csv", sep = "")
    #read the file
    #print(paste("Reading file...", filen))
    m<-read.csv(filen)
    #Calculate complete records
    j<-subset(m$ID, !is.na(m$sulfate) & !is.na(m$nitrate))
    adf<-data.frame(id=id, nobs=length(j))
}
