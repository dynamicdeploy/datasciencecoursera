pollutantmean<-function(directory, pollutant, id=1:332){
    
    dat <- data.frame()
    for(i in id)
    {
        filen<-paste("specdata", "/", sprintf("%03s", i), ".csv", sep = "")
        dat <- rbind(dat, read.csv(filen))
       
    }
    mean(dat[,pollutant], na.rm = TRUE)
}

pollutantsinglemean<-function(directory="specdata", pollutant, id=1){
    
    #Generate file name from the directory and provided id
    filen<-paste("specdata", "/", sprintf("%03s", id), ".csv", sep = "")
    #read the file
    print(paste("Reading file...", filen))
    m<-read.csv(filen)
   
    #Calculate the mean of the pollutant and return
    mean(m[,pollutant], na.rm = TRUE)
}

pollutantmeanwithlapply<-function(directory, pollutant, id=1:332){
    
    dat <- list()
    for(i in id)
    {
        filen<-paste("specdata", "/", sprintf("%03s", i), ".csv", sep = "")
        dat<-append(dat, filen)
       
    }
    tmp<-lapply(dat, read.csv)
    fdat<-do.call(rbind, tmp)
    mean(fdat[,pollutant], na.rm = TRUE)
}