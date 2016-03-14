corr<-function(directory, threshold=0){
    
    readallmonitorswithinthreshold(directory=directory, threshold=threshold)
}

readallmonitorswithinthreshold<-function(directory="specdata",id=1:332, threshold=0)
{
   ids<-readallobswithreshold(directory, id, threshold)
   v<-vector('numeric')
   if(length(ids) > 0)
   {
      
       for(i in ids)
       {
           filen<-paste(directory, "/", sprintf("%03s", i), ".csv", sep = "")
           m<-read.csv(filen)
           #Calculate complete records
           j<-subset(m, !is.na(m$sulfate) & !is.na(m$nitrate))
           cr<-cor(j$sulfate, j$nitrate)
           v<-append(v,cr)
       }
       v
   }else
    {
        v
    }
}

readallobswithreshold<-function(directory, id=1:332, threshold=0){
    
    j_sub<-complete(directory, id) 
    j_subset<-subset(j_sub, j_sub$nobs > threshold)
    j_subset$id
}