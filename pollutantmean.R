pollutantmean<-function(directory, pollutant, Id=1:332){
  summation<-0
  numberofrows<-0
  for (n in Id){
    
    if (n <10)  { 
      filepath<-paste(directory, "/" ,"00" ,n, ".csv", sep="")
    }
    
    else{
      
     if  (n<100)  {filepath<-paste(directory,"/","0",n, ".csv", sep="")}
    
    
    else {
      filepath<-paste(directory,"/",n,".csv", sep="")
    }
    }
    dataexam1<-read.csv(filepath)
    summation<-summation + sum(dataexam1[,pollutant],na.rm=TRUE)
   numberofrows<-numberofrows+NROW(na.omit(dataexam1[,pollutant]))
    }
 summation/numberofrows
 
}