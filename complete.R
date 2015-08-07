complete<-function(directory, Idd=1:332) {
  
  newdata<-data.frame(id=NULL, nobs=NULL)
  for (n in Idd) {
  num<-0  
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
  for (j in 1:nrow(dataexam1)) {
    if ( !is.na(dataexam1[j,2]) & ( !is.na(dataexam1[j,3]))) 
     {num<-num+1}
     
  }
    if (num>=0 )
   { newdata<-rbind(newdata, data.frame(id=n,nobs=num))
    }
 
    
  } 
  
  newdata
 }
  
