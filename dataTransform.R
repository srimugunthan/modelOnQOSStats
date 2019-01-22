

dataTrans <- function( qosdata ) {
  transformed <- qosdata
  sites <- numeric()
  timestamp <- qosdata[1,]$timestamp
  
  temp <- list()
  temp <- c(temp, as.character(timestamp))
  
  
  for(i in 1:nrow(qosdata)) {
    
    record <- qosdata[i,]
    if(record$timestamp != timestamp)
    {
      break
      
    }
    sites <- append(sites, record$endpoint_name)
    temp <- c(temp, record$in_demand)
    
    
  }
  
  namesSites <- character()
  for(i in 1:length(sites))
  {
    
    namesSites <- c(namesSites, paste("site" ,sites[i],sep=""))
    
  }
  
  
  # print(temp)
  targetset <- as.data.frame(temp)
  names(targetset) <- c("timestamp", namesSites)
  matrclass <- mat.or.vec(nrow(qosdata), length(sites))
  tempdf <- as.data.frame(matrclass)
  names(tempdf) <- namesSites
  
  
  timestamp <- qosdata[1,]$timestamp
  temp2 <- list()
  temp2<- c(temp2, as.character(timestamp))
  thisrecCount=0
  for(rec in 1:nrow(qosdata))
  {
    record <- qosdata[rec,]
    if(record$timestamp == timestamp)
    {
      temp2 <- c(temp2, record$in_demand)
      thisrecCount <- thisrecCount +1
    }
    else
    {
      while( thisrecCount < length(sites))
      {
        temp2 <- c(temp2, NA)
        thisrecCount <- thisrecCount +1
      }
      temp2 <- as.data.frame(temp2)
      names(temp2) <- names(targetset) 
      targetset <- rbind(targetset, temp2)
      
      
      
      timestamp <- qosdata[rec,]$timestamp
      temp2 <- list()
      temp2<- c(temp2, as.character(timestamp))
      temp2 <- c(temp2, record$in_demand)
      thisrecCount=1
    }
  }
  transformed <- subset(targetset, targetset[2] != 0  ) 
  transformed <- subset(transformed, transformed[3] != 0  ) 
  transformed <- subset(transformed, transformed[4] != 0  ) 
  transformed <- subset(transformed, transformed[5] != 0  ) 
  
  
  ##############
  ###########
  tempcf <- (as.data.frame(qosdata$trafficClass))

  clusterclass <- levels(as.factor(tempcf[[1]]))
  
  
#   print(clusterclass)
#   print(length(clusterclass))
  namescluster <- numeric()
  for(i in 1:length(clusterclass))
  {
   
    namescluster <- c(namescluster, paste("TotalDclass" ,i,sep=""))
    
  }
  
  matrclass <- mat.or.vec(nrow(transformed), length(clusterclass))
  tempdf <- as.data.frame(matrclass)
  names(tempdf) <- namescluster
  
  
  for(rec in 1:nrow(transformed))
  {
    for(k in 1:length(sites))
    {
      curclass <- getClass(sites[k])
     
      if(rec != 1)
      {
        tempdfIdx <- paste("TotalDclass",curclass,sep="")
        transfIdx <- paste("site",sites[k],sep="")
        tempdf[rec, tempdfIdx] <- (tempdf[rec-1, tempdfIdx] + transformed[rec-1,transfIdx])
        # print(tempdf[rec, tempdfIdx])
      }
      
    }
  
  }
  
 
  transformed <- cbind(transformed,tempdf)
 
  for(k in 1:length(sites))
  {
    thisIdx <- paste("site",sites[k],sep="")
    randCol <- transformed[ thisIdx]
    for(recNum in 1:nrow(transformed))
    {
      rand <- runif(1, 1, recNum)
      randCol[recNum, 1] <- transformed[floor(rand), thisIdx]
    }
    
    randTempdf <- as.data.frame(randCol)
    colname <- paste(thisIdx, "rand", sep="")
    names(randTempdf) <- c(colname)
    transformed <- cbind(transformed,randTempdf)
  }

  
  

  
  ##############
  ###########
  
  
  #write.csv(transformed, file = "transformed.csv")
  traindata <- transformed[1:100,]
  testdata <- transformed[101:nrow(transformed),]
  ret.list <- list(traindata, testdata)
  return (ret.list)
  
}




