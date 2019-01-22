

grpcluster <- function( qosdata) {
  
  
  result <- aggregate(qosdata$in_demand, list(endpoint_name = qosdata$endpoint_name), mean)
  
  names(result) <- c("endpoint_name", "meanval")
  #ncluster <- pamk(qosdata)
  
  newdf <- (qosdata$in_demand)
  
  for(rec in 1:nrow(qosdata))
  {
    for(i in 1:nrow(result))
    {
      if(qosdata[rec,]$endpoint_name == result[i,]$endpoint_name)
      {
        newdf[rec] <- result[i,]$meanval
      }
    }
    
  }
  
  newdf <- as.data.frame(newdf)
  
  kc <- kmeans(newdf, 3)
  trafficClass <- kc$cluster
  trafficClass <- as.data.frame(trafficClass)
  return (trafficClass)
  #qosdata <- cbind (qosdata, trafficClass)
}

getClass <- function( endpoint_name) {
  
  
  
  for(rec in 1:nrow(qosdata))
  {
    if(qosdata[rec,]$endpoint_name == endpoint_name)
    {
      return (qosdata[rec,]$trafficClass)
    }
  }
  return (NA)
  
  
}

