library(cluster)
library(fpc)
library(mclust)
library("dyn")



# pass in training data, test data,
# it will step through one by one
# need to give dependent var name, so that it can make this into a timeseries
predictDyn <- function( model, train, test, dependentvarname ) {
  Ntrain <- nrow(train)
  Ntest <- nrow(test)
  # can't rbind ts's apparently, so convert to numeric first
  train[,dependentvarname] <- as.numeric(train[,dependentvarname])
  test[,dependentvarname] <- as.numeric(test[,dependentvarname])
  testtraindata <- rbind( train, test )
  testtraindata[,dependentvarname] <- ts( as.numeric( testtraindata[,dependentvarname] ) )
  for( i in 1:Ntest ) {
    result <- predict(model,newdata=testtraindata,subset=1:(Ntrain+i-1))
    testtraindata[Ntrain+i,dependentvarname] <- result[Ntrain + i + 1 - start(result)][1]
  }
  return( testtraindata[(Ntrain+1):(Ntrain + Ntest),] )
}


predictValues <- function( model, trainData, testData, dependentvarname ) {
  
  m <-3
  
  traintestdata <- rbind(trainData,testData)

  targetVarCol <- as.data.frame(traintestdata[[dependentvarname]])
  names(targetVarCol) <- c(dependentvarname)
  
  
  vect <- as.vector(targetVarCol[[dependentvarname]])
  vectlags <- embed(vect, m)
  dflags <- as.data.frame(vectlags)
  
  names(dflags) <- c(dependentvarname, paste(dependentvarname, "l1", sep="") ,
                     paste(dependentvarname, "l2", sep=""))
  
  singlNArow <- (as.data.frame(as.list(rep(NA, m))))
  singlNArow <- rbind(singlNArow,singlNArow)
  names(singlNArow)  <- names(dflags) 
  
  dflags <- rbind(singlNArow, dflags)
  
  modeldataP<- cbind(traintestdata, dflags)
  
  
  # skip the NA entries
  modeldataP <- modeldataP[m:nrow(modeldataP),]
  
  dupvarname <- paste(dependentvarname)
  dupnames <- which(names(modeldataP) == dupvarname)
  
  modeldataP <- modeldataP[,-dupnames[2]]
  

  

  
  orgdata <- traintestdata
  
  Ntest <- nrow(testData)

  Ntrain <- (nrow(modeldataP) - Ntest)
#   print(Ntrain)
#   print(Ntest)
#   print(modeldataP[Ntrain,])
#   print(modeldataP[Ntrain+1,])
#   


#   beforevalues <- modeldataP[(Ntrain+1):(Ntrain + Ntest),]
#   print(beforevalues)
  
 
 
  classnum <- getClass("906") 
  TotDVar <- paste("TotalDclass", classnum, sep="")
  print(TotDVar)
   for( i in 1:(Ntest )) {
    modeldataP[Ntrain+i,TotDVar] <-( modeldataP[Ntrain+i-1,TotDVar]  +  modeldataP[Ntrain+i-1,"site906"])
    singleRec <- as.data.frame(modeldataP[Ntrain+i,])
    result <- predict(model,newdata=singleRec,subset=1:(1))
    modeldataP[Ntrain+i,"site906"] <- result[1][1]
    modeldataP[Ntrain+i+1,"site906l1"] <- modeldataP[Ntrain+i,"site906"]
    modeldataP[Ntrain+i+2,"site906l2"] <- modeldataP[Ntrain+i,"site906"]
  

   }
  View(modeldataP)
  predictedvalues <- modeldataP[(Ntrain+1):(Ntrain + Ntest),]
  #print(predictedvalues)
  return (predictedvalues$site906)
  
}

modelFit <- function( trainData, dependentvarname ) {

  
  m <-3
  targetVarCol <- as.data.frame(trainData[[dependentvarname]])
  names(targetVarCol) <- c(dependentvarname)
  
 
  vect <- as.vector(targetVarCol[[dependentvarname]])
  vectlags <- embed(vect, m)
  dflags <- as.data.frame(vectlags)
  
  names(dflags) <- c(dependentvarname, paste(dependentvarname, "l1", sep="") ,
                     paste(dependentvarname, "l2", sep=""))
  
  singlNArow <- (as.data.frame(as.list(rep(NA, m))))
  singlNArow <- rbind(singlNArow,singlNArow)
  names(singlNArow)  <- names(dflags) 
  
  dflags <- rbind(singlNArow, dflags)

  modeldata<- cbind(trainData, dflags)
 
  
  # skip the NA entries
  modeldata <- modeldata[m:nrow(modeldata),]
  dupvarname <- paste(dependentvarname)
  dupnames <- which(names(modeldata) == dupvarname)
#   print("dupindexes:")
#   print(dupnames)
  modeldata <- modeldata[,-dupnames[2]]
  #modeldata <- subset(modeldata, modeldata[2] != NA ) 
  

  PredictorVariables1 <- paste("TotalDclass", 1:3, sep="")
  lagvarname <- paste(dependentvarname, "l", sep="")
  PredictorVariables2 <- paste(lagvarname, 1:2, sep="")
  randvarname <- paste(dependentvarname, "rand", sep="")
  PredictorVariables3 <- randvarname
  PredictorVariables <- c(PredictorVariables1,PredictorVariables2)
  PredictorVariables <- c(PredictorVariables,PredictorVariables3)
  Formula <- formula(paste( paste(dependentvarname, " ~ ", sep =""), 
                           paste(PredictorVariables, collapse=" + ")))
  
  

    
    print(Formula)
    model <- lm( Formula, modeldata) 
  print(model)
  return(model)
  

  
 
}


