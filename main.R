
#mainDir="/Users/sdhandap/Dropbox/dataForExperiment"
mainDir="/home/sdhandap/Rprototype"
setwd(file.path(mainDir))
source("./clusterit.R")
source("./dataTransform.R")
source("./models.R")
setwd(file.path(mainDir))
qosdata <- read.csv("./qos_gqm_stats_aug15.csv", sep=",")
classify <- grpcluster(qosdata)
names(classify) <- c("trafficClass")
qosdata<-cbind(qosdata, classify)
transModelData <- dataTrans(qosdata)
trainData <- transModelData[[1]]
testData <- transModelData[[2]]
# 
siteendpt <- 906
dependentvarname <- paste("site", 906, sep="")

actual <- as.data.frame(testData$site906)
names(actual) <- c(dependentvarname)
testData$site906 <- NA
clss <- getClass(siteendpt)
tempdfIdx <- paste("TotalDclass",clss,sep="")
testData[tempdfIdx] <- NA




#View(trainData)
model <- modelFit(trainData,dependentvarname)
#predictValues(model, trainData, testData, dependentvarname)
predicted <- as.data.frame(predictValues(model, trainData, testData, dependentvarname))
names(predicted) <- c(dependentvarname)
resultdf <- cbind(actual, predicted)
names(resultdf) <- c("actual","predicted")
print(resultdf)