data_delay<-read.csv(".\\751378209_T_ONTIME.csv", header=TRUE, sep="," ,stringsAsFactors=FALSE)
nrow(data_delay)
ncol(data_delay)
airport<- c('ALT','LAX','ORD','DFW','JFK','SFO','CLT','LAS','PHX')
data_delay<-subset(data_delay, DEST %in% airport & ORIGIN %in% airport)
head(data_delay)
data_delay$X<-NULL
cor(data_delay[c("ORIGIN_AIRPORT_SEQ_ID", "DEST_AIRPORT_SEQ_ID")])
data_delay$ORIGIN_AIRPORT_SEQ_ID<-NULL
cor(data_delay[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")])
data_delay$DEST_AIRPORT_SEQ_ID<-NULL
mistmatch<-data_delay[data_delay$UNIQUE_CARRIER != data_delay$CARRIER,]
nrow(mistmatch)
head(data_delay)
data_delay_ontime<-data_delay[!is.na(data_delay$ARR_DEL15) & data_delay$ARR_DEL15 !="" & !is.na(data_delay$DEP_DEL15)  & data_delay$DEP_DEL15 !="",]
data_delay_ontime$DISTANCE<-as.integer (data_delay_ontime$DISTANCE)
data_delay_ontime$CANCELLED<-as.integer (data_delay_ontime$CANCELLED)
data_delay_ontime$DIVERTED<-as.integer (data_delay_ontime$DIVERTED)
data_delay_ontime$ARR_DEL15<-as.factor(data_delay_ontime$ARR_DEL15)

data_delay_ontime$DEP_DEL15<-as.factor(data_delay_ontime$DEP_DEL15)
data_delay_ontime$DEST_AIRPORT_ID<-as.factor(data_delay_ontime$DEST_AIRPORT_ID)
data_delay_ontime$ORIGIN_AIRPORT_ID<-as.factor(data_delay_ontime$ORIGIN_AIRPORT_ID)
data_delay_ontime$DAY_OF_WEEK<-as.factor(data_delay_ontime$DAY_OF_WEEK)
data_delay_ontime$DEST<-as.factor(data_delay_ontime$DEST)
data_delay_ontime$ORIGIN<-as.factor(data_delay_ontime$ORIGIN)
data_delay_ontime$DEP_TIME_BLK<-as.factor(data_delay_ontime$DEP_TIME_BLK)
data_delay_ontime$CARRIER<-as.factor(data_delay_ontime$CARRIER)
tapply(data_delay_ontime$ARR_DEL15 , data_delay_ontime$ARR_DEL15, length)
5841/(20386+5481) # 22 percent of flight s are delayed s our training sets are ready
#####  training the data and splitting 
install.packages('caret')
library(caret)
set.seed(3456544)
predictioncol<-c("ARR_DEL15","DAY_OF_WEEK","CARRIER","DEST","DEP_TIME_BLK")
newontimedata<-data_delay_ontime[,predictioncol]
intrainrows<-createDataPartition(newontimedata$ARR_DEL15, p=0.70, list=FALSE)
trandatafiltered<-newontimedata[intrainrows,]
testdatafiltered<-newontimedata[-intrainrows,]
nrow(trandatafiltered)/(nrow(trandatafiltered)+nrow(testdatafiltered))
nrow(testdatafiltered)/(nrow(trandatafiltered)+nrow(testdatafiltered))
logisticregressionmodel<-train(ARR_DEL15~., data=trandatafiltered, method="glm", family="binomial")

##evaluating the result by using the test data set
logisticmodeltest<-predict(logisticregressionmodel,testdatafiltered)
logidticconfusionmatric<-confusionMatrix(logisticmodeltest, testdatafiltered[,"ARR_DEL15"])
# the  Specificity : 0.007299 
##to improve the model, use randomforest algorithm 
install.packages("randomForest")
library(randomForest)
randomforestmodel<-randomForest(trandatafiltered[-1],trandatafiltered$ARR_DEL15, proximity = TRUE, importance = TRUE)
rfvalidation<-predict(randomforestmodel, testdatafiltered)
rfconfusionmtrix<-confusionMatrix(rfvalidation, testdatafiltered[, "ARR_DEL15"])
