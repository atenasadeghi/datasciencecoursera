install.packages("corrplot")
library(corrplot)
correlation <- cor(mydata[,c('satisfaction_level','last_evaluation','number_project',
                             'average_montly_hours','time_spend_company','Work_accident',
                             'left','promotion_last_5years')])
corrplot(correlation)
par(mfrow=c(2,2))
hist(mydata&satisfaction_level, break=5, col="lightblue", border="black", main="satisfaction level")
hist(mydata$satisfaction_level, breaks =5, col="lightblue", border="black", main="satisfaction level")
hist(mydata$promotion_last_5years, breaks =5, col="lightblue", border="black", main="last_promotion in 5 year")
hist(mydata$average_montly_hours, breaks =5, col="lightblue", border="black", main="average working hours")
hist(mydata$number_project, breaks =5, col="lightblue", border="black", main="number of project")
goog_people_leave<-subset(mydata, last_evaluation>=0.70 | number_project>5 | time_spend_company >4)

## good_people_leave2<- goog_people_leave[,c('satisfaction_level','number_project','average_montly_hours','time_spend_company','Work_accident','left','promotion_last_5years')]
good_people_correlation <- cor(goog_people_leave[,c('satisfaction_level','last_evaluation','number_project',
  'average_montly_hours','time_spend_company','Work_accident',  'left','promotion_last_5years')]) 

corrplot(good_people_correlation, method = "number")
corrplot(good_people_correlation, method = "square")
##pick the variable left
goog_people_leave$left <- as.factor(goog_people_leave$left)
###predict model
library(caret)
train_control<- trainControl(method="cv", number=5, repeats=3)
head(train_control)
library("rpart")
library("rpart.plot")
# train the model 
rpartmodel<- train(left~., data=goog_people_leave, trControl=train_control, method="rpart")
# make predictions
predictions<- predict(rpartmodel,goog_people_leave)
hr_model_tree<- cbind(goog_people_leave,predictions)
# summarize results
confusionMatrix<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$left)
confusionMatrix
# logistic regression
gmlmodel <- train(left~., data=goog_people_leave, trControl=train_control, method="LogitBoost")
predictions<- predict(gmlmodel,goog_people_leave)
gmlmodelbinded <- cbind(goog_people_leave,predictions)
# summarize results
confusionMatrix<- confusionMatrix(gmlmodelbinded$predictions,gmlmodelbinded$left)
confusionMatrix
###
set.seed(100)
# Keep some data to test again the final model
inTraining <- createDataPartition(goog_people_leave$left, p = .75, list = FALSE)
training <- goog_people_leave[ inTraining,]
testing  <- goog_people_leave[-inTraining,]
# Estimate the drivers of attrition
logreg = glm(left ~ ., family=binomial(logit), data=training)
# Make predictions on the out-of-sample data
probaToLeave=predict(logreg,newdata=testing,type="response")
# Structure the prediction output in a table
predattrition = data.frame(probaToLeave)
# Add a column to the predattrition dataframe containing the performance
predattrition$performance=testing$last_evaluation
plot(predattrition$probaToLeave,predattrition$performance)
