rm(list = ls())
#packages#
library(mice)
library(ggplot2)
library(caret)
library(ROCR)
library(class)
library(C50)
library(rpart)
library(randomForest)
library(shiny)
library(tree)


#Import 4 datasets#
processed.cleveland <- read.csv("path", header=FALSE)
processed.hungarian <- read.csv("path", header=FALSE)
processed.switzerland <- read.csv("path", header=FALSE)
processed.va <- read.csv("path", header=FALSE)
#combine 4 datasets into one data pool#
Total_data<-rbind(processed.cleveland,processed.hungarian,processed.switzerland,processed.va)

#replace ? to NA#
idx <- Total_data == "?"
is.na(Total_data) <- idx

#add attribute name#
colnames(Total_data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
#change attribute to numerial"
Total_data$sex<-as.factor(Total_data$sex)
Total_data$cp<-as.factor(Total_data$cp)
Total_data$trestbps<-as.numeric(Total_data$trestbps)
Total_data$chol<-as.numeric(Total_data$chol)
Total_data$fbs<-as.factor(Total_data$fbs)
Total_data$restecg<-as.factor(Total_data$restecg)
Total_data$thalach<-as.numeric(Total_data$thalach)
Total_data$exang<-as.factor(Total_data$exang)
Total_data$oldpeak<-as.numeric(Total_data$oldpeak)
Total_data$slope<-as.factor(Total_data$slope)
Total_data$num<-as.factor(Total_data$num)

#Turn the num into binary level0=N(egative),others=P(ositive)#
ds <- as.data.frame(apply(Total_data, 2, as.numeric))
ds$num[ds$num > 0] <- 1
ds$num <- factor(ds$num, levels = c(0,1), labels = c("negative", "positive"))



#Deal with missing value#
#Delete NA>50%
#Replace NA<50% by using mice
Total_data_clean<-ds[,-c(12,13)]
percentmiss=function(x){
  sum(is.na(x))/length(x)*100
}
replace=mice(Total_data_clean)
Final_data=complete(replace)
#Visualize data


#View the data
featurePlot(x=training[,c("age","cp","trestbps","chol","thalach","oldpeak")],
            y=training$num,plot = "pairs")


#Split the dataset
set.seed(323)
inTrain<-createDataPartition(y=Final_data2$num,p=0.7,list=F)
training<-Final_data2[inTrain,]
testing<-Final_data2[-inTrain,]
dim(training);dim(testing)

#Deal with outliers#
training[,-c(12)]
mahal=mahalanobis(training[,-c(12)],colMeans(training[,-c(12)]),cov(training[,-c(12)],use = "pairwise.complete.obs"))
cutoff=qchisq(.999,ncol(training[,-c(12)]))
training=training[mahal<cutoff,]


#Cross validation set(do it in the model)     (just a try)   
#set.seed(323)
#folds<-createFolds(y=Final_data2$num,k=10,list=T,returnTrain = T)
#sapply(folds, length)

#classifier#
model_rf<-train(num~.,data=training,method="rf",trControl=trainControl(method="cv"),number=10)
model_glm<-train(num~.,data=training,method="glm")
model_c50<-C5.0(num~.,data=training,rules=T,trControl=trainControl(method="cv"),number=10)
model_tree<-train(num~.,data=training,method="rpart",trControl=trainControl(method="cv"),number=10)
model_tree1<-tree(num~.,data=training)
model_knn <- train(num ~., method = "knn", data = training,tuneLength = 10,  tuneGrid=data.frame(k=1:5),
                           trControl = trainControl(
                             method = "cv"))

#testing data set prediction

pred_rf_p<-predict(model_rf,newdata=testing,method="class",type="prob")
pred_rf_r<-predict(model_rf,newdata = testing,method="class",type="raw")
pred_glm_p<-predict(model_glm,newdata=testing,type="prob")
pred_glm_r<-predict(model_glm,newdata=testing,type="raw")
pred_c50_p<-predict(model_c50,newdata=testing,type="prob")
pred_c50_c<-predict(model_c50,newdata = testing,type="class")
pred_tree_p<-predict(model_tree,newdata=testing,type="prob",metric="ROC")
pred_tree_r<-predict(model_tree,newdata=testing,type="raw")
pred_knn_p<-predict(model_knn,newdata=testing,type="prob")
pred_knn_r<-predict(model_knn,newdata=testing,type="raw")

#plot(ROC)
#rf AUC=0.900

pred_roc_rf<-prediction(pred_rf_p$positive,testing$num)
perf_roc_rf<-performance(pred_roc_rf,"tpr", "fpr")
plot(perf_roc_rf,col=2,main="ROC curves comparing classification performance of five machine learning models")
auc_rf<- performance(pred_roc_rf,"auc")
auc_rf

#glm AUC=0.89
pred_roc_glm<-prediction(pred_glm_p$positive,testing$num)
perf_roc_glm<-performance(pred_roc_glm,"tpr", "fpr")
plot(perf_roc_glm,add=T,col=3)
auc_glm<- performance(pred_roc_glm,"auc")
auc_glm


#c50 AUC=0.86807554
pred_roc_c50<-prediction(as.data.frame(pred_c50_p)$positive,testing$num)
perf_roc_c50<-performance(pred_roc_c50,"tpr", "fpr")
plot(perf_roc_c50,add=T,col=3)
auc_c50<- performance(pred_roc_c50,"auc")
auc_c50

#tree AUC= 0.771
pred_roc_tree<-prediction(pred_tree_p$positive,testing$num)
perf_roc_tree<-performance(pred_roc_tree,"tpr", "fpr")
plot(perf_roc_tree,add=T,col=4)
auc_tree<- performance(pred_roc_tree,"auc")
auc_tree

#knn AUC= 0.7489905

pred_roc_knn<-prediction(pred_knn_p$positive,testing$num)
perf_roc_knn<-performance(pred_roc_knn,"tpr", "fpr")
plot(perf_roc_knn,col=5,add=T)
auc_knn<- performance(pred_roc_knn,"auc")
auc_knn

#legend of the ROC table
legend(0.1, 0.3, c('randomForest', 'c50','tree','knn'), 2:5,seg.len = 0.01,text.width=0.05,horiz = T)

#Evaluation metrix(Recall...)
Evaluation_outuput<- function(x,y){
  TP  <- sum(x == "positive" & y== "positive")
  FP   <- sum(x == "negative" & y== "positive")
  TN  <- sum(x == "negative" & y == "negative")
  FN   <- sum(x == "positive" & y == "negative")
  Accuracy<-(TP+TN)/(TP+FP+TN+FN)
  Precision<-TP/(TP+FP)
  Recall_Sensitivity<-TP/(TP+FN)
  F1<-2*Precision*Recall_Sensitivity/(Precision+Recall_Sensitivity)
  row.names <- c("Prediction_T", "Prediction_F" )
  col.names <- c("Test_T", "Test_F")
  Outcome_Matrix<-cbind(Outcome = row.names, as.data.frame(matrix( 
    c(TP, FN, FP, TP) ,
    nrow = 2, ncol = 2,dimnames = list(row.names, col.names))))
  cat("Accuracy:",Accuracy)
  cat("Precision:",Precision)
  cat("Recall:",Recall_Sensitivity)
  cat("F:",F1)
  print(Outcome_Matrix)
}
#Evaluation result(knn):Accuracy: 0.7043796 Precision: 0.7417219 Recall: 0.7272727 F: 0.7344262 
#Evaluation result(tree):Accuracy: 0.7810219 Precision: 0.8278146 Recall: 0.7861635 F: 0.8064516 
#Evaluation result(c50):Accuracy: 0.8175182 Precision: 0.8211921 Recall: 0.8435374 F: 0.8322148
#Evaluation result(rf):Accuracy: 0.8248175 Precision: 0.8476821 Recall: 0.8366013 F: 0.8421053 
#Evaluation result(glm):Accuracy: 0.810219 Precision: 0.8344371 Recall: 0.8235294 F: 0.8289474 




