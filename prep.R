#remove 7th and 9th cols

View(test_fffffff)

test_final_import127 = test_final_import127[,-1]

test_final_import127$Anon_Student_id <- factor(test_final_import127$Anon_Student_id)
test_final_import127$Problem_Hierarchy<- factor(test_final_import127$Problem_Hierarchy)
test_final_import127$Problem_Name<- factor(test_final_import127$Problem_Name)
test_final_import127$Step_Name <- factor(test_final_import127$Step_Name)
test_final_import127$Correct_Transaction_Time<-factor(test_final_import127$Correct_Transaction_Time)
test_final_import127$KC<- factor(test_final_import127$KC)
test_final_import127$Opportunity <- factor(test_final_import127$Opportunity)
test_final_import127$Correct_First_Attempt <- as.factor(test_final_import127$Correct_First_Attempt)

#test_final_import127$Step_Duration<-as.numeric(test_final_import127$Step_Duration


test_final_import127$Anon_Student_id<- as.numeric(test_final_import127$Anon_Student_id)
test_final_import127$Problem_Hierarchy<- as.numeric(test_final_import127$Problem_Hierarchy)
test_final_import127$Problem_Name<- as.numeric(test_final_import127$Problem_Name)
test_final_import127$Step_Name <- as.numeric(test_final_import127$Step_Name)
test_final_import127$Correct_Transaction_Time <- as.numeric(test_final_import127$Correct_Transaction_Time)
test_final_import127$KC<- as.numeric(test_final_import127$KC)
test_final_import127$Opportunity <- as.numeric(test_final_import127$Opportunity)
test_final_import127$Correct_First_Attempt <- as.numeric(test_final_import127$Correct_First_Attempt)

View(test_final_import127)
test_final_import127 <- na.omit(test_final_import127)
apply(test_final_import127,MARGIN=2,FUN=function(x)sum(is.na(x)))
maxs = apply(test_final_import127, MARGIN = 2, max)
mins = apply(test_final_import127, MARGIN = 2, min)
scaled = as.data.frame(scale(test_final_import127, center = mins, scale = maxs - mins))
View(scaled)

final_data127 <- scaled
View(final_data127)
trainIndex <- sample(1:nrow(final_data127), 0.8 * nrow(final_data127)) 
train127 <- final_data127[trainIndex, ]
train <- final_data127[trainIndex, ]
test <- final_data127[-trainIndex, ]
n <- names(train)
f <- as.formula(paste("Correct_First_Attempt~", paste(n[!n %in% "Correct_First_Attempt"], collapse = " + ")))
f127 <- as.formula(paste("Correct_First_Attempt~", paste(n[!n %in% "Correct_First_Attempt"], collapse = " + ")))
#neural network:
library(neuralnet)
nn <- neuralnet(f,data=train)
nn_hidden <- neuralnet(f,data=train,hidden=c(5,3))
plot(nn)
plot(nn_hidden)
#c(f)
nn$result.matrix
pred <- compute(nn,test[,1:14])

pred.scaled <- pred$net.result *(max(test_final_import127$Correct_First_Attempt)-min(test_final_import127$Correct_First_Attempt))+min(test_final_import127$Correct_First_Attempt)
real.values <- (test$Correct_First_Attempt)*(max(test_final_import127$Correct_First_Attempt)-min(test_final_import127$Correct_First_Attempt))+min(test_final_import127$Correct_First_Attempt)
MSE.nn <- sum((real.values - pred.scaled)^2)/nrow(test)

c(MSE.nn)
pred_hidden <- compute(nn_hidden,test[,1:14])
pred.scaled_hidden <- pred_hidden$net.result *(max(test_final_import127$Correct_First_Attempt)-min(test_final_import127$Correct_First_Attempt))+min(test_final_import127$Correct_First_Attempt)
real.values_hidden <- (test$Correct_First_Attempt)*(max(test_final_import127$Correct_First_Attempt)-min(test_final_import127$Correct_First_Attempt))+min(test_final_import127$Correct_First_Attempt)
MSE.nn_hidden <- sum((real.values_hidden - pred.scaled_hidden)^2)/nrow(test)
c(MSE.nn_hidden)
nn_hidden$result.matrix
nn_hidden_testing <- neuralnet(f,data=train,hidden=c(5,4,3))
plot(nn_hidden_testing)

library(ROCR)
nn.pred = prediction(pred$net.result, test$Correct_First_Attempt)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)


#Adaboost
ec = 0
pre = 0
#boosting
library(pROC)
library(rpart)
library(caret)
library(ggplot2)
library(adabag)

  p=0
  acc=0
    bt1 <- boosting(f, data=train, boos=TRUE, mfinal=10)
  #bt1 <- boosting(f, data=train, mfinal=100)
    pred_boosting <- predict.boosting(bt ,newdata = test)
 
    tpr <- 0.446
    fpr <- 0.554
    perf_boosting127= performance(pred_boosting,"tpr","fpr")
    plot(perf_boosting127)
    
    #tab <- table(pred127=pred_boosting, true=test$Correct_First_Attempt)

    summary(pred_boosting$error)
    p <- factor(pred_boosting$class)
    acc =1 - pred_boosting$error
  print(paste('acurracy = ',acc))


print(paste('recal = ',rec))
print(paste('precision = ',pre))





nn_hidden_testing$result.matrix
pred_hidden_testing <- compute(nn_hidden_testing,test[,1:14])
pred_hidden_testing$net.result <- round(pred_hidden_testing$net.result)
pred_hidden_testing$net.result
library(ROCR)
nn.pred_hidden_test = prediction(pred_hidden_testing$net.result, test$Correct_First_Attempt)
pref_hidden <- performance(nn.pred_hidden_test, "tpr", "fpr")
plot(pref_hidden)#}
pred$net.result
nn$result.matrix
A <- test$Correct_First_Attempt
c(A)
predict_hidden_test <- prediction(predictions= pred_hidden_testing, labels= A)
pred.scaled_hidden_testing <- pred_hidden_testing$net.result *(max(test_final_import127$Correct_First_Attempt)-min(test_final_import127$Correct_First_Attempt))+min(test_final_import127$Correct_First_Attempt)
real.values_testing <- (test$Correct_First_Attempt)*(max(test_final_import127$Correct_First_Attempt)-min(test_final_import127$Correct_First_Attempt))+min(test_final_import127$Correct_First_Attempt)
MSE.nn_hidden_testing <- sum((real.values_testing - pred.scaled_hidden_testing)^2)/nrow(test)
c(MSE.nn_hidden_testing)

test$Correct_First_Attempt <- as.factor(test$Correct_First_Attempt)
train$Correct_First_Attempt <- as.factor(train$Correct_First_Attempt)
pred$net.result <- as.factor(pred$net.result)

pred$net.result <- as.integer(pred$net.result)
c(pred$net.result)
pred$net.result <- round(pred$net.result, digits=0)

tab <- confusionMatrix(pred$net.result, test$Correct_First_Attempt)

library(caret)
table(pred_hidden_testing$net.result, test[15])
print(pred_hidden_testing) 
c(pred_hidden_testing)
dim(pred_hidden_testing)
#values are taken from confusion matrix
#tpr <- 200/448
#fpr <- 48/148

library(ROCR)

predict_pred = prediction(pred$net.result, test$Correct_First_Attempt)

perf = performance(predict_pred,"tpr","fpr")
plot(perf)


predict_pred127 = prediction(pred127, test$Correct_First_Attempt)
tpr <- 0.446
fpr <- 0.554
perf127 =performance(predict_pred127, "tpr", "fpr")
plot(perf127)

tab127 <- confusionMatrix(pred127, test$Correct_First_Attempt)

tab127
pred$net.result #has the actual prediction
pred$net.result <- round(pred$net.result)
pred$net.result[0]

pred127 <-pred$net.result # for manipulation
for(i in 1:500){
  if(pred127[i]==1)
    pred127[i] <- 0
 if(pred127[i]==0)
  pred127[i] <-1
}

auc(predictions$survived, predictions$pred)
library(pROC)
roc <- calculate_roc(predict_pred, 1, 2, n = 100)
plot_roc(roc, 0.7, 1, 2)
library(ggplot2)
plot(perf,lwd=2,col="blue",main="ROC - Neural Network")
ggplot(x= 0.554, y= 0.446)
abline(a=0,b=1)

plot(real.values_testing, pred.scaled_hidden_testing, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

View(final_data)
test_fffffff$`Correct_Step_Duration(sec)`<-factor(test_fffffff$`Correct_Step_Duration(sec)`)
test_fffffff$`Correct_Step_Duration(sec)`<-as.numeric(as.character(test_fffffff$`Correct_Step_Duration_(sec)`))
