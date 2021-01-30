

#implement 
#版权声明：本文为CSDN博主「Tiaaaaa」的原创文章，遵循 CC 4.0 BY-SA 版权协议，转载请附上原文出处链接及本声明。
#原文链接：https://blog.csdn.net/Tiaaaaa/article/details/58116346

##  Built by Matthew J. Schneider, add profit by Yue 'Alex' Fu

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  c1=1300   # reward me  $1300 for a true positive
  c2=100  #  penalize me $100 for a false positive
  profit=tp*c1-fp*c2
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn,profit)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn","profit")
  
  #print(scores)
  return(scores);
}



setwd("~/Desktop/2020WINTER/STAT630/Group Project/0214")
data_in=read.csv("dataset5815.csv",header = TRUE, row.names = 1) #***modify the dataset***
summary(data_in)
str(data_in)
dim(data_in)
#data_in$Racegrp <- as.factor(data_in$Racegrp)
#data_in$CareSource <- as.factor(data_in$CareSource)
#data_in$Activity <- as.factor(data_in$Activity)
names(data_in)
library(matrixStats)
library(caret)
set.seed(429)
require(caret)
folds <- createFolds(y=data_in$CKD, k=10)

acc_total =  matrix(nrow = 1000, ncol = 13)
acc_mean = matrix(nrow = 100, ncol = 13)
acc_var = matrix(nrow = 100, ncol = 13)
acc_median= matrix(nrow = 100, ncol = 13)
step = 0.01
for (thresh in seq(0.01,0.8,by = step)) {
  for (i in 1:10) { 
    fold_test <-data_in[folds[[i]],]
    fold_train <- data_in[-folds[[i]],]
    
    fold_pre <- glm(CKD~Age + Female + HDL + PVD + Activity + Hypertension + Fam.Hypertension + Diabetes + CVD + Fam.CVD + CHF + Anemia +Racegrphispa,
                      family=binomial(link = 'logit'),data=fold_train)    #***modify the model***
    
    fold_predict <- predict(fold_pre, type ='response', newdata = fold_test)
    fold_predict =ifelse(fold_predict>thresh,1,0) #####                         ***modify the threshold***
    fold_test$predict_ckd =fold_predict
    acc = c_accuracy(fold_test$CKD,fold_test$predict_ckd)
    acc_total [(  ( thresh /step -1)  *10 + i) ,1:11] = acc
    acc_total [(  ( thresh/step -1)  *10 + i),12] = thresh
    acc_total [(  ( thresh /step -1)  *10 + i),13] = i
  }
  acc_mean[thresh /step,] = colMeans(acc_total[(  ( thresh/step -1)  *10 + 1):(  ( thresh /step -1)  *10 + 10),])
  acc_var[thresh /step,] = colVars(acc_total[(  ( thresh/step -1)  *10 + 1):(  ( thresh /step -1)  *10 + 10),])
  acc_median[thresh /step,] = colMedians(acc_total[(  ( thresh/step -1)  *10 + 1):(  ( thresh /step -1)  *10 + 10),])
}

#acc_mean = colMeans(acc_total)
colnames(acc_total)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn","profit","threshold","times")
colnames(acc_mean)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn","profit","threshold","times")
setwd("~/Desktop/2020WINTER/STAT630/Group Project/0222")
write.csv(acc_total, file = "0222-model2_total.csv")
write.csv(acc_mean, file = "0222-model2_mean.csv")





