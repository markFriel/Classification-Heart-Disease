####A number of model that will perfrom classififcation on the stated problem####
library(MASS)
library(nnet)
library(caret)
library(ranger)

ordinalRegression <-function(df,formula,dependantVariable){
  dfSplit <-dataPartition(df, dependantVariable)
  train <- dfSplit[['Training']]
  test <- dfSplit[['Testing']]

  df[,dependantVariable]<-as.factor(df[,dependantVariable])
  model <-polr(formula, data =train, Hess=TRUE)
  ctable <-coef(summary(model))
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  ctable <- cbind(ctable, "p value" = p)
  pred <- predict(model, newdata = test,  type = 'class')

  CI<-confusionMatrix(data=pred, test[,dependantVariable])
  print(CI)
  Output <- list("Ctable" = ctable, "ConfMat"=CI )
  return(Output)
}


neuralNetwork<-function(df,dependantVariable){
  inds<-createDataPartition(df[,dependantVariable], p =0.75, list = FALSE)
  xtrain <- scale(df[inds,1:13])
  xtest <- scale(df[-inds,1:13])
  
  yvariable <- as.data.frame(class.ind(df[,dependantVariable]))
  ytrain<-yvariable[inds,]
  ytest<-yvariable[-inds,]
  
  nnfit<-nnet(x=xtrain,y=ytrain, size=10,MaxNWts = 26000)
 
  
  pred <- predict(nnfit, newdata = xtest)
  m<-apply(pred,1,which.max)
  for(i in 1:nrow(pred)){
    ref<-m[i]
    pred[i,ref]<-ceiling(pred[i,ref])
  }
  return(pred)
}

randomforest<-function(df,formula,dependantVariable){
  dfSplit <-dataPartition(df, dependantVariable)
  train <- dfSplit[['Training']]
  test <- dfSplit[['Testing']]

  fit<-ranger(formula, data=train,num.tree=5)
  pred <- predict(fit, data = test)
  Predictions<-pred[["predictions"]]
  table<-cbind(test,Predictions )
  CI<-confusionMatrix(test[,dependantVariable],pred[["predictions"]])
  print(CI)
  return(table)
}

logReg<-function(df,formula,dependantVariable){
  inds <- createDataPartition(df[,dependantVariable], p =0.75, list = FALSE)
  
  fit<-glm(formula,family="binomial",data=df[inds,])
  pred<-predict(fit, newdata= df[-inds,],type ='response')
  pred<-round(pred)
  table<-cbind(df[-inds,],pred)
  CI<-confusionMatrix(table[,dependantVariable],pred)
  print(CI)
  return(CI)
}

