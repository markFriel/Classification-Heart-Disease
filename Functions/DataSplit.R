####Split the data set into traning and testing samples####
library(caret)

dataPartition <- function(df, dependantVariable){
  inds <- createDataPartition(df[,dependantVariable], p =0.75, list = FALSE)
  training <- df[inds,]
  testing <- df[-inds,]
  tt<- list("Training" = training, "Testing"= testing)
  return(tt)
}
 

formulaSU<-function(df,dependantVariable){
  n<-names(df)
  f<- as.formula(paste(paste(dependantVariable,"~"), paste(n[!n%in% dependantVariable], collapse = " + ")))
}


formulaNN<-function(df,responcseCol){
  col<-df[,responcseCol]
  colN<-names(col)
  predictor<-paste(colN,collapse = "+")
  n<-names(df)
  f<- as.formula(paste(paste(predictor,"~"), paste(n[!n%in% colN], collapse = " + ")))
  
}


