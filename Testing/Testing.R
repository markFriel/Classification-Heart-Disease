source("C:/Projects/Machine Learning Projects/Classification Heart Disease/Functions/ClassificationModels.R")
source("C:/Projects/Machine Learning Projects/Classification Heart Disease/Functions/DataSplit.R")
source("C:/Projects/Machine Learning Projects/Classification Heart Disease/Functions/DataSummarization.R")

#Read the data set into a dataframe from a csv

df <- read.csv("C:/Projects/Machine Learning Projects/Classification Heart Disease/Data/HeartDisease.csv")
df$thal <- as.numeric(as.character(df$thal))
df$ca <- as.numeric(as.character(df$ca))
df$HDP<-as.factor(df$HDP)
df <- na.omit(df)


formula<-formulaSU(df,"HDP")

ordinalReg<-ordinalRegression(df,formula,"HDP")

#neural network is not giving acceptable metrics so have not formatted for presentation
neuralnet <-neuralNetwork(df,"HDP")
rf<-randomforest(df,formula,"HDP")

####Analysis after binning has taken place####

negative<-subset(df,HDP==0)
positive<-subset(df,HDP !=0)
positive[,"HDP"]<-1
class<-rbind(positive,negative)
class[,"HDP"]<-as.factor(class[,"HDP"])



binnedOrdinalReg<-randomforest(BinnedDF,formula,"HDP")
