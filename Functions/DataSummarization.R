####Functions to read in and provide data summarization in the forms of plots####

library(purrr)
library(tidyr)
library(ggplot2)
library(corrplot)

####Univarte plots####
#PLots a histogram for each if the numeric variables in the dataframe

UnivariatePlot <- function(df){
  df%>%
    keep(is.numeric)%>%
    gather()%>%
    ggplot(aes(value))+
      facet_wrap(~ key,scales = "free")+
    geom_histogram()
}


####Multivariate Plots####
#Calculate the correlation between each numeric pair

correlationPlot<-function(df){
  correlations<- cor(df[,7:14])
  corrplot(correlations,method = "number")
}

#Produces scatterplots with the class variable coloured

scatterplot<-function(df){
  data(df)
  pairs(HDP~.,data=df[7:14], col=df$HDP)
}
