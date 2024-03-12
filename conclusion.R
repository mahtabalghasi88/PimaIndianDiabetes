
rf <- randomForest(dataset$Class_variable~., mtry = 4, data=dataset, ntree=500) 
datasetT$Outcome <- predict(rf, newdata = datasetT, type='response')
head(datasetT)
tail(datasetT)

library(ggplot2)
ggplot(datasetT,aes(x=Outcome))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=scales::percent)+
  ylab('Percentage')+
  xlab('Diabetes')+
  ggtitle('Prediction on test set')

library(GGally)
ggpairs(datasetT, aes(colour=Outcome), size=0.6)
