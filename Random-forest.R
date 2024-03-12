row.names(dataset) <- dataset$X
dataset <- dataset[,2:10]
dataset$Class_variable <- factor(dataset$Class_variable, labels=c('No', 'Yes'))
attach(dataset)

library(randomForest)
set.seed(2612)
(rf <- tuneRF(x = subset(dataset, select = -Class_variable), y = dataset$Class_variable, ntreeTry =500, plot = TRUE, mtryStart=8, doBest=T))
varImpPlot(rf, col="blue", pch=20, main="Train set")


#confusion matrix
N <- nrow(dataset)
random.probs <- predict(rf, data=dataset, type='prob') 
Pred.Cl.tree <- rep("No", N)
Pred.Cl.tree[random.probs[,2] > threshold(random.probs[,2], dataset$Class_variable)]="Yes"
(matrix <- addmargins(table(Pred.Cl.tree, dataset$Class_variable)))
(accuracy <-(matrix[1,1]+matrix[2,2])/N*100)
(error <- 100-accuracy)


#validation
nV <- nrow(datasetV)
row.names(datasetV) <- datasetV$X
datasetV <- datasetV[,2:10]
datasetV$Class_variable <- factor(datasetV$Class_variable, labels=c('No', 'Yes'))
attach(datasetV)

randomforestV <- predict(rf, newdata = datasetV, type='prob')
Pred.Cl.treeV <- rep("No", nV)
Pred.Cl.treeV[randomforestV[,2] > threshold(randomforestV[,2], datasetV$Class_variable)]="Yes"
(matrix <- addmargins(table(Pred.Cl.treeV, datasetV$Class_variable)))
(accuracy <-(matrix[1,1]+matrix[2,2])/nV*100)
(error <- 100-accuracy)

library(verification)
random.probs <- predict(rf, data=dataset, type='prob') 
roc.plot(dataset$Class_variable== "Yes", random.probs[,2], ylab = "True Positive Rate", xlab = "False Positive Rate", main='ROC Curve train set')$roc.vol
random.probsV <- predict(rf, newdata=datasetV, type='prob')
roc.plot(datasetV$Class_variable== "Yes", random.probsV[,2], ylab = "True Positive Rate", xlab = "False Positive Rate", main='ROC Curve validation set')$roc.vol
