dataset <- read.csv(file.choose())
row.names(dataset) <- dataset$X
dataset <- dataset[,2:10]
dataset$Class_variable <- factor(dataset$Class_variable, labels=c('No', 'Yes'))
attach(dataset)
#variable selection: mixed approach
library(MASS)
glm <- stepAIC(glm(Class_variable ~.  +Age*pregnant_times, data = dataset, family = binomial(link='logit')), steps=100)
summary(glm)

#Confusion matrix
threshold <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(df$sens + df$spec), "cut"]
}

N <- nrow(dataset)
glm.probs <- predict.glm(glm, dataset, type='response')
glm.pred <- rep('No', N)
glm.pred[glm.probs> threshold(glm.probs, dataset$Class_variable)]='Yes'
(confMat <- addmargins(table(glm.pred, dataset$Class_variable)))
(accuracy <- (confMat[1,1]+confMat[2,2])/N*100)
(Err <- 100-accuracy)

#--------------------------------------
#VALIDATION

#validation set
datasetV <- read.csv(file.choose())
row.names(datasetV) <- datasetV$X
datasetV <- datasetV[,2:10]
datasetV$Class_variable <- factor(datasetV$Class_variable, labels=c('No', 'Yes'))
attach(datasetV)

#Confusion matrix
nV <- nrow(datasetV)
glm.probsV <- predict.glm(glm, newdata=datasetV, type='response')
glm.predV <- rep('No', nV)
glm.predV[glm.probsV>threshold(glm.probsV, datasetV$Class_variable)]='Yes'
(confMatV <- addmargins(table(glm.predV, datasetV$Class_variable)))
(accuracyV <- (confMatV[1,1]+confMatV[2,2])/nV*100)
(ErrV <- 100-accuracyV)

#plot
library(verification)
roc.plot(dataset$Class_variable=="Yes", glm.probs)$roc.vol
roc.plot(datasetV$Class_variable=='Yes', glm.probsV)$roc.vol
