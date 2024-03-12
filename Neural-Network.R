rownames(dataset) <- dataset$X
dataset <- dataset[2:10]
train_data <- as.matrix(scale(dataset[1:8]))
train_labels <- as.numeric(dataset$Class_variable)


library(keras)
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 6, activation = 'relu', kernel_initializer='uniform', input_shape = c(8)) %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units = 1, activation = 'sigmoid', kernel_initializer='uniform')
summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

#validation
rownames(datasetV) <- datasetV$X
datasetV <- datasetV[2:10]
val_data <- as.matrix(scale(datasetV[1:8]))
val_labels <- as.numeric(datasetV$Class_variable)

set.seed(2612)
history <- model %>% fit(
  train_data, train_labels, 
  epochs =100, batch_size = 5, 
  validation_data = list(val_data, val_labels),
  verbose=0)
plot(history)


N <- nrow(dataset)
nn.pred <- rep('No', N)
nn.pred[model %>% predict(train_data) > threshold(model %>% predict(train_data), dataset$Class_variable)]='Yes'
(confMat <- addmargins(table(nn.pred, dataset$Class_variable)))
(accuracy <- (confMat[1,1]+confMat[2,2])/N*100)
(Err <- 100-accuracy)

nV <- nrow(datasetV)
nn.predV <- rep('No', nV)
nn.predV[model %>% predict(val_data) > threshold(model %>% predict(val_data), datasetV$Class_variable)]='Yes'
(confMat <- addmargins(table(nn.predV, datasetV$Class_variable)))
(accuracy <- (confMat[1,1]+confMat[2,2])/nV*100)
(Err <- 100-accuracy)

library(verification)
predictnn <- model %>% predict(train_data)
roc.plot(dataset$Class_variable=="1", predictnn)$roc.vol
predictnnV <- model %>% predict(val_data)
roc.plot(datasetV$Class_variable=='1', predictnnV)$roc.vol

rocplot1 <- roc.plot(x = (dataset$Class_variable == "Yes"), pred = cbind(random.probs[,2], glm.probs, predictnn), main = "ROC curve train set", legend = T, leg.text = c("Random Forest", "Logistic Regression", "Neural Network"))
rocplot1 <- roc.plot(x = (datasetV$Class_variable == "Yes"), pred = cbind(random.probsV[,2], glm.probsV, predictnnV), main = "ROC curve validation set", legend = T, leg.text = c("Random Forest", "Logistic Regression", "Neural Network"))

