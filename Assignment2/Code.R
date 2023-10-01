#Load the CSV by doing the following

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
dataset <- read.csv('data.csv')

#Delete empty columns
dataset <- dataset[, colSums(is.na(dataset)) != nrow(dataset)]

#Install libraries
if(!require('MASS')) {
  install.packages('MASS')
  library('MASS')
}
if(!require('caret')) {
  install.packages('caret')
  library('caret')
}
if(!require('randomForest')) {
  install.packages('randomForest')
  library('randomForest')
}


#Structure of the dataset
print(str(dataset))

#Remove id column
dataset$id <- NULL

#Classes
print(table(dataset$diagnosis))

#Turn diagnosis into factors
dataset$diagnosis <- factor(dataset$diagnosis, levels = c("B", "M"))

#Turn the values of the target feature into percentages
print(round(prop.table(table(dataset$diagnosis)) * 100, digits = 1))

#Getting statistics
print(summary(dataset))

#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_normalized <- as.data.frame(lapply(dataset[2:31], normalize))

#Split the dataset into training and test datasets
training_set <- cbind(data_normalized[1:469, ], diagnosis = dataset$diagnosis[1:469])
test_set <- cbind(data_normalized[470:569, ], diagnosis = dataset$diagnosis[470:569])

#F1
f1_score <- function(predictions) {
  cm <- table(predictions, test_set$diagnosis)
  precision <- cm[1,1] / sum(cm[1,])
  recall <- cm[1,1] / sum(cm[,1])
  return (2 * ((precision * recall) / (precision + recall)))
}

#LDA
lda_fit <- lda(diagnosis ~ ., data = training_set)
lda_predictions <- predict(lda_fit, test_set)$class

print(paste('LDA f1 score: ', round(f1_score(lda_predictions), 8)))

#QDA
qda_fit <- qda(diagnosis ~ ., data = training_set, )
qda_predictions <- predict(qda_fit, test_set)$class

print(paste('QDA f1 score: ', round(f1_score(qda_predictions), 8)))

#Logistic Regression Classifier
lrc_fit <- glm(diagnosis ~ ., family = binomial(logit), data = training_set)
lrc_predictions_probs <- predict(lrc_fit, test_set)
lrc_predictions <- ifelse(lrc_predictions_probs <= 0.5, 'B', 'M')
lrc_predictions <- factor(lrc_predictions, levels = c("B", "M"))

print(paste('LRC f1 score: ', round(f1_score(lrc_predictions), 8)))

#k-NN
for (i in 1:9) {
  tuneGrid <- expand.grid(.k = i)
  knn_fit <- train(diagnosis ~ ., data = training_set, method = 'knn', tuneGrid = tuneGrid)
  knn_predictions <- predict(knn_fit, test_set)
  
  print(paste('KNN with k=', i,' f1 score: ', round(f1_score(knn_predictions), 8)))
}

#Random Forest
rf_fit <- randomForest(diagnosis ~ ., data = training_set)
rf_predictions <- predict(rf_fit, test_set)

print(paste('RF f1 score: ', round(f1_score(rf_predictions), 8)))

