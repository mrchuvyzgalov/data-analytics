#Install libraries
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
if(!require('caret')) {
  install.packages('caret')
  library('caret')
}
if(!require('rpart')) {
  install.packages('rpart')
  library('rpart')
}


#Load the CSV by doing the following
dataset <- read.csv('data.csv')


#Splitting data into train and test groups
smp_size <- floor(0.70 * nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

train_charges <- train$charges
test_charges <- test$charges

train$charges <- NULL
test$charges <- NULL


# Create the dummy variable
#train
dmy <- dummyVars(" ~ .", data = train)
train <- data.frame(predict(dmy, newdata = train))

#test
dmy <- dummyVars(" ~ .", data = test)
test <- data.frame(predict(dmy, newdata = test))


# Identify near zero variance variables
#train
nzv <- nearZeroVar(train, saveMetrics= TRUE)
train <- train[, !nzv$nzv | !nzv$zeroVar]

#test
nzv <- nearZeroVar(test, saveMetrics= TRUE)
test <- test[, !nzv$nzv | !nzv$zeroVar]


# Compute the correlation matrix
#train
correlationMatrix <- cor(train)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
train <- train[,-highlyCorrelated]

#test
correlationMatrix <- cor(test)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
test <- test[,-highlyCorrelated]


# Identify linear combinations
#train
linear_combos <- findLinearCombos(train)

if (length(linear_combos$linearCombos) > 0) {
  train <- train[,-linear_combos$remove]
}

#test
linear_combos <- findLinearCombos(test)

if (length(linear_combos$linearCombos) > 0) {
  test <- test[,-linear_combos$remove]
}


# Create the preprocessing object with specified steps
#train
preproc <- preProcess(train, method = c("center", "scale", "BoxCox"))
train <- predict(preproc, train)

#test
preproc <- preProcess(test, method = c("center", "scale", "BoxCox"))
test <- predict(preproc, test)


#Add the response variable charges 
train$charges <- train_charges
test$charges <- test_charges


#Fit multiple linear regression model
mlr_model <- lm(charges ~ ., data = train)
print(summary(mlr_model))
print(mlr_model$coefficients)
#print(plot(mlr_model))


#Fit regression tree
rt_model <- rpart(charges ~ ., data = train)
print(summary(rt_model))


#Comparing MLR and RT models: Tree is better by MSE
mlr_pred <- predict(mlr_model, test)
mlr_mse = mean((mlr_pred - test$charges)^2)
print(paste('MLR MSE score: ', round(mlr_mse, 8)))

rt_pred <- predict(rt_model, test)
rt_mse = mean((rt_pred - test$charges)^2)
print(paste('RT MSE score: ', round(rt_mse, 8)))

