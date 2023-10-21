#1. Briefly answer the following questions:
#  (a) What is the general idea of ensemble learning?
#      Ensemble learning is a machine learning concept in which the idea is to train multiple models using the same learning algorithm. 
#      The ensemble of models, also known as “base models”, are then combined in a way that they produce superior results. 
#      The main principle behind ensemble learning is that a group of “weak learners” can come together to form a “strong learner”.
#      The result of an ensemble learning method can offer better prediction, more stability, and more accuracy than a single model. 
#      Some popular types of ensemble methods are bagging, boosting, stacking and random forest, each with different approaches to train the base models and combine their results.
#  (b) What is the difference between bagging and boosting?
#      Bagging: Bagging, or Bootstrap Aggregating, involves creating multiple subsets of the original data, training a model on each subset, and then combining the outputs. Each subset is created by randomly drawing instances with replacement from the original dataset -- hence the term "bootstrapping". The final output typically uses majority voting for classification, or averaging for regression. Bagging helps to reduce variance, and thus helps to prevent overfitting. The Random Forest algorithm is an example of a bagging method.
#      Boosting: Boosting involves iteratively learning weak classifiers with respect to a distribution and adding them to a final strong classifier. After each round, it changes the sampling distribution by increasing the weights of the wrongly predicted instances and decreasing the ones of the correctly predicted instances. Therefore, new weak learners focus more on the instances that previous learners classified incorrectly. This results in models that complement each other and helps to decrease bias and increase prediction strength. Gradient Boosting and AdaBoost are examples of boosting methods.
#      In summary, Bagging and Boosting are similar in that they both involve combining weak learners to produce a strong model, but Bagging helps reduce variance and is typically used when the model is complex and tends to overfit, while Boosting helps reduce bias and is typically used when the model is weak and tends to underperform.
#  (c) What are the advantages and disadvantages of both?
#      Advantages of Bagging:
#        * It helps to decrease the model's variance, not its bias. Therefore, it works best for high-variance low-bias models (complex models).
#        * Bagging methods work effectively not only with homogeneous but also with heterogeneous weak learners.
#        * It can significantly improve the stability and accuracy of machine learning algorithms, especially in the case of decision trees.
#        * The training of models can be parallelized since each model is independently trained.
#      Disadvantages of Bagging:
#        * While bagging helps to improve the stability and generalization of the model by reducing variance, it does not help to avoid overfitting. In fact, bagging of overfit models tends to have an overfitting problem as well.
#        * Bagging can be computationally expensive and time-consuming, as it requires multiple iterations to generate multiple sub-datasets and train different models.
#        * If the base models are biased, the aggregated predictions might also be biased.
#        * Unlike boosting, bagging methods do not pay attention to instances in the training set that are harder to classify, which could be advantageous in some scenarios. 
#      Advantages of Boosting:
#        * It reduces both the bias and variance, hence can improve on both underfitting and overfitting models.
#        * It is very effective in predictive modeling, often giving superior results on the data where complex relationships are involved.
#        * It can be used with many different types of base models (weak learners).
#        * It can handle a mix of discrete and continuous features.
#        * It places more weight on misclassified data points during model training, improving their classification in the subsequent models.
#      Disadvantages of Boosting:
#        * Boosting can be sensitive to noisy data and outliers and therefore might lead to overfitting on such data.
#        * It is typically more computationally expensive than Bagging as the base models are constructed sequentially, hence it's difficult to parallelize.
#        * The parameters of the boosting algorithm are harder to tune than for other models and can sometimes lead to poor performance if not set correctly.
#        * It may lead to overfitting if the number of weak learners (base models) becomes excessive.
#        * It is more complex to visualize and interpret compared to some simpler models like decision trees.

#Install libraries
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
if(!require('naniar')) {
  install.packages('naniar')
  library('naniar')
}
if(!require('magrittr')) {
  install.packages('magrittr')
  library('magrittr')
}
if(!require('tree')) {
  install.packages('tree')
  library('tree')
}
if(!require('randomForest')) {
  install.packages('randomForest')
  library('randomForest')
}
if(!require('gbm')) {
  install.packages('gbm')
  library('gbm')
}


#Load the CSV by doing the following
dataset <- read.csv('data.csv')

#Missing values
print(paste0('Amount of missing values: ', sum(is.na(dataset))))

#Applying median imputation
dataset <- data.frame(lapply(dataset, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))

#Split the dataset into training set (80%) and test set (the remaining 20%)
smp_size <- floor(0.80 * nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

#Train a decision tree with the tree() function from the tree library and predict the output variable from the test set.
tree_model <- tree(median_house_value ~ ., data = train)
tree_predictions <- predict(tree_model, test)

#Calculate the MSE for the tree test data.
mse = mean((tree_predictions - test$median_house_value)^2)
print(paste0('Tree MSE: ', mse))

#RandomForest
random_forest_model <- randomForest(median_house_value ~ ., data = train, mtry = 13)
random_forest_predictions <- predict(random_forest_model, test)

# Calculate MSE
mse <- mean((test$median_house_value - random_forest_predictions)^2)
print(paste0('Random tree with 13 features MSE: ', mse))

#What is the default number of bootstrap replications used in the randomForest function? 
#Train another bagging model with 30 bootstrap replications by specifying the corresponding parameter in the randomForest() function. How does the MSE for the test data change? 
# Answer: The randomForest() function in R has a default value of 500 for the ntree parameter, which specifies the number of bootstrap replications or the number of trees to grow.
random_forest_model2 <- randomForest(median_house_value ~ ., data = train, mtry = 13, ntree = 30)
random_forest_predictions2 <- predict(random_forest_model2, test)

# Calculate MSE
mse <- mean((test$median_house_value - random_forest_predictions2)^2)
print(paste0('Random tree with 30 trees MSE: ', mse))

#Now fit a random forest model using the default settings of the function randomForest. 
#Compute the MSE for this model.
random_forest_model3 <- randomForest(median_house_value ~ ., data = train)
random_forest_predictions3 <- predict(random_forest_model3, test)

# Calculate MSE
mse <- mean((test$median_house_value - random_forest_predictions3)^2)
print(paste0('Default random tree MSE: ', mse))

#Now vary the parameter mtry in the randomForest function in a loop from the default value to the maximum possible value in this case. 
#Which mtry value yields the smallest MSE in the test data
best_mse <- Inf
best_mtry <- NULL
p <- 13
for (mtry in floor(sqrt(p)):p) {
  model <- randomForest(median_house_value ~ ., data = train, mtry = mtry)
  predictions <- predict(model, test)

  mse <- mean((test$median_house_value - predictions)^2)
  if (mse < best_mse) {
    best_mse <- mse
    best_mtry <- mtry
  }
}

print(paste("Best mtry: ", best_mtry)) #5
print(paste("Best MSE: ", best_mse))   #2330243434.68722

#For this “best” model set the hyperparameter importance = TRUE in the randomForest() function. 
#Run importance(YOUR randomForest FUNCTION). What do the two measures of variable importance tell? Explain briefly.
best_random_forest_model <- randomForest(median_house_value ~ ., data = train, mtry = 5, importance = TRUE)
importance_table <- importance(best_random_forest_model)
print(importance_table)

# median_income - is the most important feature

#Use the gbm() function from the gbm library to build a boosting model with 5000 trees 
#and the number 4 for interaction.depth, which sets the depth of the tree models included. 
#Check the MSE.
train$ocean_proximity <- as.factor(train$ocean_proximity)
test$ocean_proximity <- as.factor(test$ocean_proximity)

gbm_model <- gbm(median_house_value ~ ., 
                 data = train, n.trees = 5000, interaction.depth = 4)
gbm_predictions <- predict(gbm_model, newdata = test, n.trees = 5000)
gbm_mse <- mean((test$median_house_value - gbm_predictions)^2)
print(paste0('GBM MSE: ', gbm_mse)) #2089397118.006


# Which of the models computed in this assignment gives the best MSE for the test data?
#Answer: gbm model
