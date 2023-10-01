#1.Load the CSV by doing the following:

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
insurance <- read.csv('insurance.csv', stringsAsFactors = TRUE)

#2.Check the data structure by using the function str()
str(insurance)

#3.Check the statistics and check for normality of the data before a 
#regression task by using the function summary() on the dependent variable charges
summary(insurance$charges)

#4.We can see that the mean value is higher than the median, 
#which indicates a skewed distribution. Take a guess about how it is skewed. 
#Then, visualize it by using a histogram and see the skewed distribution. 
#(Use the function hist().)
hist(insurance$charges)

#5.Regression tasks are straightforward for numeric features. 
#If there are categorical features, special care needs to be taken. 
#We will see later how R handles them by default it if we are going straight 
#to the Regression Model part. Now we are going to look closer to the correlations 
#between those numeric features by using the cor() function.
#[Sidenote: You can extract the numeric variables by 
#subsetting using insurance[c("age", "bmi", "children", "charges")]]
cor(insurance[c('age', 'bmi', 'children', 'charges')])

#6.Visualizing the correlations with the pairs() function. 
#And look at the scatterplot matrix. What do you see? How would you interpret it?
pairs(insurance[c('age', 'bmi', 'children', 'charges')])

#7.With this scatterplot matrix just now, it is quite difficult to find some trends. 
#Try out the pairs.panels() function to plot on 
#insurance[c("age", "bmi", "children", "charges")] and see the difference.
#[Sidenote: Install the package psych in order to use the pairs.panels() function.]
if(!require('psych')) {
  install.packages('psych')
  library('psych')
}
pairs.panels(insurance[c('age', 'bmi', 'children', 'charges')])

#8.What does the loess-curve for age and bmi indicate?

#9.Split the data now in training and test data using 80% 
#for the training data and the rest for
#test data. How many observations are in each data set?
smp_size <- floor(0.80 * nrow(insurance))
train_ind <- sample(seq_len(nrow(insurance)), size = smp_size)

train <- insurance[train_ind, ] # Training data: 1070 obs
test <- insurance[-train_ind, ] # Test data: 268 obs

#10.Create a linear regression model for the training data with the function lm(). 
#You can either write it like:
#.  ins_model <- lm(charges ~ age+children+bmi+sex+smoker+region, data = train)
#or like the following, which is shorter because of the dot. 
#The dot can be used to specify all features:
#   ins_model <- lm(charges ~ ., data = train)
ins_model <- lm(charges ~ age+children+bmi+sex+smoker+region, data = train)

#11.Look at the regression coefficients by typing the name of the model: ins model. 
#Try to interpret the values. How did R handle the categorical features?
ins_model$coefficients

#12.Now, evaluate the regression model by using the summary() 
#function on ins model. What does it tell us? What is the p-value, what does the 
#Multiple R-squared indicate?
summary(ins_model)

#13.Evaluate your model using the test data and compute 
#the mean square prediction errors.
predict_test = predict(ins_model, test)
mse = mean((predict_test - test$charges)^2)
print(mse)

#14.Repeat the steps in questions 9 to 13 ten times and compare training 
#and test error for the different samples. 
#What can you say about bias and variance of these models?
for (i in 1:10) {
  smp_size <- floor(0.80 * nrow(insurance))
  train_ind <- sample(seq_len(nrow(insurance)), size = smp_size)
  
  train <- insurance[train_ind, ] # Training data: 1070 obs
  test <- insurance[-train_ind, ] # Test data: 268 obs
  
  ins_model <- lm(charges ~ age+children+bmi+sex+smoker+region, data = train)
  
  predict_test = predict(ins_model, test)
  mse = mean((predict_test - test$charges)^2)
  print(paste(i, mse, sep=": "))
}
