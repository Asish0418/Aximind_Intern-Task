#We will use the Credit card dataset 'Default' for implementation of ML model. We'll be using a logistic regression model for interpreting the affect of high balances on the rate of default of the credit cards.
install.packages("ISLR")
library("ISLR")
data("Default")
View(Default)

#We are asked to make a model and train it by having 70% of data in the training dataset.
#So we have to 1st split the dataset accordingly.
install.packages("caret")
library("caret")
set.seed(1)
idx <- createDataPartition(y = Default$default, times = 1, p = 0.7, list = FALSE)
train <- Default[idx,]
test <- Default[-idx,]
#Just take a look at the proportions of the Yes/No ratio. (It stays same for the original dataset as well as the splitted ones.)
prop.table(table(train$default))

#Now make model and train it on the training dataset.
train_glm <- glm(default~balance, data = train, family = binomial(link = 'logit'))
#Predict on the test data.
install.packages("broom")
library("broom")
predict_glm <- augment(train_glm, newdata = test, type.predict = 'response')
predict_glm
#We have successfully predicted the model on the test set in terms of probability values.

#Lets check the accuracy! For that, change the probability values to rounded values.
library("dplyr")
default_plus <- train_glm %>% augment(type.predict = 'response') %>% mutate(defaulted_hat = round(.fitted))
default_plus %>% select(default, balance, .fitted, defaulted_hat)
#We'll use confusion matrix method.
default_plus %>% select(default, defaulted_hat) %>% table()

#Now comes an important thing, that if we consider the values as defaults, we would miss many cases that are actually defualt but are not showed in the accuracy stats as it's values depended. That's where the accuracy is affected for our model.
#So lets have a probability threshold (Pitfalls), basically it's the accuracy validation of the model.

#Take mean of the probability predictions on the train dataset and assign 'Yes' for defaulted cases if values > mean value or else assign 'No'.
mean_def <- train_glm %>% augment(type.predict = 'response') %>% summarise(mean = mean(.fitted))

#Now predict the defaults in the test set using the probability threshold.
predict_glm %>% mutate(defaulted_hat = if_else(.fitted > mean_def, "Yes", "No")) %>% select(default, defaulted_hat) %>% table()
#Here the defaulted_hat has only the No values, which quite shows the disadvantage of logistic regression as evaluation metrices.


