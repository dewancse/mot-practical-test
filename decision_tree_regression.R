# Decision Tree Regression
Packages <- c('caTools', 'rpart', 'ggplot2')
install.packages(Packages)
lapply(Packages, library, character.only = TRUE)

# Path setting
getwd()
setwd("~/Downloads/R Project/Ministry of Transport/ML Algorithms")

# Importing the dataset
dataset = read.csv('data.csv')

# Fitting Decision Tree Regression to the dataset
regressor = rpart(formula = NumberOfCrashes ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 5))

# Predicting a new result with Decision Tree Regression
predict(regressor, data.frame(CrashYear = 2021))

# Visualizing the Decision Tree Regression results
ggplot() +
  geom_point(aes(x = dataset$CrashYear, y = dataset$NumberOfCrashes),
             colour = 'red') +
  geom_line(aes(x = dataset$CrashYear, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Number of crashes in New Zealand by Year (Decision Tree Regression)') +
  xlab('Crash Year') +
  ylab('Number of Crashes')
