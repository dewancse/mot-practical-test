# Support Vector Regression
Packages <- c('e1071', 'ggplot2')
install.packages(Packages)
lapply(Packages, library, character.only = TRUE)

# Path setting
getwd()
setwd("~/Downloads/R Project/Ministry of Transport/ML Algorithms")

# Importing the dataset
dataset = read.csv('data.csv')

# Fitting SVR to the dataset
regressor = svm(formula = NumberOfCrashes ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting a new result
predict(regressor, data.frame(CrashYear = 2021))

# Visualizing the SVR results
ggplot() +
  geom_point(aes(x = dataset$CrashYear, y = dataset$NumberOfCrashes),
             colour = 'red') +
  geom_line(aes(x = dataset$CrashYear, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Number of crashes in New Zealand by Year (Support Vector Regression)') +
  xlab('Crash Year') +
  ylab('Number of Crashes')
