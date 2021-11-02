# Random Forest Regression
Packages <- c('randomForest', 'ggplot2')
install.packages(Packages)
lapply(Packages, library, character.only = TRUE)

# Path setting
getwd()
setwd("~/Downloads/R Project/Ministry of Transport/ML Algorithms")

# Importing the dataset
dataset = read.csv('data.csv')

# Fitting Random Forest Regression to the dataset
regressor = randomForest(x = dataset[-2],
                         y = dataset$NumberOfCrashes,
                         ntree = 500)

# Predicting a new result with Random Forest Regression
predict(regressor, data.frame(CrashYear = 2021))

# Visualizing the Random Forest Regression results
ggplot() +
  geom_point(aes(x = dataset$CrashYear, y = dataset$NumberOfCrashes),
             colour = 'red') +
  geom_line(aes(x = dataset$CrashYear, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Number of crashes in New Zealand by Year (Random Forest Regression)') +
  xlab('Crash Year') +
  ylab('Number of Crashes') +
  ggsave("Random-Forest-Regression.png")
