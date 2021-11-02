# Polynomial Regression
Packages <- c('caTools', 'ggplot2')
install.packages(Packages)
lapply(Packages, library, character.only = TRUE)

# Path setting
getwd()
setwd("~/Downloads/R Project/Ministry of Transport/ML Algorithms")

# Importing the dataset
dataset = read.csv('data.csv')

# Fitting Polynomial Regression to the dataset
dataset$CrashYear2 = dataset$CrashYear^2
dataset$CrashYear3 = dataset$CrashYear^3
dataset$CrashYear4 = dataset$CrashYear^4
dataset$CrashYear5 = dataset$CrashYear^5
poly_reg = lm(formula = NumberOfCrashes ~ ., data = dataset)

# Predicting a new result
predict(poly_reg, data.frame(CrashYear = 2021,
                             CrashYear2 = 2021^2,
                             CrashYear3 = 2021^3,
                             CrashYear4 = 2021^4,
                             CrashYear5 = 2021^5))

# Visualizing the Polynomial Regression results
ggplot() +
  geom_point(aes(x = dataset$CrashYear, 
                 y = dataset$NumberOfCrashes),
                 colour = 'red') +
  geom_line(aes(x = dataset$CrashYear, 
                y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Number of crashes in New Zealand by Year (Polynomial Regression)') +
  xlab('Crash Year') +
  ylab('Number of Crashes')
