###########################
# Data Mining Group Project
# John Henry, Bao Nguyen, Mason O connor
#############################


# Functions to use:

#Mean Squared Error Function
MSE <- function(y_pred, y_true) {
  mean((y_pred - y_true)^2)
}

# R^2 Adjusted function
R2_adjusted <- function(y_pred, y_true) {
  SS_res <- sum((y_true - y_pred)^2)
  SS_tot <- sum((y_true - mean(y_true))^2)
  R2= 1 - SS_res/SS_tot
  n = length(y_true)
  p = 1
  R2_adjusted = 1 - (1-R2)*(n-1)/(n-p-1)
  return(R2_adjusted)
}

#r^2 Function
R2 <- function(y_pred, y_true) {
  SS_res <- sum((y_true - y_pred)^2)
  SS_tot <- sum((y_true - mean(y_true))^2)
  1 - SS_res/SS_tot
}

# Random Forest Function
random_forest <- function(training_set, test_set, dataset) {
  
  library(randomForest)
  library(plotly)
  library(caret)
  
  # Do a random forest regression with 500 trees
  
  regressor = randomForest(x = training_set[-1], y = training_set$victims, 
                           ntree = 500)
  
  # Predicting the Test set results
  y_pred = predict(regressor, newdata = test_set[-1])
  
  # Show summary of y pred
  print(summary(y_pred))
  
  # Show head of y pred
  print(head(y_pred))
  
  # Show summary of regressor
  print(summary(regressor))
  
  
  # Visualising the Random Forest Regression results
  # (Show wid (width) vs. victims)
  # (Have red points for y_true, and blue points for y_pred)

 
 
  # Show r^2 and MSE
  
  adj_r2 = R2_adjusted(y_pred, test_set$victims)
  r2 = R2(y_pred, test_set$victims)
  mse = MSE(y_pred, test_set$victims)
  
  print(paste("Random Forest Regression Adjusted R^2: ", adj_r2))
  print(paste("Random Forest Regression R^2: ", r2))
  print(paste("Random Forest Regression MSE: ", mse))
  
  
  
  results <- data.frame(y_true = test_set$victims, y_pred = y_pred, wid = test_set$wid)
  
  
  library(ggplot2)
  
  ggplot(results, aes(x = wid, y = y_true)) +
    geom_point(color = "red") +
    geom_point(aes(y = y_pred), color = "blue") +
    labs(x = "wid", y = "victims")
  
  ggplot(results, aes(x = wid, y = y_true)) +
    geom_point(color = "red") +
    geom_point(aes(y = y_pred), color = "blue") +
    labs(x = "wid", y = "victims") +
    annotate("text", x = Inf, y = Inf, label = paste0("Adjusted R-squared = ", round(r2, 3)), hjust = 1, vjust = 1)


    return (adj_r2, r2, mse)
}



# Data PreProcessing

#Load the Data

dataset <- read.csv("us_tornado_dataset_1950_2021.csv")


# Drop the 'st' column (lat long cover it) (the 3rd column)
dataset = subset(dataset, select = -c(st))


# Drop the 'year', 'date', 'day' columns (date is covered, year day irrelevant)

dataset = subset(dataset, select = -c(date, yr, dy))


# Encode month into seasons

# Spring = 0, Summer = 1, Fall = 2, Winter = 3
# Spring months: 3, 4, 5
# Summer months: 6, 7, 8
# Fall months: 9, 10, 11
# Winter months: 12, 1, 2

# Create a new column called 'season' and set it to 0

dataset$season = 0

# Replace values in 'season' column that are equal to 0 with values from 'mo' column

dataset$season[dataset$mo == 3] = 0
dataset$season[dataset$mo == 4] = 0
dataset$season[dataset$mo == 5] = 0
dataset$season[dataset$mo == 6] = 1
dataset$season[dataset$mo == 7] = 1
dataset$season[dataset$mo == 8] = 1
dataset$season[dataset$mo == 9] = 2
dataset$season[dataset$mo == 10] = 2
dataset$season[dataset$mo == 11] = 2
dataset$season[dataset$mo == 12] = 3
dataset$season[dataset$mo == 1] = 3
dataset$season[dataset$mo == 2] = 3

# Drop the 'mo' column
dataset = subset(dataset, select = -c(mo))


# If ending elat is 0, replace with slat. (unknown ending lat)
# Same with elon


dataset$elat = ifelse(dataset$elat == 0, dataset$slat, dataset$elat)

dataset$elon = ifelse(dataset$elon == 0, dataset$slon, dataset$elon)



# If magnitude is -9, replace with the average magnitude of the ones that are not -9

# get all rows where mag is not -9

dataset$mag[dataset$mag == -9] = NA

# get average of mag

mag_avg = mean(dataset$mag, na.rm = TRUE)

# replace -9 with mag_avg

dataset$mag[is.na(dataset$mag)] = mag_avg


# Get victims (fatalities + injuries)

dataset$victims = dataset$inj + dataset$fat

# Drop the 'inj' and 'fat' columns

dataset = subset(dataset, select = -c(inj, fat))


# Splitting the dataset into the Training set and Test set


library(caTools)

source("randomforest.R")


# We will do 10 trials and take the average of the results



adjr2s = c()
r2s = c()
mses = c()

for (i in 1:10) {
  # Do not set seed for splitting. 
  split = sample.split(dataset$wid, SplitRatio = 0.80)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)

  # Call random forest function, and append results to vectors
  results = random_forest(training_set, test_set, dataset)
  adjr2s = c(adjr2s, results[1])
  r2s = c(r2s, results[2])
  mses = c(mses, results[3])
}

# Take average of vectors

adjr2_avg = mean(adjr2s)
r2_avg = mean(r2s)
mse_avg = mean(mses)

# Print results

print(paste("Random Forest Regression Adjusted R^2 Average: ", adjr2_avg))
print(paste("Random Forest Regression R^2 Average: ", r2_avg))
print(paste("Random Forest Regression MSE Average: ", mse_avg))
