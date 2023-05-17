# Importing the dataset
dataset = read.csv('us_tornado_dataset_1950_2021.csv')

# Preprocessing
#----------------------------------------------------------------------
# Removing 'year', 'day', 'date' and 'state' columns
dataset = dataset[,-c(1,3,4,5)]

# Bucketing the month column into seasons

# default spring
dataset$season = 0

# summer
dataset$season[dataset$mo == 6] = 1
dataset$season[dataset$mo == 7] = 1
dataset$season[dataset$mo == 8] = 1

# fall
dataset$season[dataset$mo == 9] = 2
dataset$season[dataset$mo == 10] = 2
dataset$season[dataset$mo == 11] = 2

# winter
dataset$season[dataset$mo == 12] = 3
dataset$season[dataset$mo == 1] = 3
dataset$season[dataset$mo == 2] = 3

# dropping the month column
dataset = dataset[, -c(1)]

# Combine injuries and fatalities columns into victims column
dataset$vic = dataset$inj + dataset$fat

# remove injury and fatality columns
dataset = dataset[, -c(2,3)]

# if elat or elon value is 0
# it means ending latitude or longitude are unknown
# so we replace them with its starting longitude or latitude
dataset$elat[dataset$elat == 0.0] = dataset$slat[dataset$elat == 0.0]
dataset$elon[dataset$elon == 0.0] = dataset$slon[dataset$elon == 0.0]

# if magnitude is -9
# replace it with the average of magnitudes that are not -9
known_mag = dataset$mag[dataset$mag != -9]
avg_mag = mean(known_mag)
dataset$mag[dataset$mag == -9] = avg_mag


#----------------------------------------------------------------------

#Splitting the dataset
#----------------------------------------------------------------------
library(caTools)
split = sample.split(dataset$vic, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
#----------------------------------------------------------------------

# Fitting SVR to the training set
#----------------------------------------------------------------------
library(e1071)
regressor = svm(formula = vic ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial',
                tolerance = 0.1)

# radial preforms best when compared to poly, sigmoid, and linear kernels
#----------------------------------------------------------------------

# Making a prediction based on the test set
#----------------------------------------------------------------------
y_pred = predict(regressor, newdata = test_set)

#----------------------------------------------------------------------

# Visualizing the SVR results
#----------------------------------------------------------------------
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$wid, y = test_set$vic),
             colour = 'red') +
  geom_point(aes(x = test_set$wid, y = y_pred),
             colour = 'blue') +
  ggtitle('SVR') +
  xlab('Width') +
  ylab('Victims')
#----------------------------------------------------------------------

# Evaluating the Model Performance
#----------------------------------------------------------------------
num_of_ind_vars = 8
ssr = sum((test_set$vic - y_pred) ^ 2)
sst = sum((test_set$vic - mean(test_set$vic)) ^ 2)
r2 = 1 - (ssr/sst)
print(r2)
r2_adjusted = 1 - (1 - r2) * (length(test_set$vic) - 1) / (length(test_set$vic) - num_of_ind_vars - 1)
print(r2_adjusted)

# Recording r^2 and adjusted r^2
#----------------------------------------------------------------------
# Round 1: R^2 = 0.1899483, adjR^2 = 0.1895635
# Round 2: R^2 = 0.1066539, adjR^2 = 0.1062295
# Round 3: R^2 = 0.1540451, adjR^2 = 0.1536432
# Round 4: R^2 = 0.1428268, adjR^2 = 0.1424196
# Round 5: R^2 = 0.1794842, adjR^2 = 0.1790944
#----------------------------------------------------------------------
