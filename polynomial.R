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
split = sample.split(dataset$vic, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
#----------------------------------------------------------------------

# Build the polynomial regression model with backward elimination
#----------------------------------------------------------------------
# Start with a full model with all the predictor variables
# poly_model <- lm(vic ~ poly(distance, 3) + season + mag, data = training_set)

# Step 1: Fit the full model with all variables
#poly_model <- lm(vic ~ poly(mag, 3) + slat + slon +
#                  elat + elon + poly(len, 1) +
#                   poly(wid, 1) , data = training_set)

#summary(poly_model)

poly_model = lm(formula = vic ~ ., data = training_set)

poly_model_opt = lm(formula = vic ~ wid + slat + slon + elat + elon + len + mag +season, data = training_set)
summary(poly_model_opt)

#poly_model <- lm(vic ~ poly(mag,1)+ poly(slat,3) + poly(slon,3) + poly(len,2) , data = training_set)
#poly_model = lm(vic ~len + wid + mag + slat + slon + elat + elon, data = training_set)
#summary(poly_model)
#Remove season

poly_model_opt = lm(formula = vic ~ wid + slat + slon + elat + elon + len + mag, data = training_set)
summary(poly_model_opt)

poly_model_opt = lm(formula = vic ~ wid + slat + slon + elat + elon  + wid, data = training_set)
summary(poly_model_opt)

#poly_model_opt = lm(formula = vic ~ mag + len + wid, data = training_set)
#summary(poly_model_opt)


# Making a prediction based on the test set
#----------------------------------------------------------------------
y_pred = predict(poly_model_opt, newdata = test_set)

# Visualizing the polynomial regression results
#----------------------------------------------------------------------
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$wid, y = test_set$vic),
             colour = 'red') +
  geom_point(aes(x = test_set$wid, y = y_pred),
            colour = 'blue') +
  ggtitle('Polynomial Regression') +
  xlab('Width of Tornado') +
  ylab('Victims')



# Evaluating the Model Performance
#----------------------------------------------------------------------
num_of_ind_vars = 7
ssr = sum((test_set$vic - y_pred) ^ 2)
sst = sum((test_set$vic - mean(test_set$vic)) ^ 2)
r2 = 1 - (ssr/sst)
print(r2)
r2_adjusted = 1 - (1 - r2) * (length(test_set$vic) - 1) / (length(test_set$vic) - num_of_ind_vars - 1)
print(r2_adjusted)

library(ggplot2)
x_grid = seq(min(training_set$wid), max(training_set$wid), 0.1)
ggplot() +
  geom_point(aes(x = test_set$wid, y = test_set$vic),
             colour = 'red') +
  geom_point(aes(x = x_grid, y = predict(poly_model_opt,
                                         newdata = data.frame('wid' = x_grid,
                                                              'slat' = mean(training_set$slat),
                                                              'slon' = mean(training_set$slon),
                                                              'elat' = mean(training_set$elat),
                                                              'elon' = mean(training_set$elon),
                                                              'len' = mean(training_set$len),
                                                              'mag' = mean(training_set$mag)))),
             colour = 'blue') +
  ggtitle('Polynomial Regression') +
  xlab('Magnitude') +
  ylab('Victims')