library(tidyverse)
library(caret)
library(dslabs)
data(heights)

heights <- heights %>% mutate(height_cm = height * 2.54)

# define the outcome and predictors
y <- heights$sex
x <- heights$height_cm

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# predict the outcome by simple guessing
# format y_hat as factor/category (same format as the y vector)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# predict outcome: x > 156 inches == "Male"
heights %>%
  group_by(sex) %>%
  summarize(mean(height_cm), sd(height_cm))
y_hat <- ifelse(x > 156, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 20 cutoffs
cutoff <- seq(150, 170)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height_cm > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test_set$height_cm > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)

# maximize F-score
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height_cm > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
max(F_1)

# find the best model
best_cutoff <- cutoff[which.max(F_1)]
y_hat <- ifelse(test_set$height_cm > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)
mean(y_hat == test_set$sex)

# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)
