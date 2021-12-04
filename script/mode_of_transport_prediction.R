library(caret)
library(e1071)

# read data
cars <- read.csv("../Data/mode_of_transport.csv")

# convert numerical values of categorical column to factors(categories)
cars$Engineer = as.factor(cars$Engineer)
cars$MBA = as.factor(cars$MBA)
cars$license = as.factor(cars$license)
cars$Gender = as.factor(cars$Gender)
cars$Transport = as.factor(cars$Transport)

# create data partition
random <- createDataPartition(cars$Transport, p=0.70, list=FALSE)

# split data into train and test
train_set = cars[random,]
test_set = cars[-random,]


# KNN
trControl <- trainControl(method = "cv", number = 10)
fit.knn <- train(Transport ~ ., method = "knn",
                 tuneGrid = expand.grid(k=2:20),
                 trControl=trControl,
                 metric = "Accuracy",
                 preProcess = c("center", "scale"),
                 data = train_set)

# KNN prediction on train set
knn_pred_train <- predict(fit.knn, train_set)
# KNN prediction on test set
knn_pred_test <- predict(fit.knn, test_set)