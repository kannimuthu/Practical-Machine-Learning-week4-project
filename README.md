# Practical-Machine-Learning-week4-project

# Importing Libraries
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
library(repmis)

# Load Datasets
training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

# Removing NA's
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

# Trimming columns which are not useful
training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]

# Training dataset size (Observations - Variables)
dim(training)

# Testing dataset size (Observations - Variables)
dim(testing)

set.seed(10000) 
inTraining <- createDataPartition(training$classe, p = 0.6, list = FALSE)
training0 <- training[inTraining, ]
validation0 <- training[-inTraining, ]

# Training dataset size (Observations - Variables)
dim(training0)

# Validation dataset size (Observations - Variables)
dim(validation0)

control <- trainControl(method = "cv", number = 5)
fit_rpart <- train(classe ~ ., data = training0, method = "rpart", 
                   trControl = control)

print(fit_rpart, digits = 4)

fancyRpartPlot(fit_rpart$finalModel)
fit_rf <- train(classe ~ ., data = training0, method = "rf", 
                trControl = control)
print(fit_rf, digits = 4)
predict_rf <- predict(fit_rf, validation0)

# Model Validation 
# Show prediction result
(conf_rf <- confusionMatrix(validation0$classe, predict_rf))
Correct_Classe = LETTERS[c(2,1,2,1,1,5,4,2,1,1,2,3,1,5,5,1,2,2,2)]

(accuracy_rpart <- conf_rpart$overall[1])
##  Accuracy 

(predict(fit_rpart, testing))

Correct_Classe

(accuracy_rf <- conf_rf$overall[1])
##  Accuracy 
(predict(fit_rf, testing))
Correct_Classe
