dataset <- read.csv("dataset_filtered.csv")

# print dataset columns
print(colnames(dataset))

library("cvTools")
library("caret")

library("ranger")

# install.packages("ranger")

set.seed(43)
train_index <- sample(seq_len(nrow(dataset)), 0.8*nrow(dataset))
test_index <- setdiff(seq_len(nrow(dataset)), train_index)

train_data <- dataset[train_index,]
test_data <- dataset[test_index,]

rfTrainControl <- trainControl(method="cv", number=10)
rfModel <- train(ALLSKY_SFC_SW_DWN ~ RH2M + T2M + PS + PRECTOTCORR_SUM + cld + pet, data=train_data, method="ranger", trControl=rfTrainControl)
rfModel
rfModel$finalModel
predictedTest <- predict(rfModel, test_data, interval="prediction")
png("r_output/rfPredictedTest.png")
plot(predictedTest, test_data$ALLSKY_SFC_SW_DWN, xlab="Predicted", ylab="Actual", main="Random Forest Regression Model")
abline(0, 1)
dev.off()
mean((test_data$ALLSKY_SFC_SW_DWN - predictedTest)^2)

predictedTest <- cbind(predictedTest, data_type="test")

predictedTrain <- predict(rfModel, train_data, interval="prediction")
predictedTrain <- cbind(predictedTrain, data_type="train")

predictedTrain <- cbind(predictedTrain, train_data)
predictedTest <- cbind(predictedTest, test_data)

colnames(predictedTrain)[1] <- "prediction"
colnames(predictedTest)[1] <- "prediction"

colnames(predictedTrain)
colnames(predictedTest)

predictedTrainTest <- rbind(predictedTrain, predictedTest)
write.csv(predictedTrainTest, "r_output/predictedTest_rfModel.csv")
