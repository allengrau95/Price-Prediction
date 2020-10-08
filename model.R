#Set working directory
setwd("O:/Chem/CDCM/Chemistry/Diplom/2020 Allen Grau/06_Price Prediction Web App")
# Importing libraries
library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)
library(Metrics)

# Importing the data set
rfanalysis<-read.csv("02102020_CompleteDataSetRF_05.csv",header=TRUE, sep=";", fileEncoding = "UTF-8-BOM")

str(rfanalysis)
rfanalysis$Time<-as.numeric(rfanalysis$Time)
str(rfanalysis)
set.seed(100)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(rfanalysis$Price, p=0.8, list = FALSE)
TrainingSet <- rfanalysis[TrainingIndex,] # Training Set
TestingSet <- rfanalysis[-TrainingIndex,] # Test Set

#make sure the data types are correct for all datasets
TestingSet$Kilo<-as.numeric(TestingSet$Kilo)
TestingSet$Time<-as.numeric(TestingSet$Time)
TrainingSet$Time<-as.numeric(TrainingSet$Time)

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
TestSet <- read.csv("testing.csv", header = TRUE)
TestSet <- TestSet[,-1]

str(TrainSet)
TrainSet$Time<-as.numeric(TrainSet$Time)
str(TestSet)
TestSet$Kilo<-as.numeric(TestSet$Kilo)
TestSet$Time<-as.numeric(TestSet$Time)
str(TrainSet)
str(TestSet)

# Building Random forest model

model <- randomForest(Price ~ ., data = TrainSet, ntree = 1000, mtry = 3, importance = TRUE)


# Save model to RDS file
saveRDS(model, "model.rds")

#read the model from he rds file
read.Model<-readRDS("model.rds")

#make predictions
model.training <-predict(model,TrainingSet)
model.testing <-predict(model,TestingSet)


result<-TestingSet
result['Prediction']<-model.testing
result



