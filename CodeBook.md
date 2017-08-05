# Getting and Cleaning Data Project

## 0. Downloading data, installing packages, and reading files

### Downloading the data set
filesPath <- "C:/Users/monic/OneDrive/Documents/R/Getting and Cleaning Data"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

### Unzipping the data file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

### Loading the appropriate packages
library(dplyr)
library(data.table)
library(tidyr)

filesPath <- "C:/Users/monic/OneDrive/Documents/R/Getting and Cleaning Data/data/UCI HAR Dataset"

### Reading subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

### Reading activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

### Reading data files
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

## 1. Merge the training sets to create on data set

### Merge the training and the test sets by row binding and rename variables "subject" and "activityNum"

alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

### Combine the training and test files
dataTable <- rbind(dataTrain, dataTest)

### Name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

### Column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

### Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

## 2. Extract only the meansurements on the mean and standard deviation for each measurement

### Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

### Taking only measurements for the mean and standard deviation and add "subject","activityNum"
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

## 3. Use descriptive activity names to name the activities in the data set

### Enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

### Create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr <- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable <- tbl_df(arrange(dataAggr,subject,activityName))

## 4. Appropriately label the data set with descriptive variable names

### Before label changes:
head(str(dataTable),2)

### Change labels to descriptive variable names
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

### After lable changes:
head(str(dataTable),2)

## 5. From data set in #4, create a second, independent tidy data set with the average of each variable for each activity and each subject

### Create dataTable with variable means sorted by subject and Activity
Data2<-aggregate(. ~subject + activityName, dataTable, mean)
Data2<-Data2[order(Data2$subject,Data2$activityName),]

### Writing second tidy data set in text file
write.table(Data2, "TidyData.txt", row.name=FALSE)
