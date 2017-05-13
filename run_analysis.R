# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#check data.table library
if (!require("data.table")) {
  install.packages("data.table")
}

require("data.table")

#load data
activity_labels <- read.table(".R/Getting and Cleaning Data Course Project/activity_labels.txt")
features <- read.table(".R/Getting and Cleaning Data Course Project/features.txt")

X_test <- read.table(".R/Getting and Cleaning Data Course Project/test/x_test.txt")
Y_test <- read.table(".R/Getting and Cleaning Data Course Project/test/y_test.txt")
subject_test <- read.table(".R/Getting and Cleaning Data Course Project/test/subject_test.txt")

X_train <- read.table(".R/Getting and Cleaning Data Course Project/train/X_train.txt")
Y_train <- read.table(".R/Getting and Cleaning Data Course Project/train/y_train.txt")
subject_train <- read.table(".R/Getting and Cleaning Data Course Project/train/subject_train.txt")

colnames(X_test) <- features[,2] 
colnames(Y_test) <- "activity"
colnames(subject_test) <- "subject"

colnames(X_train) <- features[,2] 
colnames(Y_train) <- "activity"
colnames(subject_train) <- "subject"

colnames(activity_labels) <- c('activity','activityType')

# Merges the training and the test sets to create one data set.
train <- cbind(Y_train, subject_train, X_train)
test <- cbind(Y_test, subject_test, X_test)
dataAll <- rbind(train, test)

# Extract only the measurements on the mean and standard deviation for each measurement.
colNames <- colnames(dataAll)
meanStd <- (grepl("activity" , colNames) | grepl("subject" , colNames) | 
                   grepl("mean.." , colNames) | grepl("std.." , colNames))
meanStddata <- dataAll[ , meanStd == TRUE]

# Uses descriptive activity names to name the activities in the data set
activityNamesData <- merge(meanStddata, activity_labels, by='activity',all.x=TRUE)

# Appropriately labels the data set with descriptive variable names.
names(activityNamesData)<-gsub("^t", "time", names(activityNamesData))
names(activityNamesData)<-gsub("^f", "frequency", names(activityNamesData))
names(activityNamesData)<-gsub("Acc", "Accelerometer", names(activityNamesData))
names(activityNamesData)<-gsub("Gyro", "Gyroscope", names(activityNamesData))
names(activityNamesData)<-gsub("Mag", "Magnitude", names(activityNamesData))
names(activityNamesData)<-gsub("BodyBody", "Body", names(activityNamesData))

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(. ~subject + activity, activityNamesData, mean)
tidyData <- tidyData[order(tidyData$subject, tidyData$activity),]
write.table(tidyData, file = "tidydata.txt",row.name=FALSE)


