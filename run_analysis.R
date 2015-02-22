install.packages("data.table")
require("data.table")

install.packages("reshape2")
require("reshape2")

# Load features with features.txt
featuresLabels <-read.table("dataset/features.txt", sep ="")[,2]
# Extract only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", featuresLabels)

# Step 1- Process Test Data
# Load and process X_test & y_test data
testData<-read.table("dataset/test/X_test.txt", sep="")
activityTest<-read.table("dataset/test/Y_test.txt", sep="")
subjectTest<-read.table("dataset/test/subject_test.txt", sep="")

# Name the column names based on features Labels
names(testData) = featuresLabels 

# Extract only the measurements on the mean and standard deviation for each measurement.
testData = testData[,extract_features]

# Load activityLabels with activity_labels.txt
activityLabels <-read.table("dataset/activity_labels.txt", sep ="")[,2]

activityTest[,2] = activityLabels[activityTest[,1]]
names(activityTest) = c("Activity_ID", "Activity_Label")
names(subjectTest) = "subject"

# Bind additional 3 columsns to data
testData <- cbind(as.data.table(subjectTest), activityTest, testData)

#######################################################################
# Step 2- Process Train Data
# Load and process X_train & y_train data.
#######################################################################
trainData<-read.table("dataset/train/X_train.txt", sep="")
activityTrain<-read.table("dataset/train/Y_train.txt", sep="")
subjectTrain<-read.table("dataset/train/subject_train.txt", sep="")

# Name the column names based on features Labels
names(trainData) = featuresLabels 

# Extract only the measurements on the mean and standard deviation for each measurement.
trainData = trainData[,extract_features]

# Load activity data
activityTrain[,2] = activityLabels[activityTrain[,1]]
names(activityTrain) = c("Activity_ID", "Activity_Label")
names(subjectTrain) = "subject"

# Bind additional 3 columsns to data
trainData <- cbind(as.data.table(subjectTrain), activityTrain, trainData)

#######################################################################
# Step 3- Merge Train and Test Data
# Combine both training and test data sets using RBind
#######################################################################
mergedData <- rbind(testData, trainData)

###########################################################################
# Adding Appropriately labels the data set with descriptive variable names
###########################################################################
names(mergedData)<-gsub("^t", "time", names(mergedData))
names(mergedData)<-gsub("^f", "frequency", names(mergedData))
names(mergedData)<-gsub("Acc", "Accelerometer", names(mergedData))
names(mergedData)<-gsub("Gyro", "Gyroscope", names(mergedData))
names(mergedData)<-gsub("Mag", "Magnitude", names(mergedData))
names(mergedData)<-gsub("BodyBody", "Body", names(mergedData))

id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(mergedData), id_labels)
melt_data      = melt(mergedData, id = id_labels, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt")
