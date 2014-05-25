# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive activity names.
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#1. Merge the training and the test sets to create one data set.
#Load variable names
activity.recognition.labels <- read.table('./UCI HAR Dataset/activity_labels.txt')
activity.recognition.features <- read.table('./UCI HAR Dataset/features.txt')[, 2]

#Load trainig data sets
training.dataset.subject <- read.table('./UCI HAR Dataset/train/subject_train.txt')
training.dataset.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
training.dataset.y <- read.table('./UCI HAR Dataset/train/y_train.txt')


#Load test data sets
test.dataset.subject <- read.table('./UCI HAR Dataset/test/subject_test.txt')
test.dataset.x <- read.table('./UCI HAR Dataset/test/X_test.txt')
test.dataset.y <- read.table('./UCI HAR Dataset/test/y_test.txt')


#Merge the data sets
dataset.x <- rbind(training.dataset.x, test.dataset.x)
dataset.y <- rbind(training.dataset.y, test.dataset.y)
dataset.subject <- rbind(training.dataset.subject, test.dataset.subject)

## set descriptive activity names
colnames(dataset.x) <- activity.recognition.features
colnames(dataset.y) <- "activityId"
colnames(dataset.subject) <- "subject"

#Requirement 2: Extracts only the measurements on the mean and standard deviation for each measurement.
dataset.x.meansAndStandardDeviationsOnly =
  dataset.x[, grep("-mean\\(\\)|-std\\(\\)", activity.recognition.features, value=TRUE)]

#Requirement 3: Uses descriptive activity names to name the activities in the data set
colnames(activity.recognition.labels) <- c("activityId", "activity")

#Requirement 4: Appropriately labels the data set with descriptive activity names.
# join the activity with descriptive names
dataset.y.withActivityLabels <- merge(dataset.y, activity.recognition.labels)
# join measurements dataset with activity labels
dataset.x.meansAndStandardDeviationsOnly.withActivityLabels <-
  cbind(dataset.x.meansAndStandardDeviationsOnly, dataset.y.withActivityLabels["activity"]) 

write.csv(dataset.x.meansAndStandardDeviationsOnly.withActivityLabels, "activity_recognition_means_and_stddevs.txt")
