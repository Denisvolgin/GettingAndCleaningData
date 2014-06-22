library(reshape2)

loadTestData <- function() {
  
  dataColumns <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  allData <- read.table("test/X_test.txt", header=F, col.names=dataColumns$MeasureName)
  data <- allData[, grep(".*mean\\(\\)|.*std\\(\\)", dataColumns$MeasureName)]

  y_data <- read.table("test/y_test.txt", header=F, col.names=c("ActivityID"))
  data$ActivityID <- y_data$ActivityID
  
  subjectData <- read.table("test/subject_test.txt", header=F, col.names=c("SubjectID"))
  data$SubjectID <- subjectData$SubjectID
  
  return(data)
}

loadTrainingData <- function() {
  
  dataColumns <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  allData <- read.table("train/X_train.txt", header=F, col.names=dataColumns$MeasureName)
  data <- allData[, grep(".*mean\\(\\)|.*std\\(\\)", dataColumns$MeasureName)]
  
  y_data <- read.table("train/y_train.txt", header=F, col.names=c("ActivityID"))
  data$ActivityID <- y_data$ActivityID
  
  subjectData <- read.table("train/subject_train.txt", header=F, col.names=c("SubjectID"))
  data$SubjectID <- subjectData$SubjectID
  
  return(data)
}

tidyUpData <- function() {
  
  #merge test and training data
  data <- rbind(readTestData(), readTrainData())
  
  #fix the non-standard column names
  columnNames <- colnames(data)
  columnNames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
  columnNames <- gsub("\\.+std\\.+", cnames, replacement="Std")
  colnames(data) <- columnNames
  
  #add the activity name column
  activityLabels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
  activityLabels$ActivityName <- as.factor(activityLabels$ActivityName)
  mergedAndlabeledData <- merge(data, activityLabels)
  
  # melt the dataset
  activityAndSubjectVariables = c("ActivityID", "ActivityName", "SubjectID")
  measureVariables = setdiff(colnames(mergedAndlabeledData), activityAndSubjectVariables)
  tidyData <- melt(mergedAndlabeledData, id=activityAndSubjectVariables, measure.vars=measureVariables)
  
  # recast
  dcast(tidyData, ActivityName + SubjectID ~ variable, mean)
}

# Create the tidy data set and save it on to the named file
saveTidyData <- function(tidyDataFileName) {
  write.table(tidyUpData(), tidyDataFileName)
}

createTidyDataFile("tidy.txt")
