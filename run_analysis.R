renameColumns <- function(dataset) {
  renamed_cols <- gsub("\\.+mean\\.+", colnames(dataset), replacement="Mean")
  renamed_cols <- gsub("\\.+std\\.+",  renamed_cols, replacement="Std")
  colnames(dataset) <- renamed_cols
  dataset
}

mergeDatasets  <- function() {
  print("Starting datasets load and merge...")
  # Read feature names
  dataset.features  <-  read.table("./features.txt", header=FALSE, col.names=c("FeatureId", "FeatureName"))
  
  # Read test data (assign column names from already loaded feature list).
  dataset.test.x  <- read.table("./test/X_test.txt", header=FALSE, col.names=dataset.features$FeatureName)
  dataset.test.y  <- read.table("./test/y_test.txt", header=FALSE, col.names="ActivityId")
  dataset.test.subject  <- read.table("./test/subject_test.txt", header=FALSE, col.names="SubjectId")
  
  # Read train data (assign column names from already loaded feature list).
  dataset.train.x  <- read.table("./train/X_train.txt", header=FALSE, col.names=dataset.features$FeatureName)
  dataset.train.y  <- read.table("./train/y_train.txt", header=FALSE, col.names="ActivityId")
  dataset.train.subject  <- read.table("./train/subject_train.txt", header=FALSE, col.names="SubjectId")
  
  #Merge train and test datasets
  dataset.x <- rbind(dataset.train.x, dataset.test.x)
  dataset.y <- rbind(dataset.train.y, dataset.test.y)
  dataset.subject  <- rbind(dataset.train.subject, dataset.test.subject)

  # names of subset columns required
  subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)",dataset.features$FeatureName)
  # subset the data (done early to save memory)
  dataset.x <- dataset.x[,subset_data_cols]
  
  # Merge x and y datasets.
  dataset.x$ActivityId  <- dataset.y
  
  
  # Merge with subject dataset.
  dataset.x$SubjectId <- dataset.subject
  
  print("Train and test datasets are sucessfully loaded and merged.")
  
  renameColumns(dataset.x)
}

# Add a column with Activity names.
addActivityName <- function(dataset) {
  print("Adding activity names...")
  activity_labels <- read.table("activity_labels.txt", header=FALSE, col.names=c("ActivityId", "ActivityName"))
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  merged_dataset <- merge(dataset, activity_labels)
  merged_dataset
}

# 1. Loads and merges datasets.
# 2. Extracts subset with mean and std measurements.
# 3. Add a column with Activity names.
loadAndProcessDataset <- function() {
  dataset <- extractMeanAndStdMeasurements(mergeDatasets())
  addActivityName(dataset)
}

# Creates a tidy dataset from a given processed (a subset of merged dataset
# with activity names).
extractTidyDataset  <- function(dataset) {
  print("Extracting a tidy dataset.")
  library(reshape2)
  
  # melt the dataset
  var_ids = c("ActivityID", "ActivityName", "SubjectID")
  var_measures = setdiff(colnames(dataset), var_ids)
  melted_data <- melt(dataset, id=var_ids, measure.vars=var_measures)
  
  # Return a tidy dataset.
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

saveDatasetToFile <- function(dataset, fname) {
  print("Saving dataset to the file.")
  write.table(dataset, fname)
}

# Creates a tidy dataset and returns in-memory object to 
# let further dataset processing.
createTidyDatasetFile <- function(dataset_filename) {
  dataset  <- extractTidyDataset(loadAndProcessDataset())
  saveDatasetToFile(dataset, dataset_filename)
  dataset
}

d  <- createTidyDatasetFile("tidy_dataset.txt")
