clean_data <- function(baseDir){
  
  testPath <- paste0(baseDir,"/","test")
  trainPath <- paste0(baseDir,"/","train")
  
  #Load Test Data
  subjectTest <- read.table(paste0(testPath,"/subject_test.txt"))
  experimentDataTest <- read.table(paste0(testPath,"/X_test.txt"))
  ActivityLabelTest <- read.table(paste0(testPath,"/y_test.txt"))
  
  #Load Train data 
  subjectTrain <- read.table(paste0(trainPath,"/subject_train.txt"))
  experimentDataTrain <- read.table(paste0(trainPath,"/X_train.txt"))
  ActivityLabelTrain <- read.table(paste0(trainPath,"/y_train.txt"))
  
  #Load features and add column names for easier processing afterwards
  features <-read.table(paste0(baseDir,"/features.txt"),stringsAsFactors=FALSE)
  colnames(features) <- c("index","column_name")
  
  #Load activity lables and add column names for easier processing afterwards
  activityLables <- read.table(paste0(baseDir,"/activity_labels.txt"),stringsAsFactors=FALSE)
  colnames(activityLables) <- c("activity_number","activity_name")
  
  #1.Merges the training and the test sets to create one data set. 
  subjectMerged<- rbind(subjectTest,subjectTrain)
  colnames(subjectMerged) <- c("subjectID")
  
  experimentDataMerged <- rbind(experimentDataTest,experimentDataTrain)
  
  #Cleaning up column names
  features[,c(2)] <- gsub("mean","Mean",features[,c(2)])
  features[,c(2)] <- gsub("std","Std",features[,c(2)])
  features[,c(2)] <- gsub("\\(","",features[,c(2)])
  features[,c(2)] <- gsub("\\)","",features[,c(2)])
  features[,c(2)] <- gsub("-","",features[,c(2)])
  
  #4. Appropriately labels the data set with descriptive variable names.
  colnames(experimentDataMerged) <- features[,c(2)] 
  
  ActivityLabelMerged <- rbind(ActivityLabelTest,ActivityLabelTrain)
  colnames(ActivityLabelMerged) <- c("activity_number")
  
  #2. Extracts only the measurements on the mean and standard deviation for each measurement.
  columnsNeeded <- grep(".*Mean.*|.*Std.*", features[,2])
  
  #3. Uses descriptive activity names to name the activities in the data set
  enrichedActivities <- inner_join(ActivityLabelMerged,activityLables,c("activity_number" = "activity_number"))
  
  combinedInfo <- cbind(experimentDataMerged[,columnsNeeded],enrichedActivities,subjectMerged)
  
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  tidy <- summarise_all(group_by(combinedInfo,subjectID,activity_name), mean)
  
  #Write out the data set to a file
  write.table(tidy,"tidy.txt",row.names = FALSE,quote = FALSE)
}