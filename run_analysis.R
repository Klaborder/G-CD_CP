#Author: Karina Laborde

library(dplyr)

#Download data
urldata <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(urldata, destfile = "datasets/data.zip", mode = "wb")
unzip("datasets/data.zip", exdir = "datasets")

#read data training
X_train <- read.table("datasets/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("datasets/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("datasets/UCI HAR Dataset/train/subject_train.txt")

#read data testing
X_test <- read.table("datasets/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("datasets/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("datasets/UCI HAR Dataset/test/subject_test.txt")


#read 'activity_labels.txt': Links the class labels with their activity name.
activity_label <- read.table("datasets/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = F)

#read 'features.txt': List of all features.
feature_list <- read.table("datasets/UCI HAR Dataset/features.txt", stringsAsFactors = F)


####### Step 1 ####### 
#Merges the training and the test sets to create one data set.

#merge training
df_training <- cbind(subject_train, X_train, y_train)

#merge testing
df_test <- cbind(subject_test, X_test, y_test)

#total
total_df <- rbind(df_training, df_test)
summary(total_df$V563)


###### Step 4 #######
#Appropriately labels the data set with descriptive variable names. 

var_names <- c("subject_id", feature_list$V2, "activity_id")
colnames(total_df) <- var_names

colnames(activity_label) <- c("activity_id", "activity_label")


###### Step 2 #######
#Extracts only the measurements on the mean and standard deviation for each measurement. 

mean_std_measure <- total_df[, grepl("subject_id|activity_id|mean()|std()", var_names)]

###### Step 3 #######
#Uses descriptive activity names to name the activities in the data set

mean_std_measure2 <- merge(mean_std_measure, activity_label, by= "activity_id")
mean_std_measure2$activity_id <- NULL

###### Step 5 #######
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

mean_subject_activity <- mean_std_measure2 %>%
  group_by(subject_id, activity_label) %>%
  summarize_all(mean, na.rm = T)

write.table(mean_subject_activity, "tidy_dataset_klaborde.txt", row.names = FALSE, quote = FALSE)
