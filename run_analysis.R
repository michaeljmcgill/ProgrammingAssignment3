### HOUSEKEEPING

library(dplyr)

# Set the wd and download the required files

setwd("C:\\Users\\micha\\Documents\\JohnsHopkins\\JohnsHopkins_Course3_GettingAndCleaningData\\Week_4\\ProgrammingAssignment3")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = ".\\data.zip")

unzip(".\\data.zip")

file.rename(".\\UCI HAR Dataset", "UCI_HAR_Dataset")

# Load the required files into R

features <- read.table(".\\UCI_HAR_Dataset\\features.txt")[[2]]
activity_lables <- read.table(".\\UCI_HAR_Dataset\\activity_labels.txt",
                              colClasses = c("integer", "character"))

x_train <- read.table(".\\UCI_HAR_Dataset\\train\\x_train.txt")
y_train <- read.table(".\\UCI_HAR_Dataset\\train\\y_train.txt",
                      colClasses = "factor")
subject_train <- read.table(".\\UCI_HAR_Dataset\\train\\subject_train.txt")

x_test <- read.table(".\\UCI_HAR_Dataset\\test\\x_test.txt")
y_test <- read.table(".\\UCI_HAR_Dataset\\test\\y_test.txt",
                     colClasses = "factor")
subject_test <- read.table(".\\UCI_HAR_Dataset\\test\\subject_test.txt")

### QUESTION 1
### Merge the training and the test sets to create one data set

# Merge the testing and training datasets

merged_data_ungrouped <- rbind(x_train, x_test)

### QUESTION 2
### Extract only the measurements on the mean and standard deviation
### for each measurement

# Create a boolean mask to identify columns including "mean()" and "std()"

features_boolean <- sapply("mean()", grepl, features, fixed = TRUE) |
                    sapply("std()", grepl, features, fixed = TRUE)

# Subset out unneeded measurements from the datasets

merged_data_ungrouped <- merged_data_ungrouped[,features_boolean]
features <- features[features_boolean]

### QUESTION 3
### Use descriptive activity names to name the activities in the data set

y_test[[1]] <- factor(y_test[[1]],
                      levels = activity_lables[[1]],
                      labels = activity_lables[[2]])

y_train[[1]] <- factor(y_train[[1]],
                       levels = activity_lables[[1]],
                       labels = activity_lables[[2]])

### QUESTION 4
### Appropriately label the data set with descriptive variable names

# Create a function to tidy each measurement name

cleanName <- function(x) {
        
        name <- gsub("-", "_", x)
        name <- gsub("\\()", "", name)
        name <- gsub("mean", "Mean", name)
        name <- gsub("std", "StandardDeviation", name)
        
}

# Apply function to all names

features <- sapply(features, cleanName)

# Apply the measurement names to each column in the merged data set

colnames(merged_data_ungrouped) <- features

### QUESTION 5
### Create a second, independent tidy data set with the
### average of each variable for each activity and each subject

# Group the testing and training datasets by subject and activity

subject <- rbind(subject_train, subject_test)
colnames(subject) <- "subject"

y <- rbind(y_train, y_test)
colnames(y) <- "activity_label"

merged_data_grouped <- cbind(subject, y, merged_data_ungrouped)
merged_data_grouped <- group_by(merged_data_grouped, activity_label, subject)

# Find the mean of each column

average_grouped <- summarize_all(merged_data_grouped, mean)

### EXTRACT THE TIDY DATASET

write.table(average_grouped, file = ".\\tidydata.txt", row.names = FALSE)