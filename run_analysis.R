### HOUSEKEEPING

library(dplyr)

# Set the wd and download the required files

setwd("C:\\Users\\micha\\Documents\\JohnsHopkins\\JohnsHopkins_Course3_GettingAndCleaningData\\Week_4\\ProgrammingAssignment3")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = ".\\data.zip")

unzip(".\\data.zip")

file.rename(".\\UCI HAR Dataset", "UCI_HAR_Dataset")

# Load the required files into R

features <- read.table(".\\UCI_HAR_Dataset\\features.txt")
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

# Find the mean and st dev of each column

mean_ungrouped <- summarize_all(merged_data_ungrouped, mean)
sd_ungrouped <- summarize_all(merged_data_ungrouped, sd)

mean_and_sd_ungrouped <- data.frame(mean = t(mean_ungrouped),
                                    sd = t(sd_ungrouped))

# label each measurement

rownames(mean_and_sd_ungrouped) <- paste(features[,1], features[,2])

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

colnames(x_train) <- paste(features[,1],features[,2])
colnames(y_train) <- "activity_label"
colnames(subject_train) <- "subject"

colnames(x_test) <- paste(features[,1],features[,2])
colnames(y_test) <- "activity_label"
colnames(subject_test) <- "subject"

merged_train <- cbind(subject_train, y_train, x_train)
merged_test <- cbind(subject_test, y_test, x_test)

### QUESTION 5
### Create a second, independent tidy data set with the
### average of each variable for each activity and each subject

# Merge and group the testing and training datasets

merged_data_grouped <- rbind(merged_train, merged_test)
merged_data_grouped <- group_by(merged_data_grouped, activity_label, subject)

# Find the mean of each column

mean_grouped <- summarize_all(merged_data_grouped, mean)

### EXTRACT THE TIDY DATASETS

write.csv(mean_and_sd_ungrouped, file = ".\\tidydata1.csv")
write.csv(mean_grouped, file = ".\\tidydata2.csv")