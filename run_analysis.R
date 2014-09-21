# ---------------------------------------------------------------------------
# Title : Tidy data project - Cousera.org
# Description: This script has the target to load a raw dataset and get a tidy data frame that is
# easy to be manipulated. It make use of some r commands like read.csv, read.table, save.table,
# cbind, rbind, grep, group_by and others.
# Author: Carlos R Lacerda
# Date : 09/21/2014
# ---------------------------------------------------------------------------

library(dplyr)

# load features and activity names
feature <- read.csv("features.txt", header = FALSE, sep = " ")
act_labels <- read.table("activity_labels.txt", header = FALSE, fill= T)

# load activity training data 
act_train <- read.csv("train/y_train.txt", header = FALSE)
# load subject trainig data
sub_train <- read.csv("train/subject_train.txt", header = FALSE)
# load train data
training <- read.table("train/X_train.txt", header = FALSE, fill = TRUE)

# load activity test data
act_test <- read.csv("test/y_test.txt", header = FALSE)
# load subject test data
sub_test <- read.csv("test/subject_test.txt", header = FALSE)
# load test data
test <- read.table("test/X_test.txt", header = FALSE, fill = TRUE)

# 1. Merges the training and the test sets to create one data set.

all <- rbind(training,test)
# get activity frame
activity <- rbind(act_train, act_test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement
# It seems that the feature name that contains the word "mean" in lower-case reflects the
# calculation of mean value for that feature. That is why I decided to use the following
# command to extract the indices

# extract mean measurements
ic_mean <- grep("mean", feature[,2], ignore.case = FALSE)
# build a data frame with mean
mean_df <- all[,ic_mean]

# extract stardard deviation measurements
ic_dev <- grep("std", feature[,2], ignore.case = FALSE)
dev_df <- all[,ic_dev]

# 3. Uses descriptive activity names to name the activities in the data set
# add activity to data set

mean_dfa <- cbind(activity,mean_df)
# set the labels
mean_dfa[,1] <- factor(mean_dfa[,1], levels = act_labels$V1, labels = as.character(act_labels$V2))

# do the same for standard deviation

dev_dfa <- cbind(activity,dev_df)
# set the labels
dev_dfa[,1] <- factor(dev_dfa[,1], levels = act_labels$V1, labels = as.character(act_labels$V2))

# 4. Appropriately labels the data set with descriptive variable names

# set column names for mean_df
colnames(mean_dfa)[2:(dim(mean_dfa)[2])] <- as.character(feature[ic_mean,2])
# set column names for dev_df
colnames(dev_dfa)[2:(dim(dev_dfa)[2])] <- as.character(feature[ic_dev,2])

# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.

newdata <- cbind(mean_dfa,dev_dfa[,2:34])
colnames(newdata)[1] <- "Activity"
allsub <- rbind(sub_train,sub_test)
allact <- newdata[,1]
# put columns in order activity, subject
data <- newdata[, 2:dim(newdata)[2]]
newdata <- data
newdata_sub <- cbind(allsub,newdata)
colnames(newdata_sub)[1] <- "Subject"
newdata_act_sub <- cbind(allact,newdata_sub)
colnames(newdata_act_sub)[1] <- "Activity"

# group all collected mean by activity and subject
by_act_sub <- group_by(newdata_act_sub, Activity, Subject)
tidydata <- by_act_sub

# save tidy data to file
write.table(tidydata, file = "tidydata.csv")

