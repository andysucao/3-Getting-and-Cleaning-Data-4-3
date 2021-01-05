rm(list = ls())
library(Hmisc)
library(plyr)
library(reshape2)
library(dplyr)



# 1 - Merges the training and the test sets to create one data set

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",header = FALSE, sep = "", dec = ".")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE, sep = "", dec = ".")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt",header = FALSE, sep = "", dec = ".")


subject_Y_test <- cbind(subject_test, Y_test)
colnames(subject_Y_test) <- c("subject", "activity")
# ftable(subject_Y_test)
test1 <- cbind(subject_Y_test, X_test)


subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE, sep = "", dec = ".")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE, sep = "", dec = ".")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt",header = FALSE, sep = "", dec = ".")


subject_Y_train <- cbind(subject_train, Y_train)
colnames(subject_Y_train) <- c("subject", "activity")
# ftable(subject_Y_train)
train1 <- cbind(subject_Y_train, X_train)


merged1 <- merge(test1, train1, all = TRUE)
rm(subject_test, X_test, Y_test, subject_Y_test, test1, subject_train, X_train, Y_train, subject_Y_train, train1)




# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

features1 <- read.table("./UCI HAR Dataset/features.txt",header = FALSE, sep = "", dec = ".")
mean1 <- grepl("mean\\(", features1$V2)
stdev1 <- grepl("std\\(", features1$V2)
subset1 <- mean1 | stdev1
features2 <- features1[subset1, ]

# Add "True" for subject and activity in merged1 data frame
subset2 <- c(as.logical(1), as.logical(1), subset1)
merged2 <- merged1[, subset2]

rm(features1, mean1, stdev1, subset1, subset2, merged1)




# 3 - Uses descriptive activity names to name the activities in the data set

activity1 <- read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE, sep = "", dec = ".")
activity2 <- activity1[ , 2]
activity3 <- tolower(activity2)
activity4 <- gsub("_","-",activity3)

for (ii in 1:6) 
{
      merged2$activity <- gsub(ii, activity4[ii], merged2$activity)
}

rm(activity1, activity2, activity3, activity4, ii)




# 4 - Appropriately labels the data set with descriptive variable names. 

features3 <- tolower(features2[ ,2])
features4 <- gsub("\\(\\)","",features3)

# Add labels for subject and activity in features4
features5 <- c("subject", "activity", features4)

for (ii in 1:length(merged2)) 
{
      colnames(merged2)[ii] <- features5[ii]
}

rm(features2, features3, features4, features5, ii)



# 5 - From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.


merged3 <- split(merged2, merged2$subject)
n <- 0
subject2 <- matrix(0, rep(180))
activity2 <- matrix(0, rep(180))
colMeans2 <- matrix(0, ncol=66, nrow = 180)

for (ii in 1:length(merged3))
{
      merged4 <- merged3[[ii]]
      merged5 <- split(merged4, merged4$activity)
      
      for (jj in 1:length(merged5))
      {
            n <- n+1
            merged6 <- merged5[[jj]]
            merged7 <- merged6[ , 3:68]
            subject2[n] <- ii
            activity2[n] <- jj
            colMeans2[n , ] <- colMeans(merged7)
      }
}

merged8 <- cbind(subject2, activity2, colMeans2)
merged9 <- as.data.frame(merged8)
names(merged9) <- names(merged2)

rm(merged3, merged4, merged5, merged6, merged7, merged8, ii, jj, n, subject2, activity2, colMeans2)

activity1 <- read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE, sep = "", dec = ".")
activity2 <- activity1[ , 2]
activity3 <- tolower(activity2)
activity4 <- gsub("_","-",activity3)

for (ii in 1:6) 
{
      merged9$activity <- gsub(ii, activity4[ii], merged9$activity)
}

rm(activity1, activity2, activity3, activity4, ii)



# Output final data file

write.table(merged2, file = "TidyData1.txt")

write.table(merged9, file = "TidyData2.txt")


























































































































































