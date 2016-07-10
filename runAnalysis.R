# Peer Graded Assignment: Getting and Cleaning Data Course Project
# Vanessa Morgan
# 2016-07-10

# runAnalysis.R description
# this script does the following:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##########

### step 1 - merge the training and test sets

loadData <- function(type) {
        # load data
        folder <- "UCI HAR Dataset/"
        filetype <- ".txt"
        
        subjects <- read.table(paste(folder, type, "/subject_", type, filetype, 
                                     sep = ""))
        y <- read.table(paste(folder, type, "/y_", type, filetype, sep = ""))
        
        featuresName <- read.table(paste(folder, "features", filetype, sep = ""))
        x <- read.table(paste(folder, type, "/X_", type, filetype, sep = ""))
        
        # Put header of the feature value with features name
        names(x) <- featuresName[, 2]
        
        # Put header of the activity value and subject with proper name
        colnames(subjects) <- "subject"
        colnames(y) <- "activity"
        
        # Combine the y ,subject and x together
        df <- cbind(cbind(subjects, y), x)
        return(df)
}
setwd("/Users/vanessamorgan/datasciencecoursera")
test <- loadData("test")
train <- loadData("train")

fullData <- rbind(train, test)

### step 2 - extract mean and standard deviation

featuresNames <- read.table("UCI HAR Dataset/features.txt")
colnames(featuresNames) <- c("fId", "featurename")
keepFeatures = subset(featuresNames, grepl("mean|std", featurename))[, 2]

# select mean and standard deviation
preData <- subset(fullData, select = c(cbind("subject", "activity"), as.character(keepFeatures)))

### step 3 - use descriptive activity name

activityNames <- read.table("UCI HAR Dataset/activity_labels.txt")
fullData$activity <- sapply(fullData$activity, function(x) activityNames[x, 2])

### step 4 - label data with descriptive variable names

library(reshape)
tidyMelted <- melt(preData, id = c("subject", "activity"))

### step 5 - create tidy data set from step 4

tidy <- cast(tidyMelted, subject + activity ~ variable, mean)
dim(tidy)

# output dataset tidy in .txt
write.table(tidy, "tidy.txt")