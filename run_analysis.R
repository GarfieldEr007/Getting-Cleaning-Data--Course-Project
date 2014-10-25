
## Merging the Training and the Test Sets, naming Variables, extracting Satisfactory Measurements
## and create a new tidy data set as asked

run_analysis <- function() {

subject_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt")    ## read training data
subject_test <- read.table("UCI_HAR_Dataset/test/subject_test.txt")       ## read test data
subject <- rbind(subject_train, subject_test)                             ## merge training and test data
names(subject) <- "subject"                                               ## naming

y_train <- read.table("UCI_HAR_Dataset/train/y_train.txt")                ## read y training data
y_test <- read.table("UCI_HAR_Dataset/test/y_test.txt")                   ## read y  test data
y <- rbind(y_train, y_test)                                               ## merge training and test data
names(y) <- "activity"                                                    ## naming

x_train <- read.table("UCI_HAR_Dataset/train/X_train.txt")                ## read x training data 
x_test <- read.table("UCI_HAR_Dataset/test/X_test.txt")                   ## read x test data
x <- rbind(x_train, x_test)                                               ## merge x training and test data

## read 561 features and assign them to the names of x data
features <- read.table("UCI_HAR_Dataset/features.txt") 
names(x) <- features[, 2] 

## merges the training and the test sets to create one data set
data <- cbind(subject, y, x)    

condition_mean <- grepl("mean()", names(data))                          ## matching column names
condition_std <- grepl("std()", names(data))
condition <- condition_mean | condition_std                             ## merge the condition

##extracts the measurements on the mean and standard deviation for each measurement
meanstd <- data[condition] 

## naming the activities with proper names
data[, "activity"][data[, "activity"] == 1] <- "Walking"
data[, "activity"][data[, "activity"] == 2] <- "Walking_Up"
data[, "activity"][data[, "activity"] == 3] <- "Walking_Down"
data[, "activity"][data[, "activity"] == 4] <- "Sitting"
data[, "activity"][data[, "activity"] == 5] <- "Standing"
data[, "activity"][data[, "activity"] == 6] <- "Laying"

## create a new data.frame from data
average <- data["subject"] 
average <- cbind(average, data[, "activity"]) 

x_t <- t(x)                                             ## the transposition of x
x_t <- as.data.frame(x_t)                               ## turn x_t into data.frame
mean <- sapply(x_t, mean)                               ## the average of each variable  
average <- cbind(average, mean)                         ## add mean to average
names(average) <- c("subject", "activity", "mean")      ## naming

write.table(average, "average.txt", row.name = FALSE)   ## creating txt文件

}
