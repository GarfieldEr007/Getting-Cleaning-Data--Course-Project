
## Merging the Training and the Test Sets, naming Variables, extracting Satisfactory Measurements
## and create a new tidy data set as asked

run_analysis <- function() {

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")    ## read training data
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")       ## read test data
subject <- rbind(subject_train, subject_test)                             	## merge training and test data
names(subject) <- "subject"                                               	## naming

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")                ## read y training data
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")                   ## read y  test data
y <- rbind(y_train, y_test)                                               	## merge training and test data
names(y) <- "activity"                                                    	## naming

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")                ## read x training data 
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")                   ## read x test data
x <- rbind(x_train, x_test)                                               	## merge x training and test data

## read 561 features and assign them to the names of x data
features <- read.table("./UCI HAR Dataset/features.txt") 
names(x) <- features[, 2] 

## merges the training and the test sets to create one data set
data <- cbind(subject, y, x)    

condition_mean <- grepl("mean()", names(data))                          ## matching column names
condition_std <- grepl("std()", names(data))
condition <- condition_mean | condition_std                             ## merge the condition

##extracts the measurements on the mean and standard deviation for each measurement
meanstd <- data[condition] 
data2 <- cbind(subject, y, meanstd)
names(data2) <- c("subject", "activity", names(meanstd))

## data2 is a new data set extracted from data, naming the activities with proper names
data2[, "activity"][data2[, "activity"] == 1] <- "Walking"
data2[, "activity"][data2[, "activity"] == 2] <- "Walking_Up"
data2[, "activity"][data2[, "activity"] == 3] <- "Walking_Down"
data2[, "activity"][data2[, "activity"] == 4] <- "Sitting"
data2[, "activity"][data2[, "activity"] == 5] <- "Standing"
data2[, "activity"][data2[, "activity"] == 6] <- "Laying"

## get the row number and column number of data2
rownum <- nrow(data2)
colnum <- ncol(data2)

## initial variables setting
subject_length <- length(unique(subject[, 1]))
activity_names <- c("Walking", "Walking_Up", "Walking_Down", "Sitting", "Standing", "Laying")
tidy_data <- data.frame(NULL)
x_same <- data.frame(NULL)

for(s in 1:subject_length){           			## for1
	for(a in activity_names){           		## for2

		names <- c(s, a)                		## subject and activity names

		for(i in 1:rownum){               		## for3
			name_temp <- c(data2[i, "subject"], data2[i, "activity"])
			if(all(names == name_temp)){    	## if matching successfully, store the content of this row
			x_same <- rbind(x_same, data2[i, 3:colnum])
			##next
		}                                 		## if
		}                                 		## for3
		
		x_mean <- sapply(x_same, mean)    		## compute the average
		tidy_data <- rbind(tidy_data, c(names, x_mean))   ## add subject, activity and average into tidy data set
		x_same <- data.frame(NULL)        		## assign NUL to it for next loop

		}                                 		## for2
	
}                                     			## for1

names(tidy_data) <- names(data2)      			## set the name of tidy data
write.table(tidy_data, "tidy_data.txt", row.name = FALSE)   ## creating txt file and store the tidy data

}
