#    Merges the training and the test sets to create one data set.
#    Extracts only the measurements on the mean and standard deviation for each measurement.
#    Uses descriptive activity names to name the activities in the data set
#    Appropriately labels the data set with descriptive variable names.
#    From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
filepathtest<-"C:/Users/advisori/Documents/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/"
filepathtrain<-"C:/Users/advisori/Documents/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/"
filepath<-"C:/Users/advisori/Documents/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"

main_data_test<-c("subject_test.txt","X_test.txt","Y_test.txt")
main_data_train<-c("subject_train.txt","X_train.txt","Y_train.txt")

activityLabels <- read.table(paste(filepath,"activity_labels.txt",sep=""))
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table(paste(filepath,"features.txt",sep=""))
features[,2] <- as.character(features[,2])

# Extract only the data on mean and standard deviation
featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)


# Load the datasets
train <- read.table(paste(filepathtrain,main_data_train[2],sep=""))[featuresWanted]
train.activities <- read.table(paste(filepathtrain,main_data_train[3],sep=""))
train.subjects <- read.table(paste(filepathtrain,main_data_train[1],sep=""))
train <- cbind(train.subjects, train.activities, train)

test <- read.table(paste(filepathtest,main_data_test[2],sep=""))[featuresWanted]
test.activities <- read.table(paste(filepathtest,main_data_test[3],sep=""))
test.subjects <- read.table(paste(filepathtest,main_data_test[1],sep=""))
test <- cbind(test.subjects, test.activities, test)

# merge datasets and add labels
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresWanted.names)

# turn activities & subjects into factors
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

write.table(allData.mean, paste(filepath,"tidy.txt",sep=""), row.names = FALSE, quote = FALSE)