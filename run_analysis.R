library("dplyr")
# helper function
transformActivity <- function(number) {
    if (number == 1) {
        "walking"
    } else if (number == 2) {
        "walking_upstairs"
    } else if (number == 3) {
        "walking_downstrairs"
    } else if (number == 4) {
        "sitting"
    } else if (number == 5) {
        "standing"
    } else if (number == 6) {
        "laying"
    }
}

setwd("~/Dropbox/coursera data science/gettingData/project/UCI HAR Dataset")

#import trainingset
X_train <- read.csv("train/X_train.txt", sep="", header=FALSE)
y_train <- read.csv("train/y_train.txt", sep="", header=FALSE)
sub_train <- read.csv("train/subject_train.txt", sep="", header = FALSE)

#import testset
y_test <- read.csv("test/y_test.txt", sep="", header=FALSE)
X_test <- read.csv("test/X_test.txt", sep="", header=FALSE)
sub_test <- read.csv("test/subject_test.txt", sep="", header = FALSE)

# convert activity number to activity name
train_activities <- sapply(y_train[,1], transformActivity)
test_activities <- sapply(y_test[,1], transformActivity)

# merge activity data with measurement data
train <- cbind(train_activities, sub_train, X_train)
test <- cbind(test_activities, sub_test, X_test)

# give activity and subject columns in both sets appropriate names to allow concatination
names(train)[1:2] <- c("activity", "subject")
names(test)[1:2] <- c("activity", "subject")

# concatinate test and train sets
total <- rbind(train, test)

# read the feature names, to be used as column names
featureNames <- read.csv("features.txt", header=FALSE, stringsAsFactors = FALSE, sep="")
# add feature names to merged data set, after making them unique
unique_feature_names <- make.names(featureNames[,2], unique = TRUE)
colnames(total) <- c("activity", "subject", make.names(featureNames[,2], unique = TRUE))

# create a data table
total_dt <- tbl_df(total)

# select only columns with measurements representing mean and standard deviation
total_meanstd <- select(total_dt, c(1:2, contains(".mean."), contains(".std.")) )

# group data set by activity and subject
total_grouped <- group_by(total_meanstd, activity, subject)

# summarize means per activity and subject
summarized_means <- summarise_each(total_grouped, funs(mean))

write.table(summarized_means, "summarized_means.txt", row.names = FALSE)
