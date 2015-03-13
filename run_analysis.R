## Read overall files 
## (UCI HAR Dataset folder uploaded to directory from course website and setwd() to UCI HAR Dataset)
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

## Read train files
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
body_acc_x_train <- read.table("./train/Inertial Signals/body_acc_x_train.txt")
body_acc_y_train <- read.table("./train/Inertial Signals/body_acc_y_train.txt")
body_acc_z_train <- read.table("./train/Inertial Signals/body_acc_z_train.txt")
body_gyro_x_train <- read.table("./train/Inertial Signals/body_acc_x_train.txt")
body_gyro_y_train <- read.table("./train/Inertial Signals/body_acc_y_train.txt")
body_gyro_z_train <- read.table("./train/Inertial Signals/body_acc_z_train.txt")
total_acc_x_train <- read.table("./train/Inertial Signals/body_acc_x_train.txt")
total_acc_y_train <- read.table("./train/Inertial Signals/body_acc_y_train.txt")
total_acc_z_train <- read.table("./train/Inertial Signals/body_acc_z_train.txt")

## Read test files
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
body_acc_x_test <- read.table("./test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test <- read.table("./test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test <- read.table("./test/Inertial Signals/body_acc_z_test.txt")
body_gyro_x_test <- read.table("./test/Inertial Signals/body_acc_x_test.txt")
body_gyro_y_test <- read.table("./test/Inertial Signals/body_acc_y_test.txt")
body_gyro_z_test <- read.table("./test/Inertial Signals/body_acc_z_test.txt")
total_acc_x_test <- read.table("./test/Inertial Signals/body_acc_x_test.txt")
total_acc_y_test <- read.table("./test/Inertial Signals/body_acc_y_test.txt")
total_acc_z_test <- read.table("./test/Inertial Signals/body_acc_z_test.txt")

## Label variable V1 in subject_test as "subject" 
## This variable is the subject ID for each observation
library(dplyr)
subject_test <- rename(subject_test, subject=V1)
head(subject_test)

## Label variables in X_test according to the values for variable V2 in features
## These variables are the measurement variables for each observation
names(X_test) <- features$V2
head(X_test)

## Label variable V1 in y_test as "activity_as_factor"
## This variable is the activity ID for each observation
y_test <- rename(y_test, activity_as_factor=V1)
head(y_test)

## Create a test/train variable
## To keep track of which subjects are test or train
test_or_train <- rep("TEST", 2947)

## Combine renamed subject_test, y_test, X_test into one dataframe (df)
## This will yield a df with a subject, an activity, and measurements for each observation
test <- cbind(subject_test, y_test, test_or_train, X_test)
head(test)

## Repeat for train
## This will yield another df with a subject, an activity, and measurements for each observation
subject_train <- rename(subject_train, subject=V1)
names(X_train) <- features$V2
y_train <- rename(y_train, activity_as_factor=V1)
test_or_train <- rep("TRAIN", 7352)
train <- cbind(subject_train, y_train, test_or_train, X_train)
head(train)

## Merge the train and test dfs into one master df
df <- rbind(train, test)
str(df)

## Uses descriptive activity names to name the activities in the data set, ie use characters instead of factors for each
## activity
## Make "activity_as_factor" variable into factor, and change levels from numeric to corresponding character factors
df[, 2] <- as.factor(df[, 2])
levels(df[, 2]) <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
## Make sure col names are formatted correctly before renaming 
valid_column_names <- make.names(names=names(df), unique=TRUE, allow_ = TRUE)
names(df) <- valid_column_names
## Rename "activity_as_factor" to "activity"
df <- rename(df, activity=activity_as_factor)

## Extracts only the measurements on the mean and standard deviation for each measurement
## Find all instances of mean or std in variable names
std_or_mean <- grep(".[Mm]ean.|.std.", names(df), value=T)
## Subset those selected variables 
mean_and_std <- df[ , which(names(df) %in% std_or_mean)]
head(mean_and_std)
## Add back the condition variables (subject, activity, test_or_train) to create new tidy df with mean and std measurements
three <- df[, 1:3]
df_tidy <- cbind(three,mean_and_std)

## Appropriately labels the data set with descriptive variable names
## At this point, labels are already clear, but extra clean-up by removing double-periods to separate characters
names(df_tidy) <- gsub("\\.\\.", "", names(df_tidy))
names(df_tidy)

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for 
## each activity and each subject
## Get rid of test_or_train variable
df_tidy <- df_tidy[, -3]
## Calculate group means organizes first by activity and then by subject
means <- aggregate(df_tidy, list(df$activity, df$subject), mean)
## Reorder columns so that subject comes first
means <- means[ ,-(3:4)]
## Relabel 1st and 2nd column as subject and activity
means <- rename(means, activity=Group.1, subject=Group.2)
means <- means[ , c(2,1,3:88)]



