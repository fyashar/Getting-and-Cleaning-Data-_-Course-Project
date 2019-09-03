##Calling dplyr package from the library
library(dplyr)

## Reading "features.txt" file the 2nd Column of which will be used to rename xtest dataframe columns.
features <- read.table(file = "./UCI HAR Dataset/features.txt", header = FALSE)

## Reading "X_test.txt" and "X_train.txt" file, and renaming the columns using a vector extracted from the above "features" variable. This should satisfy part 4 of the assignment.
xtest <- read.table(file = "./UCI HAR Dataset/test/X_test.txt", header = FALSE, col.names = features$V2)
xtrain <- read.table(file = "./UCI HAR Dataset/train/X_train.txt", header = FALSE, col.names = features$V2)


## Reading "y_test.txt" file and "y_train.txt" file, and renaming the column to "activity.class". 
ytest <- read.table(file = "./UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = "activity.class")
ytrain <- read.table(file = "./UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = "activity.class")

## Reading "subject_test.txt" file, and renaming the column to "subject.id". 
subjecttest <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subject.id")
subjecttrain <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subject.id")

## Merging 3 sets of entities - subjects, activities, and readings.Then adding a new column named "origin" with values of "test" or "train" to determine the origin of data.
merged.test.data <- tbl_df(cbind(subjecttest, ytest, xtest))
merged.test.data$origin <- "test"
merged.train.data <- tbl_df(cbind(subjecttrain, ytrain, xtrain))
merged.train.data$origin <- "train"

## Merging the training and the test sets to create one data set.
merged.data.set <- rbind(merged.train.data, merged.test.data)

## Identifying the columns labels that contain "Mean", "mean", "Std" , or "std".
selected_columns <- grep("[Mm]ean|[Ss]td", names(merged.data.set), value = TRUE)
## Extracting only the measurements on the mean and standard deviation for each measurement.
extracted_dataset <- select(merged.data.set, subject.id, activity.class, origin, selected_columns)

## Reading "activity_labels.txt" file. The descriptive activity names in this file will be used to name the activities in the merged data set.
activity_labels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("activity_id", "activity_description"))
renamed.activity <- tbl_df(merge(x = activity_labels, y = merged.data.set, by.x = "activity_id", by.y = "activity.class", sort = FALSE))

## Creating a tidy data set with the average of each variable for each activity and each subject.
tidy.data <- renamed.activity %>% group_by(activity_description, subject.id) %>% summarise_all(mean)
write.table(tidy.data, file = "./tidydata.txt", row.names = FALSE)

## Creating codebook
token.dictionary <- read.csv(file = "./token dictionary.csv", header = TRUE)

pattern <- token.dictionary$Token
replacement <- token.dictionary$Description
labels <- names(tidy.data)

description <- mgsub(pattern, replacement, labels, trailspace = TRUE, fixed = FALSE)
 
for (i in seq_along(tidy.data)) {
  attr(tidy.data[[i]], which = "shortDescription") <- description[[i]]
}

library(dataMaid)
dataMaid::makeCodebook(tidy.data, replace = TRUE)
