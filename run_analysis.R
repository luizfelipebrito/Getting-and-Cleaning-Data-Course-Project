##------------------------------------------------
## 1º Step: Load the libraries
##------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)

##------------------------------------------------
## 2º Step: Loading files 
##------------------------------------------------
subject_test  <- read.table("subject_test.txt")
subject_test  <- tbl_df(subject_test)

subject_train <- read.table("subject_train.txt")
subject_train <- tbl_df(subject_train)

X_test <- read.table("X_test.txt")
X_test <- tbl_df(X_test)

X_train <- read.table("X_train.txt")
X_train <- tbl_df(X_train)

y_test <- read.table("y_test.txt")
y_test <- tbl_df(y_test)

y_train <- read.table("y_train.txt")
y_train <- tbl_df(y_train)

features <- read.table("features.txt")
features <- tbl_df(features)

##------------------------------------------------
## 3º Step: Extracts only the measurements on the 
##          mean and standard deviation for each 
##          measurement 
##------------------------------------------------
features$V2 <- gsub("meanFreq", "MEANFreq", features$V2)
features$V2 <- gsub("\\s*\\(\\)\\s*", "", features$V2)
column_mean <- grep("mean", features$V2)
column_stdv <- grep("std",  features$V2)
column_measurement <- sort(c(column_mean, column_stdv))

X_test  <- select(X_test,  column_measurement)
X_train <- select(X_train, column_measurement)

##------------------------------------------------
## 4º Step: Appropriately labels the data set with 
##          descriptive variable names.
##------------------------------------------------
column_names_data <- c(features[column_measurement, 2])

names(X_test)  <- column_names_data$V2
names(X_train) <- column_names_data$V2

names(subject_test)  <- c("id_subject")
names(subject_train) <- c("id_subject")

names(y_test)  <- c("id_activity")
names(y_train) <- c("id_activity")

##------------------------------------------------
## 5º Step: Uses descriptive activity names to  
##          name the activities in the data set.
##------------------------------------------------
y_test <- mutate(y_test, activity = ifelse(id_activity == 1 , "WALKING", 
                                           ifelse(id_activity == 2 , "WALKING_UPSTAIRS", 
                                                  ifelse(id_activity == 3 , "WALKING_DOWNSTAIRS", 
                                                         ifelse(id_activity == 4 , "SITTING", 
                                                                ifelse(id_activity == 5 , "STANDING", "LAYING")))))  )

y_train <- mutate(y_train, activity = ifelse(id_activity == 1 , "WALKING", 
                                           ifelse(id_activity == 2 , "WALKING_UPSTAIRS", 
                                                  ifelse(id_activity == 3 , "WALKING_DOWNSTAIRS", 
                                                         ifelse(id_activity == 4 , "SITTING", 
                                                                ifelse(id_activity == 5 , "STANDING", "LAYING")))))  )


##------------------------------------------------
## 6º Step: Merges the training and the test sets 
##          to create one data set. 
##------------------------------------------------
data_set_test  <- cbind(y_test[2],  subject_test,  X_test)
data_set_train <- cbind(y_train[2], subject_train, X_train)

data_set_final <- rbind(data_set_train, data_set_test)

##------------------------------------------------
## 7º Step: Creates a second, independent tidy 
##          data set with the average of each 
##          variable for each activity and each
##          subject.
##------------------------------------------------
data_set_final_group_by <- group_by(data_set_final, activity, id_subject)

data_set_final_group_by <- summarise_all(data_set_final_group_by, (funs(mean)))

##------------------------------------------------
## Final Step: View/Write tidy dataset
##------------------------------------------------
View(data_set_final_group_by)
write.table(data_set_final_group_by,  file="data_set_final_group_by.txt", row.name=FALSE)
