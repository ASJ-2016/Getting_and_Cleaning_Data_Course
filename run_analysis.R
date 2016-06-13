#******************************************************************************
#***********                                                          *********
#***********          Created by Alejandro Serrano, June 2016         *********
#***********                                                          *********
#******************************************************************************

# INSTRUCTIONS

# 1. Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each 
#   measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set  
#   with the average of each variable for each activity and each subject.


require(data.table)
require(plyr)


# Clear the environment to ensure that everything is defined and set the wd
rm(list=ls(all=TRUE))
setwd(paste0(COURSERADIR, "./03 - Data Cleaning/Quizes and Assigments"))


# Download and unzip the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
file <- "Dataset.zip"
folder <- "UCI HAR Dataset"
if(!file.exists(file)){ download.file(url, file)} 
if(!file.exists(folder)){ unzip(file, list = FALSE, overwrite = TRUE)}  


# Read the files using "data.table" package
list.files(folder, recursive=TRUE)

# The files that will be used to load data are:
# test: subject_test.txt, X_test.txt, y_test.txt
# train: subject_train.txt, X_train.txt, y_train.txt

    # Subject files
    dtSubjTrain <- fread(file.path(folder, "train", "subject_train.txt"))
    dtSubjTest  <- fread(file.path(folder, "test" , "subject_test.txt" ))
    
    # Activity files
    dtActivTrain <- fread(file.path(folder, "train", "Y_train.txt"))
    dtActivTest  <- fread(file.path(folder, "test" , "Y_test.txt" ))
    dtActivNames <- fread(file.path(folder, "activity_labels.txt"))
    
    # Features files
    dtFeatTrain <- fread(file.path(folder, "train", "X_train.txt"))
    dtFeatTest  <- fread(file.path(folder, "test" , "X_test.txt" ))
    dtFeatNames <- fread(file.path(folder, "features.txt"))$V2
    

# 1. Merges the training and the test sets to create one data set.    
 
    dtSubj <- rbind(dtSubjTrain, dtSubjTest)
    names(dtSubj)<-c("subject")
    
    dtActiv <- rbind(dtActivTrain, dtActivTest)
    names(dtActiv)<- c("activity")
    
    dtFeat <- rbind(dtFeatTrain, dtFeatTest)
    names(dtFeat) <- dtFeatNames
    
    dtAll <- cbind(dtSubj, dtActiv, dtFeat)
    str(dtAll)
    
    
# 2.Extracts only the measurements on the mean and standard deviation for each 
    # measurement.     
    
    Mean_Std_Index <- grep("mean\\(\\)|std\\(\\)",dtFeatNames)
    selection <- c(dtFeatNames[Mean_Std_Index], names(dtSubj), names(dtActiv))
    dtSelection <- subset(dtAll,select=selection)
    str(dtSelection)

    
# 3.Uses descriptive activity names to name the activities in the data set

    dtSelection$activity <- factor(dtSelection$activity, levels=dtActivNames$V1, 
                                   labels=dtActivNames$V2)
    summary(dtSelection$activity)
    

# 4.Appropriately labels the data set with descriptive variable names. 
    
    names(dtSelection) <- gsub("^t", "time_", names(dtSelection))
    names(dtSelection) <- gsub("^f", "frequency_", names(dtSelection))
    names(dtSelection) <- gsub("Acc", "_Accelerometer_", names(dtSelection))
    names(dtSelection) <- gsub("Gyro", "_Gyroscope_", names(dtSelection))
    names(dtSelection) <- gsub("Mag", "_Magnitude", names(dtSelection))
    names(dtSelection) <- gsub("BodyBody", "Body", names(dtSelection))
    str(dtSelection)


# 5.From the data set in step 4, creates a second, independent tidy data set  
    # with the average of each variable for each activity and each subject.
    
    tidydataset <- aggregate(. ~ subject + activity, dtSelection, FUN=mean)
    write.table(tidydataset, "tidydataset.txt", sep="\t") 
    