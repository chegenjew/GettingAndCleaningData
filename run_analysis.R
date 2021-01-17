#Step 1 : Merge the training and test data sets 

    #load the tidyverse package 
    library(tidyverse)
    
    #read the X_train and X_test Data set as a fixed length variable  from the X_train.txt file 
      X_train <-  read_fwf(".\\UCI_HAR_Dataset\\train\\X_train.txt", fwf_widths (c(rep(16, 561))))
      
      X_test <-  read_fwf(".\\UCI_HAR_Dataset\\test\\X_test.txt", fwf_widths (c(rep(16, 561))) )
    
    #read  all the train and test subjects data , then update the column headers for the dataframe
      subject_train <- read_fwf(".\\UCI_HAR_Dataset\\train\\subject_train.txt", fwf_widths (c(1)))
      subject_test <- read_fwf(".\\UCI_HAR_Dataset\\test\\subject_test.txt", fwf_widths (c(1)))
    
      subject_train <- subject_train %>% rename(subject  = X1)
      subject_test <- subject_test %>% rename(subject  = X1)
    
    #read  activity labels and provide a column header 
      Y_train <- read_fwf(".\\UCI_HAR_Dataset\\train\\y_train.txt", fwf_widths (c(1)))
      Y_test <- read_fwf(".\\UCI_HAR_Dataset\\test\\y_test.txt", fwf_widths (c(1)))
      
      Y_train <- Y_train %>% rename(activity = X1)
      Y_test <- Y_test %>% rename(activity = X1)
      
    #read  variable names from the features.txt file 
      varNames <- read.delim(".\\UCI_HAR_Dataset\\features.txt", sep = " ", header = F)
    
    #change the column names to a character vector by selecting the 2nd col of varNames
      varNames <- varNames[,2]
    
    #rename column names to meaningful names
      names(X_train) <- varNames
      names(X_test) <- varNames
    
    #merge the training/Test data information into one data set 
      trainData <- cbind(subject_train, Y_train, X_train )
      testData <- cbind(subject_test, Y_test, X_test )
      
    #merge the Training data and Test data into one 
      myData <- rbind(trainData, testData)
      
#step 2 : Extract measurements for mean and standard deviation 
      newData <- myData %>% select(matches("subject")|matches("activity")|contains("mean")|contains("std"))
   
         
#step 3 : use descriptive activity names to label the activities
    newData$activity <- factor(newData$activity,
                               levels = c(1, 2, 3, 4, 5, 6),
                               labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", 
                                          "SITTING", "STANDING", "LAYING") ) 
    
    
#step 4: Appropriately labels the data set with descriptive variable names
    #Already done as part of the step 1 
    
#step 5 : New independent dataset showing the mean 
    meanData <- newData %>% group_by(subject, activity) %>%
                              summarise_all(mean)
      