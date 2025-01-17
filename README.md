# GettingAndCleaningData
Week  4  course project , peer graded assignment
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The Below operations have been perfomed on the original data through the use of the run_analysis.R script 
#Step 1 : Merge the training and test data sets 

    #load the tidyverse package 
        
    #read the X_train and X_test Data set as a fixed length variable  from the X_train.txt file 
      X_train <-  read from the X_train.txt file 
      
      X_test <-  read from X_test.txt
    
    #read  all the train and test subjects data , then update the column headers for the dataframe
      subject_train <- read from the subject_train.txt 
      subject_test <- read from the subject_test.txt
    
      subject_train <- rename the column header to meaningfull names 
      subject_test <- rename the column header to meaningfull names 
    
    #read  activity labels and provide a column header 
      Y_train <- read from y_train.txt
      Y_test <- read from y_test.tx
      
      Y_train <- rename the column header to meaningfull names 
      Y_test <- rename the column header to meaningfull names 
      
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
      
