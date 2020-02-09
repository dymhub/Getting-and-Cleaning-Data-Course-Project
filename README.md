---
output:
  html_document: default
  pdf_document: default
---
Getting & Cleaning Data Course Project
----------
This is a code book that describes the variables, the data, and any transformations or work that I have performed to clean up the UCI data for this project.

The data in question may be found here :
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Some information about the data provided by the team that collected it may be found here:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### The Data

List of all files in the data set:

 [1] "./UCI HAR Dataset/activity_labels.txt"                         
 [2] "./UCI HAR Dataset/features_info.txt"                           
 [3] "./UCI HAR Dataset/features.txt"                                
 [4] "./UCI HAR Dataset/README.txt"                                  
 [5] "./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt"   
 [6] "./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt"   
 [7] "./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt"   
 [8] "./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt"  
 [9] "./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt"  
[10] "./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt"  
[11] "./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt"  
[12] "./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt"  
[13] "./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt"  
[14] "./UCI HAR Dataset/test/subject_test.txt"                       
[15] "./UCI HAR Dataset/test/X_test.txt"                             
[16] "./UCI HAR Dataset/test/y_test.txt"                             
[17] "./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt" <br/>
[18] "./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt"   
[19] "./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt" <br/> 
[20] "./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt"   
[21] "./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt"  
[22] "./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt" <br/> 
[23] "./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt" <br/> 
[24] "./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt" <br/> 
[25] "./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt" <br/> 
[26] "./UCI HAR Dataset/train/subject_train.txt"                       
[27] "./UCI HAR Dataset/train/X_train.txt"                            
[28] "./UCI HAR Dataset/train/y_train.txt"  

To summarize, the data can be broken down into 4 groups; the test accelerometer and gyroscope measurements (incudluding total) (1), the train accelerator and gyroscope measurements (2), the test feature data (3) and the training feature data (4). 
Data sets 1 and 2 may be considered as the raw measurement data (other than the total measurements) and the data in these sets has been filtered and windowed as descrbed in the README.txt file which accompanies the data.
Data sets 3 and 4 are described as features, these features are various calulations that have been carried out on the previous data sets 1 and 2, and these "features" are described in the file features.txt


### Data Clean Up & Analysis

The script Run_Analysis.R combines, cleans, makes some caultuations and then writes the data to an R object called averages which follows the rules of a tidy data set.
To run this script the data must be downloaded using the link provided, the file must then be unzipped, and the working directory set so that it contains the unzipped folder.
  
#### Script

This section describes each aspect of the script


Create list of all files in HAR Dataset, remove files such as README.txt an information that isn't data to be tidied. 

```
filenames <- list.files("./UCI HAR Dataset", full.names = TRUE, pattern = ".txt", recursive = TRUE)  
filenames <- filenames[c(3, 5:(length(filenames)))]  
  
Data <- sapply(filenames, read.table, header = FALSE)
```

Define a function that takes a descriptor (such as body_acc_x) and combines the test and training data sets.

```R
combine_data <- function(descriptor) {
  
  #Determines which files match descriptor (no more than 2 files should match).    
  files <- grep(descriptor, filenames)
  
  # Subsets the relevent test data.frame from the list Data and adds on Type 0 (represents test).  
  test <- tbl_df(Data[[files[1]]])
  test <- mutate(test, Type = 0)

  # Subsets the relevent train data.frame from the list Data and adds on Type 1 (represents train).
  train <- tbl_df(Data[[files[2]]])
  train <- mutate(train, Type = 1)
  
  # Combines the two tbl_dfs.
  cd <- bind_rows(test,train) 
}
```

##### Merging Data

Merges the combined test and train tbl_dfs into a list.
Before merging the data, the "X" (X_Test, X_train) is combined, the features.txt file is read, comverted to a vector, and then assigned as the column names for X.

```R
features <- read.table(filenames[1],header = FALSE)
X <- combine_data("/X_")
colnames(X) <- c(paste0(as.character(1:561),pull(features, V2)), "Type")

merged_data <<- list(body_acc_x = combine_data("body_acc_x"),  
                     body_acc_y = combine_data("body_acc_y"),  
                     body_acc_z = combine_data("body_acc_z"),  
                     body_gyro_x = combine_data("body_gyro_x"),  
                     body_gyro_y = combine_data("body_gyro_y"),  
                     body_gyro_z = combine_data("body_gyro_z"),  
                     total_acc_x = combine_data("total_acc_x"),  
                     total_acc_y = combine_data("total_acc_y"),  
                     total_acc_z = combine_data("total_acc_z"),  
                     subject = combine_data("subject_"),  
                     X = X,  
                     activity = combine_data("/y_"))
```


Extracts the mean and standard deviations of relevant measurements using combined filename.
Assigns the strings std_dev and Mean to objects to be used as column names later.
Retrieves tibble from the merged data list, calculates the mean and standard deviation,
removes the measurement data, and then updates the column names.

```R
extract <- function(cfilename){
  
 
  std_dev <- paste(cfilename, "std_dev", sep = "_")
  Mean <- paste(cfilename, "mean", sep = "_")
  
 
  cd <- get(cfilename, merged_data)
  cd <- mutate(cd, std_dev = apply(select(cd, -Type), 1, sd), Mean = apply(select(cd, -Type), 1, mean))
  cd <- select(cd, std_dev, Mean)
  colnames(cd)[1] <- std_dev
  colnames(cd)[2] <- Mean
  cd
  
}
```

**data_2**:
The mean and standard deviations of data sets 1 and 2 are calculated over all measurements for each row and combined with the feature data

Variables in this data set are;  
subject  
type   
activity  
body_acc_x_mean  
body_acc_x_std_dev  
body_acc_y_mean  
body_acc_y_std_dev  
body_acc_z_mean  
body_acc_z_std_dev  
body_gyro_x_mean  
body_gyro_x_std_dev  
body_gyro_y_mean  
body_gyro_y_std_dev  
body_gyro_z_mean  
body_gyro_z_std_dev  
total_acc_x_mean  
total_acc_x_std_dev  
total_acc_y_mean  
total_acc_y_std_dev  
total_acc_z_mean  
total_acc_z_std_dev  
1tBodyAcc-mean()-X  
2tBodyAcc-mean()-Y  
3tBodyAcc-mean()-Z ... and so on. This continues for all of the features to a final variable named   <br/> 
561angle(Z,gravityMean)

These numbers were included as a number of features had the same name but different values


##### Final Data Set

**averages**:
The previous table is grouped by subject and activity and then a mean is applied to each group for every variable. Type is no longer valid and therefore not included in the table.

The variables from the previous data set are not renamed in this data set.



