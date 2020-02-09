# Loads required packages (these packages must be previously downloaded).
library(dplyr,tidyr)

# Create list of all files in HAR Dataset.
filenames <- list.files("./UCI HAR Dataset", full.names = TRUE, pattern = ".txt", recursive = TRUE)
# Remove meta data.
filenames <- filenames[c(3, 5:(length(filenames)))]
# Read remaining files to create a list of tables.
Data <- sapply(filenames, read.table, header = FALSE)

# This function takes a descriptor (such as body_acc_x) and combines the test and training data sets. 
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


# Merges the combined test and train tbl_dfs into a list.
# Before merging the data, the "X" (X_Test, X_train) is combined, the features.txt file 
# is read, converted to a vector, and then assigned as the column names for X.

features <- read.table(filenames[1],header = FALSE)
X <- combine_data("/X_")
colnames(X) <- c(paste0(as.character(1:561),pull(features, V2)), "Type")

merged_data <<- list(body_acc_x = combine_data("body_acc_x"), body_acc_y = combine_data("body_acc_y"), body_acc_z = combine_data("body_acc_z"),
                    body_gyro_x = combine_data("body_gyro_x"), body_gyro_y = combine_data("body_gyro_y"), body_gyro_z = combine_data("body_gyro_z"),
                    total_acc_x = combine_data("total_acc_x"), total_acc_y = combine_data("total_acc_y"), total_acc_z = combine_data("total_acc_z"),
                    subject = combine_data("subject_"), X = X, activity = combine_data("/y_"))



# Extracts the mean and standard deviations of relevant measurements using combined filename.
extract <- function(cfilename){
  
  # Assigns the strings std_dev and Mean to objects to be used as column names later.
  std_dev <- paste(cfilename, "std_dev", sep = "_")
  Mean <- paste(cfilename, "mean", sep = "_")
  
  # Retrieves tibble from the merged data list, calculates the mean and standard deviation,
  # removes the measurement data, and then updates the column names.
  cd <- get(cfilename, merged_data)
  cd <- mutate(cd, std_dev = apply(select(cd, -Type), 1, sd), Mean = apply(select(cd, -Type), 1, mean))
  cd <- select(cd, std_dev, Mean)
  colnames(cd)[1] <- std_dev
  colnames(cd)[2] <- Mean
  cd
  
}

# Combines data into a single tibble with correct column names.
data_2 <- bind_cols(get("subject", merged_data), select(get("activity", merged_data), V1),
                    extract("body_acc_x"), extract("body_acc_y"), extract("body_acc_z"),
                    extract("body_gyro_x"), extract("body_gyro_y"), extract("body_gyro_z"),
                    extract("total_acc_x"), extract("total_acc_y"), extract("total_acc_z"),
                    select(get("X", merged_data),-Type))
colnames(data_2)[1] <- "subject"
colnames(data_2)[3] <- "activity"

# Group by subject and activity and then calculate the mean of each variable for each subject activity group
activity_subject_group <- group_by(data_2, activity, subject)
averages <- summarize_at(activity_subject_group, vars(2:580), ~ mean(.))



# Code for writing the data to csv

write.csv(averages, "./averages.csv", row.names = TRUE)


# Code for updating the readme.txt file that accompanies the data
allfiles <- list.files("./UCI HAR Dataset", full.names = TRUE, pattern = ".txt", recursive = TRUE)
readme <- allfiles[4]
lines <- sprintf("==================================================================

==================================================================

Further Analysis
================

This repository includes new files which tidy the data, make some calculations, and explain how this is done.
This further analysis was completed by James Dymoke-Bradshaw in January 2020 as part of the Getting and Cleaning Data Course 
provided by Johns Hopkins University via Coursera.

New Files
=========
        
- 'Run_Analysis.R'

- 'README.md'
                 
- 'averages.csv'

To generate 'averages.csv', 'Run_Analysis.R' must be run using R.
Please see 'README.md' for an explanation of the analysis performed by 'Run_analysis.R'           
                 
averages.csv
=============
                 
'averages.csv' summarises the average of each variable for each subject and activity, the variables include the mean and standard deviation of the raw data sets, plus the feature data.
The columns are as follows;

subject
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
3tBodyAcc-mean()-Z . . . and so on. This continues for all of the features to a final variable named 
561angle(Z,gravityMean)
                 
")


write(lines, readme, append = TRUE)