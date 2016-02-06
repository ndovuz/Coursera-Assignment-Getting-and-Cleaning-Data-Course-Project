#Coursera Assignment: Getting and Cleaning Data Course Project: CodeBook

##Introduction

The purpose of this code is to process data from experiments that are described in the following [location](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#).

In summary, the experiments pertain associating 3-axial linear acceleration and 3-axial angular velocity from a smartphone with the following six activities:

* WALKING
* WALKING UPSTAIRS
* WALKING DOWNSTAIRS 
* SITTING
* STANDING
* LAYING

##Data

The data was analyzed by this code was sourced from the followng [location](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/) on January 29th, 2016.

The various data sets within the data repository are described [here](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.names).



##Data Sets

The raw data sets from this study were processed as follows in this code:


| Raw Data Set Name        | Data Set Name in Code | Description  |
| ------------- |:-------------|:-----|
|features_info.txt| datacolumnlabels  | Shows information about the variables used on the feature vector.|
|activity_labels.txt| dataactivitylabel |Links the class labels with their activity name. |
|train/X_train.txt | traindata |Training set.|
|test/X_test.txt | testdata |Test set.|


The following raw datasets were concatenated on import to the resulting table.

| Resulting Table        | Left Raw Dataset  | Right Raw Dataset |
| ------------- |:-------------|:-----|
|testsubjectactivity | test/subject\_test.txt| test/y\_test.txt |
|trainsubjectactivity | train/subject\_train.txt| train/y\_train.txt |

The descriptions of the tables above can be found here:

| Raw Data Set Name   | Description  |
| ------------- |:------|
| test/subject\_test.txt | Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.  |
| train/subject\_train.txt | Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.  |
| test/y\_test.txt | Training labels  |
| train/y\_train.txt  |Training labels  |

## Updated Variable Names

The following single column data sets were given the following updated variable names(labels).

| Raw Data Set Name   | Updated Variable Name (Column Label)  |
| ------------- |:------|
| test/subject\_test.txt | Subject  |
| train/subject\_train.txt | Subject  |
| test/y\_test.txt | ActivityCode  |
| train/y\_train.txt  |ActivityCode  |
|activity_labels.txt | ActivityLabel |


##Output

The output is saved in the working directory as *assignment_week4_submission.txt*.

The columns of the outut are as follows:


| Column Name   | Data Type  |
| ------------- |:------|
| Subject | integer |
| ActivityCode | integer |
| ActivityLabel | factor |
| tBodyAcc.mean...X | numeric |
| tBodyAcc.mean...Y | numeric |
| tBodyAcc.mean...Z | numeric |
| tBodyAcc.std...X | numeric |
| tBodyAcc.std...Y | numeric |
| tBodyAcc.std...Z | numeric |
| tGravityAcc.mean...X | numeric |
| tGravityAcc.mean...Y | numeric |
| tGravityAcc.mean...Z | numeric |
| tGravityAcc.std...X | numeric |
| tGravityAcc.std...Y | numeric |
| tGravityAcc.std...Z | numeric |
| tBodyAccJerk.mean...X | numeric |
| tBodyAccJerk.mean...Y | numeric |
| tBodyAccJerk.mean...Z | numeric |
| tBodyAccJerk.std...X | numeric |
| tBodyAccJerk.std...Y | numeric |
| tBodyAccJerk.std...Z | numeric |
| tBodyGyro.mean...X | numeric |
| tBodyGyro.mean...Y | numeric |
| tBodyGyro.mean...Z | numeric |
| tBodyGyro.std...X | numeric |
| tBodyGyro.std...Y | numeric |
| tBodyGyro.std...Z | numeric |
| tBodyGyroJerk.mean...X | numeric |
| tBodyGyroJerk.mean...Y | numeric |
| tBodyGyroJerk.mean...Z | numeric |
| tBodyGyroJerk.std...X | numeric |
| tBodyGyroJerk.std...Y | numeric |
| tBodyGyroJerk.std...Z | numeric |
| tBodyAccMag.mean.. | numeric |
| tBodyAccMag.std.. | numeric |
| tGravityAccMag.mean.. | numeric |
| tGravityAccMag.std.. | numeric |
| tBodyAccJerkMag.mean.. | numeric |
| tBodyAccJerkMag.std.. | numeric |
| tBodyGyroMag.mean.. | numeric |
| tBodyGyroMag.std.. | numeric |
| tBodyGyroJerkMag.mean.. | numeric |
| tBodyGyroJerkMag.std.. | numeric |
| fBodyAcc.mean...X | numeric |
| fBodyAcc.mean...Y | numeric |
| fBodyAcc.mean...Z | numeric |
| fBodyAcc.std...X | numeric |
| fBodyAcc.std...Y | numeric |
| fBodyAcc.std...Z | numeric |
| fBodyAcc.meanFreq...X | numeric |
| fBodyAcc.meanFreq...Y | numeric |
| fBodyAcc.meanFreq...Z | numeric |
| fBodyAccJerk.mean...X | numeric |
| fBodyAccJerk.mean...Y | numeric |
| fBodyAccJerk.mean...Z | numeric |
| fBodyAccJerk.std...X | numeric |
| fBodyAccJerk.std...Y | numeric |
| fBodyAccJerk.std...Z | numeric |
| fBodyAccJerk.meanFreq...X | numeric |
| fBodyAccJerk.meanFreq...Y | numeric |
| fBodyAccJerk.meanFreq...Z | numeric |
| fBodyGyro.mean...X | numeric |
| fBodyGyro.mean...Y | numeric |
| fBodyGyro.mean...Z | numeric |
| fBodyGyro.std...X | numeric |
| fBodyGyro.std...Y | numeric |
| fBodyGyro.std...Z | numeric |
| fBodyGyro.meanFreq...X | numeric |
| fBodyGyro.meanFreq...Y | numeric |
| fBodyGyro.meanFreq...Z | numeric |
| fBodyAccMag.mean.. | numeric |
| fBodyAccMag.std.. | numeric |
| fBodyAccMag.meanFreq.. | numeric |
| fBodyBodyAccJerkMag.mean.. | numeric |
| fBodyBodyAccJerkMag.std.. | numeric |
| fBodyBodyAccJerkMag.meanFreq.. | numeric |
| fBodyBodyGyroMag.mean.. | numeric |
| fBodyBodyGyroMag.std.. | numeric |
| fBodyBodyGyroMag.meanFreq.. | numeric |
| fBodyBodyGyroJerkMag.mean.. | numeric |
| fBodyBodyGyroJerkMag.std.. | numeric |
| fBodyBodyGyroJerkMag.meanFreq.. | numeric |

