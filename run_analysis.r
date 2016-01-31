#Coursera Data Science Course
#Assignment(week 4): Getting and Cleaning Data Course Project
######################INSTRUCTIONS FOR ASSIGNMENT##############################
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#IMPORT LIBRARIES
require(data.table)
library(data.table)
require(reshape2)
library(reshape2)


#Retrieve test data & train data column labels 
datacolumnlabels <- data.frame( read.table(  "./UCI HAR Dataset/features.txt"  ) )

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
datacolumnlabelsStdMeans= grep("mean()|std()", datacolumnlabels[,2] )

# 3. Uses descriptive activity names to name the activities in the data set
## 3.a import activity label
dataactivitylabel = data.frame( read.table(  "./UCI HAR Dataset/activity_labels.txt"  ) )


##import test data and train data
testdata  <- read.table( "./UCI HAR Dataset/test/X_test.txt"  ) 
traindata <- read.table( "./UCI HAR Dataset/train/X_train.txt")

#rename columns
names(testdata)   <- datacolumnlabels[,2]
names(traindata)  <- datacolumnlabels[,2]


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 2.b Isolated columns from train and test data that stdev and means
testdatameanstd   = testdata[,datacolumnlabelsStdMeans]
traindatameanstd  = traindata[,datacolumnlabelsStdMeans]

#To save memory, we delete testdata, traindata
rm(testdata)
rm(traindata)

# 3. Uses descriptive activity names to name the activities in the data set
## 3.b merge subject data with activity code
testsubjectactivity  = cbind( read.table( "./UCI HAR Dataset/test/subject_test.txt")    , read.table( "./UCI HAR Dataset/test/Y_test.txt")     )
trainsubjectactivity = cbind( read.table( "./UCI HAR Dataset/train/subject_train.txt")  , read.table( "./UCI HAR Dataset/train/Y_train.txt")   )

# concatenate "Subject":"Activity":"Measurements(stdev, means)"
traintablefinal = cbind(  trainsubjectactivity ,  data.frame( traindatameanstd ) )
testtablefinal  = cbind(  testsubjectactivity  ,  data.frame( testdatameanstd  ) )

# append test and train data together
testplustraindata =   rbind( testtablefinal , traintablefinal   )  

# names the columns appropriately
colnames(testplustraindata)[1]<- "Subject"
colnames(testplustraindata)[2]<- "ActivityCode"


# add "Activity Label" the the appended data set above
testplustraindatafinal = merge(x=testplustraindata , y=dataactivitylabel , by.x = colnames(testplustraindata)[2] , by.y = colnames(dataactivitylabel)[1] )


# name the columns appropriately for column added from above merge
colnames(testplustraindatafinal)[ncol(testplustraindatafinal)]<- "ActivityLabel"


#Create Summary Table
melttestplustraindata   = melt( testplustraindatafinal , id = names(testplustraindatafinal)[c(2,1,82)] , measure.vars = names(testplustraindatafinal)[4:ncol(testplustraindatafinal)-1] )
tidytestplustraindata   = dcast(meltdata, Subject + ActivityCode + ActivityLabel  ~ variable , mean)

#Write table out for assignment submission 
write.table(tidytestplustraindata, "assignment_week4_submission.txt" , row.name=FALSE )
