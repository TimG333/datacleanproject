#Load relevant training data sets
TrainActivityLabels <- read.table("./UCI HAR Dataset/train/Y_train.txt")
TrainDataSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
TrainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#Load packages used in this
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("gdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

#Flag if the number of rows aren't lining up in training data sets
if(!(nrow(TrainActivityLabels)==nrow(TrainDataSet) & nrow(TrainSubjects))){
  print("Note the number of rows are different and input might be off")
}

#Rename activities by their labels after giving the column a new name
names(TrainActivityLabels) <- c("Activity")
TrainActivityLabels$Activity <- gsub("1","Walking",TrainActivityLabels$Activity,fixed=TRUE)
TrainActivityLabels$Activity <- gsub("2","Walking_Upstairs",TrainActivityLabels$Activity,fixed=TRUE)
TrainActivityLabels$Activity <- gsub("3","Walking_Downstairs",TrainActivityLabels$Activity,fixed=TRUE)
TrainActivityLabels$Activity <- gsub("4","Sitting",TrainActivityLabels$Activity,fixed=TRUE)
TrainActivityLabels$Activity <- gsub("5","Standing",TrainActivityLabels$Activity,fixed=TRUE)
TrainActivityLabels$Activity <- gsub("6","Laying",TrainActivityLabels$Activity,fixed=TRUE)

#Rename subject variable column name and create variable to tag as train subject
names(TrainSubjects) <- c("Subject")
TrainSubjects$SubjectType = rep("Training",nrow(TrainSubjects))

#Read features.txt to name data columns, including cleaning up inappropriate characters
DataColumnNames <- read.table("./UCI HAR Dataset/features.txt")
DataColumnNames$V2 <- gsub("()","",DataColumnNames$V2,fixed=TRUE)
DataColumnNames$V2 <- gsub("(",".",DataColumnNames$V2,fixed=TRUE)
DataColumnNames$V2 <- gsub(")",".",DataColumnNames$V2,fixed=TRUE)
DataColumnNames$V2 <- gsub("-",".",DataColumnNames$V2,fixed=TRUE)
DataColumnNames$V2 <- gsub(",",".",DataColumnNames$V2,fixed=TRUE)
names(TrainDataSet) <- DataColumnNames$V2

#Fuse the training set pieces together in subject, activity, data order
FusedTrain <- cbind(TrainSubjects, TrainActivityLabels, TrainDataSet)

#Load relevant test data sets
testActivityLabels <- read.table("./UCI HAR Dataset/test/Y_test.txt")
testDataSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Flag if the number of rows aren't lining up in testing data sets
if(!(nrow(testActivityLabels)==nrow(testDataSet) & nrow(testSubjects))){
  print("Note the number of rows are different and input might be off")
}

#Rename activities by their labels after giving the column a new name
names(testActivityLabels) <- c("Activity")
testActivityLabels$Activity <- gsub("1","Walking",testActivityLabels$Activity,fixed=TRUE)
testActivityLabels$Activity <- gsub("2","Walking_Upstairs",testActivityLabels$Activity,fixed=TRUE)
testActivityLabels$Activity <- gsub("3","Walking_Downstairs",testActivityLabels$Activity,fixed=TRUE)
testActivityLabels$Activity <- gsub("4","Sitting",testActivityLabels$Activity,fixed=TRUE)
testActivityLabels$Activity <- gsub("5","Standing",testActivityLabels$Activity,fixed=TRUE)
testActivityLabels$Activity <- gsub("6","Laying",testActivityLabels$Activity,fixed=TRUE)

#Rename subject variable column name and create variable to tag as test subject
names(testSubjects) <- c("Subject")
testSubjects$SubjectType = rep("Testing",nrow(testSubjects))

#Use already read in features.txt to name data columns
names(testDataSet) <- DataColumnNames$V2

#Fuse the testing set pieces together in subject, activity, data order
Fusedtest <- cbind(testSubjects, testActivityLabels, testDataSet)

#Put test and training together
AllData <- rbind(FusedTrain,Fusedtest)

#Produce a data set with just means and standard deviations
IndexArray <- grepl("Mean",names(AllData)) | grepl("mean",names(AllData)) | grepl("std",names(AllData)) | grepl("Subject",names(AllData)) | grepl("Activity",names(AllData))
SummaryData <- AllData[,IndexArray]

#Generate the clean data from the summary data
IntermediateSet <- melt(SummaryData,id.vars=c("Subject", "Activity"), measure.vars=4:89)
TidyData <- dcast(IntermediateSet, Subject + Activity ~ variable, fun.aggregate=mean)

#Produce a tab deliminated file with the tidy data
write.table(TidyData, file="./TidyTab.txt", sep="\t")