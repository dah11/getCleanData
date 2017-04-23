library(dplyr)

dataFldr <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"

#read in features
features <- read.table(paste(dataFldr,"/features.txt",sep=""))
features <- mutate(features,V3 = paste0(V1,'_',V2))

#read in train data
subject_train <- read.table(paste(dataFldr,"/train/subject_train.txt",sep=""))
x_train <- read.table(paste(dataFldr,"/train/x_train.txt",sep=""))
y_train <- read.table(paste(dataFldr,"/train/y_train.txt",sep=""))

#read in test data
subject_test <- read.table(paste(dataFldr,"/test/subject_test.txt",sep=""))
x_test <- read.table(paste(dataFldr,"/test/x_test.txt",sep=""))
y_test <- read.table(paste(dataFldr,"/test/y_test.txt",sep=""))

#rename activity label column
y_train <- rename(y_train, activity = V1)
y_test <- rename(y_test, activity = V1)

#rename subject column
subject_train <- rename(subject_train, subject = V1)
subject_test <- rename(subject_test, subject = V1)

#rename data variables with feature id_feature name
colnames(x_train) <- features$V3
colnames(x_test) <- features$V3

#combine subject, activity,and train/test data
trainDf <- cbind(type = "train", subject_train, y_train, x_train)
testDf <- cbind(type = "test", subject_test, y_test, x_test)

#combine train and test data sets
df <- rbind(trainDf, testDf)

#get only measurements on the mean and standard deviation
df <- select(df,`type`, `subject`, `activity`, matches("mean\\(|std"))

#remove the begining feature id and _ we added to feature names
colnames(df) <- gsub(("^[0-9]*_"),"",colnames(df))

#rename our activity id to the activity
df$activity[df$activity == 1] <- 'WALKING'
df$activity[df$activity == 2] <- 'WALKING_UPSTAIRS'
df$activity[df$activity == 3] <- 'WALKING_DOWNSTAIRS'
df$activity[df$activity == 4] <- 'SITTING'
df$activity[df$activity == 5] <- 'STANDING'
df$activity[df$activity == 6] <- 'LAYING'

#average all variables by subject,activity
dfAvg <- select(df,-type)
dfAvg <- group_by(dfAvg,subject,activity)
dfAvg <- summarize_each(dfAvg,funs(mean))
dfAvg %>% setNames(paste0('mean_', names(.))) -> dfAvg
dfAvg <- rename(dfAvg, subject = mean_subject)
dfAvg <- rename(dfAvg, activity = mean_activity)

#export our data
write.table(dfAvg,'averageData.txt',row.names = FALSE)

