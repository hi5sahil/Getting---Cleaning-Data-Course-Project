#Loading all the datasets
------
setwd('C:/Users/Sahil/Desktop/Coursera/Exploratory Data Analysis Course Project 2/UCI HAR Dataset')

test <- read.table("X_test.txt")
train <- read.table("X_train.txt")
testSubject <- read.table("subject_test.txt")
trainSubject <- read.table("subject_train.txt")
testLabels <- read.table("y_test.txt")
trainLabels <- read.table("y_train.txt")
------
*********************************************************************************************************************
#Getting subject IDs for all the observations in test & training dataset
------
test$subject<-testSubject[,1]

test$labels<-testLabels[,1]

train$subject<-trainSubject[,1]
train$labels<-trainLabels[,1]

features <- read.table("features.txt")
------

#Merging the test & training data sets
------
merged_data <- rbind(train, test, deparse.level = 1)
------
*********************************************************************************************************************
#Replacing the general variable names with variable descriptions from features.txt
names(merged_data)[1:dim(features)[1]] <- as.vector(features[,2])
names<-names(merged_data)
#Extracting only the measurements on the mean and standard deviation for each measurement
------
variable_means<-grep("mean", names)
variable_std<-grep("std", names)
all <- c(names[variable_means], names[variable_std], "labels", "subject")
subset <- merged_data[,all]
------
#Getting the Activity Labels against the Activity Codes
------
activity_labels <- read.table("activity_labels.txt")
names(activity_labels)
complete_data <- merge(x=subset, y=activity_labels, by.x = "labels", by.y = "V1", all.x = TRUE)
L <- length(names(complete_data))
names(complete_data)[L] <- "Activity_Labels"
------
*********************************************************************************************************************
#Loading the dplyr library
#From the data set in step 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject.
------
library(dplyr)
grouped_data <- group_by(complete_data, Activity_Labels, labels, subject)

limit<-dim(grouped_data)[2]-3

backup <- grouped_data
names(backup) <- 1:dim(grouped_data)[2]
names<-names(backup)
names2<-names(grouped_data)[2:80]
for(i in 1:dim(grouped_data)[2]){
	names[i] <- paste ("V", i, sep = "", collapse = NULL)}
	
names(backup)<-names
#i<-2
#while(i <= limit+1){
#names[i] <- paste ("avg ", i, " = mean (", i, ") ,", sep = "", collapse = NULL)}
#i<-i+1}

	
i<-2	
while(i <= length(names)-2){
	names[i] <- paste ("Avg", names[i], " = mean(", names[i], ")", sep = "", collapse = NULL)
	i<-i+1}

gather <- names[2]
i<-3
while(i <= length(names)-2){
	gather <- paste (gather, names[i], sep = ",", collapse = NULL)
	i<-i+1}
	

gather

write.table(gather, file = "Gather.txt", row.name=FALSE)

#final <- summarise(backup, PASTE the text from Gather.txt here as shown below)  

final <- summarise(backup, AvgV2 = mean(V2),AvgV3 = mean(V3),AvgV4 = mean(V4),AvgV5 = mean(V5),AvgV6 = mean(V6),AvgV7 = mean(V7),AvgV8 = mean(V8),AvgV9 = mean(V9),AvgV10 = mean(V10),AvgV11 = mean(V11),AvgV12 = mean(V12),AvgV13 = mean(V13),AvgV14 = mean(V14),AvgV15 = mean(V15),AvgV16 = mean(V16),AvgV17 = mean(V17),AvgV18 = mean(V18),AvgV19 = mean(V19),AvgV20 = mean(V20),AvgV21 = mean(V21),AvgV22 = mean(V22),AvgV23 = mean(V23),AvgV24 = mean(V24),AvgV25 = mean(V25),AvgV26 = mean(V26),AvgV27 = mean(V27),AvgV28 = mean(V28),AvgV29 = mean(V29),AvgV30 = mean(V30),AvgV31 = mean(V31),AvgV32 = mean(V32),AvgV33 = mean(V33),AvgV34 = mean(V34),AvgV35 = mean(V35),AvgV36 = mean(V36),AvgV37 = mean(V37),AvgV38 = mean(V38),AvgV39 = mean(V39),AvgV40 = mean(V40),AvgV41 = mean(V41),AvgV42 = mean(V42),AvgV43 = mean(V43),AvgV44 = mean(V44),AvgV45 = mean(V45),AvgV46 = mean(V46),AvgV47 = mean(V47),AvgV48 = mean(V48),AvgV49 = mean(V49),AvgV50 = mean(V50),AvgV51 = mean(V51),AvgV52 = mean(V52),AvgV53 = mean(V53),AvgV54 = mean(V54),AvgV55 = mean(V55),AvgV56 = mean(V56),AvgV57 = mean(V57),AvgV58 = mean(V58),AvgV59 = mean(V59),AvgV60 = mean(V60),AvgV61 = mean(V61),AvgV62 = mean(V62),AvgV63 = mean(V63),AvgV64 = mean(V64),AvgV65 = mean(V65),AvgV66 = mean(V66),AvgV67 = mean(V67),AvgV68 = mean(V68),AvgV69 = mean(V69),AvgV70 = mean(V70),AvgV71 = mean(V71),AvgV72 = mean(V72),AvgV73 = mean(V73),AvgV74 = mean(V74),AvgV75 = mean(V75),AvgV76 = mean(V76),AvgV77 = mean(V77),AvgV78 = mean(V78),AvgV79 = mean(V79),AvgV80 = mean(V80))
	

names3<-names(final)
end<-dim(grouped_data)[2]
for(i in 4:end){
	j<-i-3
	names3[i] <- names2[j]}
	
names(final)<-names3

for(i in 4:dim(grouped_data)[2]){
	names3[i] <- paste ("Avg ", names3[i], sep = "", collapse = NULL)}
	
names(final) <- names3


write.table(final, file = "Tidy_Data.txt", row.name=FALSE)

Tidy_Data <- read.table("Tidy_Data.txt")

Tidy_Data
------