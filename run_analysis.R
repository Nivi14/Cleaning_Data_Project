library(plyr)


feature<-read.table("features.txt",header=FALSE)


train<-read.table("X_train.txt",header=FALSE)
y_train<-read.table("y_train.txt",header=FALSE)
subject_train<-read.table("subject_train.txt",header=FALSE)
##add row names
feature_subset<-(feature$V2)
names(train)<-feature_subset

#add values to activity file
y_trainvalue<-as.character(y_train$V1)
rows<-length(y_train$V1)
i=integer()
for (i in 1:rows) {
  if (y_trainvalue[i]=="1") {y_trainvalue[i]="WALKING"}
  if (y_trainvalue[i]=="2"){y_trainvalue[i]="WALKING_UPSTAIRS"}
  if (y_trainvalue[i]=="3"){y_trainvalue[i]="WALKING_DOWNSTAIRS"}
  if (y_trainvalue[i]=="4"){y_trainvalue[i]="SITTING"}
  if (y_trainvalue[i]=="5"){y_trainvalue[i]="STANDING"}
  if (y_trainvalue[i]=="6"){y_trainvalue[i]="LAYING"}
}
#add subject, activity
train_wactivity_lab<-cbind(y_trainvalue,train)
train_wactivity<-cbind(y_train$V1,train_wactivity_lab)
train_wsubject<-cbind(subject_train,train_wactivity)

colnames(train_wsubject)[1]<-"Subject"
colnames(train_wsubject)[2]<-"Activity"
colnames(train_wsubject)[3]<-"Activity_labels"




## get test data

test<-read.table("X_test.txt",header=FALSE)
y_test<-read.table("y_test.txt",header=FALSE)
subject_test<-read.table("subject_test.txt",header=FALSE)
names(test)<-feature_subset

#add values to activity file
y_testvalue<-as.character(y_test$V1)
rowsj<-length(y_test$V1)
j=integer()
for (j in 1:rowsj) {
  if (y_testvalue[j]=="1") {y_testvalue[j]="WALKING"}
  if (y_testvalue[j]=="2"){y_testvalue[j]="WALKING_UPSTAIRS"}
  if (y_testvalue[j]=="3"){y_testvalue[j]="WALKING_DOWNSTAIRS"}
  if (y_testvalue[j]=="4"){y_testvalue[j]="SITTING"}
  if (y_testvalue[j]=="5"){y_testvalue[j]="STANDING"}
  if (y_testvalue[j]=="6"){y_testvalue[j]="LAYING"}
}
#cbind
test_wactivity_lab<-cbind(y_testvalue,test)
test_wactivity<-cbind(y_test$V1,test_wactivity_lab)
test_wsubject<-cbind(subject_test,test_wactivity)

colnames(test_wsubject)[1]<-"Subject"
colnames(test_wsubject)[2]<-"Activity"
colnames(test_wsubject)[3]<-"Activity_labels"


#merge test and train
final_data<-rbind(train_wsubject,test_wsubject)


#get only std() and mean() columns
feature_mean<-grep("*-mean\\(\\)",feature$V2,ignore.case = TRUE,value=TRUE)
feature_std<-grep("*std()",feature$V2,ignore.case = TRUE,value=TRUE)
feature_mean_std<-c("Subject","Activity","Activity_labels",feature_mean,feature_std)
final_data_sel<-final_data[,feature_mean_std]


#Get AVG data per subject per activity
testtrain_avg<-ddply(final_data,.(Subject,Activity,Activity_labels),numcolwise(mean))
write.csv(testtrain_avg,"testtrain_avg.csv")
write.table(testtrain_avg, file = "TestTrain_AVG.txt",row.names=FALSE,sep="\t")

