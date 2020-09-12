# humanactivity
library(kernlab)
library(splines)
library(ISLR)
library(ggplot2)
library(Hmisc)
library(caret)
library(patry)
datatraining<-read.csv("F:/pml-training.csv")
datatesting<-read.csv("f:/pml-testing.csv")
data<-read.csv("f:/WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv")
Intrain<-createDataPartition(y=data1$classe,p=0.75,list = FALSE)
 training<-data1[Intrain,]
 testing<-data1[-Intrain,]
 dim(training)
 set.seed(1500)
 folds<-createFolds(y=data1$total_accel_arm,k=15,list = TRUE)

sapply(folds,length)
modfit<-train(classe~. , data = training,method="glm",verbose=FALSE)
predictions<-predict(modfit,newdata=training)
predictions
confusionMatrix(predictions,testing$classe)
featurePlot(x=training[,c("raw_timestamp_part_1","roll_belt")],y=datatraining$num_window,plot = "paris")
qplot(max_picth_belt,max_roll_belt,data = training)
featurePlot(x=training[c("raw_timestamp_part_1","raw_timestamp_part_2")],y=training$raw_timestamp_part_1,plot = "box plot")
cutdata<-cut2(training$num_window,g=4)
table(cutdata)
cutdata<-cut2(training$roll_belt,g=4)
table(cutdata)
p1<-qplot(cutdata,,data = training,fill=cutdata,geom = c("boxplot"))
p1
p2<-qplot(cutdata,pitch_belt,data = training,fill=cutdata,geom = c("boxplot"))
p2
grad.arrange(p1,p2,ncol=2)
nsv<-nearZeroVar(training,saveMetrics = TRUE)
nsv
bsbasic<-bs(training$total_accel_belt)
bsbasic
c<-qplot(raw_timestamp_part_1,raw_timestamp_part_2,data = testing,colour=new_window,main = "newdatapredictions")
c
plot(data1$classe,uniform=TRUE,main="classification Tree")
text(data1$classe,use.n=TRUE,all=TRUE,cex=.8)
data2<-subset(data1,select=-c(num_window))
inbuild<-createDataPartition(y=data2$new_window,p=0.7,list = TRUE)
inbuild<-createDataPartition(y=data2$new_window,p=0.7,list = FALSE)
validation<-data2[-inbuild,]
builddata<-data2[inbuild,]
ts1train<-window(x,start = 1,end = 10)
ts1test<-window(x,start = 10,end = (15-0.02))
ts1test
