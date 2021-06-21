install.packages("readxl")
library(readxl)
data<-read.csv("C:/Users/user/Documents/R/StatwithR/datascience/accidentInfoList.csv")
View(data)
names(data)
dim(data)
summary(data)

#lda
library(MASS)
train<-sample(1:35361,28288)
(ld <- lda(formula=serious_degree~.,data=data,subset=train))
lda(serious_degree ~ ., data = data, subset = train)

#apply(ld$means%*%ld$scaling,2,mean)

predict(ld,data[-train,])$class
predict(ld,data[-train,])$posterior

(tt<-table(data$serious_degree[-train], predict(ld,data[-train,])$class))
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)


#qda
library(MASS)
train<-sample(1:35361,28288)
(qd<-qda(formula=serious_degree~ ., data=data, subset = train))

predict(qd,data[-train,])$class
predict(qd,data[-train,])$posterior

(tt<-table(data$serious_degree[-train],predict(qd,data[-train,])$class))
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)

#knn
install.packages("klaR")
library(klaR)
train<-sample(1:35361,28288)
(knn<-sknn(serious_degree~.,data=data,subset=train))
predict(knn,data[-train,])$class
#��?��з?ǥ ?ۼ?
(tt<-table(data$serious_degree[-train], predict(knn,data[-train,])$class))
#��?з?��
sum(tt[row(tt) == col(tt)])/sum(tt)
#?��з?��
1-sum(tt[row(tt) == col(tt)])/sum(tt)


#knn2 - ?ȵ??ư? .. ??
install.packages("caret")
library(caret)
sample<-createDataPartition(data$serious_degree,p=0.80)
train<-data[sample$Resample1,]
nrow(train)
test<-data[-sample$Resample1,]
nrow(test)

set.seed(100)
ctrl<-trainControl(method="repeatedcv",number=10,repeats=5)
knnFit<-train(serious_degree~.,data=train,method='knn',
              trControl=ctrl,
              preProcess = c("center","scale"),tuneLength=20)
knnFit

#knn3
class(data$serious_degree)
data$serious_degree = as.factor(data$serious_degree)
class(data$serious_degree)

install.packages("class")
library(class)
round(prop.table(table(data$serious_degree))*100,2)
nrow(data)*0.8
n<-sample(1:nrow(data),28288)

train<-data[n,]
test<-data[-n,]

data_train_labels<-train[,1]
data_test_labels<-test[,1]
sqrt(nrow(train))
data_test_pred<-knn(train=train[,-1],
                    test=test[,-1],
                    cl=data_train_labels,k=3)

#knn4
library(caTools)
set.seed(123)
spl <- sample.split(data, 0.8)
data_train <- data[spl == TRUE, ]
data_test <- data[spl == FALSE, ]

knn1.pred<-knn(train=data_train[,-7072],test=data_test[,-7072],data_train[,7072],k=1)
table(knn1.pred,sms_test[,214])
mean(knn1.pred==sms_test[,214])

knn3.pred<-knn(train=sms_train[,-214],test=sms_test[,-214],sms_train[,214],k=3)
table(knn3.pred,sms_test[,214])
mean(knn3.pred==sms_test[,214])

knn5.pred<-knn(train=sms_train[,-214],test=sms_test[,-214],sms_train[,214],k=5)
table(knn5.pred,sms_test[,214])
mean(knn5.pred==sms_test[,214])

knn10.pred<-knn(train=sms_train[,-214],test=sms_test[,-214],sms_train[,214],k=10)
table(knn10.pred,sms_test[,214])
mean(knn10.pred==sms_test[,214])

#random
dataset<-read.csv("C:/Users/user/Documents/R/StatwithR/datascience/accidentInfoList.csv")

#################
install.packages("randomForest")
library("randomForest")
data.rf<-randomForest(y~.,data=dat)

data.rf

table(actual=y_test,predicted=predict(data.rf,dat_test,type="class"))

mean(predict(data.rf,dat_test,type="class")==y_test)


##factor로 변환

head(data)

class(data$month)
data$month = as.factor(data$month)
class(data$month)

class(data$time)
data$time = as.factor(data$time)
class(data$time)

data$damaged_driver.s_agegroup = as.factor(data$damaged_driver.s_agegroup)
class(data$damaged_driver.s_agegroup)

##cv
library(caret)
set.seed(123)
train.control<-trainControl(method="cv",number=10)
model <- train(serious_degree ~.,data=data,method="lm",
               trControl = train.control)
print(model)
