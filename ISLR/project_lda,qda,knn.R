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

apply(ld$means%*%ld$scaling,2,mean)

predict(ld,data[-train,])$class
predict(ld,data[-train,])$posterior

(tt<-table(data$serious_degree[-train], predict(ld,data[-train,])$class))
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)
plot(ld,dimen = 2)


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
install.packages("class")
library(class)
round(prop.table(table(data$serious_degree))*100,2)
nrow(data)*0.8
n<-sample(1:nrow(data),28288)

data_train<-data[n,]
data_test<-data[-n,]

data_train_labels<-data_train[,1]
data_test_labels<-data_test[,1]
sqrt(nrow(data_train))
data_test_pred<-knn(train=data_train[,-1],
                    test=data_test[,-1],
                    cl=data_train_labels,k=169)
