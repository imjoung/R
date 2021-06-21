install.packages("readxl")
library(readxl)
data<-read.csv("C:/Users/user/Documents/R/StatwithR/datascience/accidentInfoList.csv")
View(data)
names(data)
dim(data)
summary(data)



###############################
##cv
library(caret)
set.seed(123)
train.control<-trainControl(method="cv",number=10)
model <- train(serious_degree ~.,data=data[],method="glm",
               trControl = train.control)
print(model)



###############################
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

###############################
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


###############################
#knn
install.packages("klaR")
library(klaR)
train<-sample(1:35361,28288)
(knn<-sknn(serious_degree~.,data=data,subset=train))
predict(knn,data[-train,])$class

(tt<-table(data$serious_degree[-train], predict(knn,data[-train,])$class))
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)


###############################
head(data)
# factor로 변환
class(data$month)
data$month = as.factor(data$month)
class(data$month)

class(data$time)
data$time = as.factor(data$time)
class(data$time)

data$damaged_driver.s_agegroup = as.factor(data$damaged_driver.s_agegroup)
class(data$damaged_driver.s_agegroup)

sample_num = sample(1:nrow(data), size = round(0.2 * nrow(data)))

str(data)
summary(data)
test_users  = data[  sample_num, ]
train_users = data[ -sample_num, ]
x = train_users[,-13]
y = train_users$serious_degree
x_test = test_users[,-13]
y_test = test_users$serious_degree
dat= data.frame(x=x, y=as.factor(y))
dat_test = data.frame(x=x_test, y=as.factor(y_test))
merge(x=x,y=as.factor(y))
install.packages("randomForest")
library("randomForest")
data.rf<-randomForest(y~.,data=dat)

data.rf

(tt<-table(actual=y_test,predicted=predict(data.rf,dat_test,type="class")))
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)
