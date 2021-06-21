dataset<-read.csv("C:/Users/user/Documents/R/StatwithR/datascience/accidentInfoList.csv")

#serious_degree factor로 전환
class(dataset$serious_degree)
dataset$serious_degree = as.factor(dataset$serious_degree)
class(dataset$serious_degree)

sample_num = sample(1:nrow(dataset), size = round(0.2 * nrow(dataset)))

str(dataset)
summary(dataset)
test_users  = dataset[  sample_num, ]
train_users = dataset[ -sample_num, ]
x = train_users[,-13]
y = train_users$serious_degree
x_test = test_users[,-13]
y_test = test_users$serious_degree
dat = data.frame(x=x, y=as.factor(y))
dat_test = data.frame(x=x_test, y=as.factor(y_test))
merge(x=x,y=as.factor(y))



######random
#################
install.packages("randomForest")
library("randomForest")
data.rf<-randomForest(y~.,data=dat)

data.rf

table(actual=y_test,predicted=predict(data.rf,dat_test,type="class"))

mean(predict(data.rf,dat_test,type="class")==y_test)
#############factor로 벡터전환
#############문자열 형식 전체 변환 
type<-rep(factor="Type",c('vehicle to people',
         'only vehicle',
         'vehicle to vehicle'), 5)
type
class(type)
str(type)
y_type<-factor(type)
y_type
class(y_type)
str(y_type)


