install.packages("readxl")
library(readxl)
data<-read.csv("C:/Users/user/Documents/R/StatwithR/datascience/accidentInfoList.csv")
View(data)
names(data)
dim(data)
summary(data)

#종속변수 코딩
data$serious_degree[data$serious_degree == '1']<-1
data$serious_degree[data$serious_degree == '0']<-0
data$serious_degree<-as.numeric(data$serious_degree)

#학습데이터와 검정데이터 생성
sample_data<-sample(1:nrow(data),nrow(data)*0.8)

data_train<-data[sample_data,]
data_test<-data[-sample_data,]

#로지스틱 회귀분석 실행
data<-glm(serious_degree~.,family="binomial",data=data_train)
summary(data)

install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(data$y,fitted(data))
#로지스틱 회귀모형 모델 예측치 생성
pred<-predict(data,newdata=data_test,type="response")
pred
result_pred<-ifelse(pred>=0.5,1,0)
#분류정확도 계산
table(result_pred,data_test$serious_degree)
(tt<-table(result_pred,data_test$serious_degree))
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)
