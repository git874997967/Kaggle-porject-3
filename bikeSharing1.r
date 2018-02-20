library(rpart)
library(rpart.plot)
library(ranger)
library(randomForest)
library(car)
library(lubridate)
library(dplyr)
train = read.csv("train.csv")
test = read.csv("test.csv")
train$label = 'train'
test$label = 'test'
# understand the data set
# datetime - hourly date + timestamp
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals

# submit format
# datetime count
str(train)
str(test)
## refine the test
test$registered = 0
test$count = 0
test$casual = 0

###no   NA values
data = bind_rows(train, test)
str(data)
## 添加派生变量
data$hour1 = as.factor(hour(data$datetime))
# data$hour2 = substr(data$datetime, 12, 13)
data$day = weekdays(as.Date(data$datetime))
data$weatherCond = data$temp * data$humidity
# in chinese envoronment use code below
# data$day[data$day == "星期六"] = "Saturday"
# data$day[data$day == '星期日'] = "Sunday"
# data$day[data$day == "星期一"] = "Monday"
# data$day[data$day == '星期二'] = "Tuesday"
# data$day[data$day == "星期三"] = "Wednesday"
# data$day[data$day == '星期四'] = "Thursday"
# data$day[data$day == "星期五"] = "Friday"

data$year = year(data$datetime)
## 产生皮尔逊相关参数
cor_data = data.frame(train$count
                      
                      
                      train$temp,
                      train$humidity,
                      train$atemp,
                      train$windspeed)
cor(cor_data)
###分为四个季度????
data$month = month(data$datetime)
par(mfrow = c(2, 1))
boxplot(casual ~ hour1, data = data)
f = rpart(casual ~ hour1, data = data)
rpart.plot(f)
train$hour1 = as.factor(hour(train$datetime))
boxplot(registered ~ hour1, data = train)
d = rpart(registered ~ hour1, data = train)
rpart.plot(d)
boxplot(temp ~ hour1, data = train)
d = rpart(temp ~ hour1, data = train)
rpart.plot(d)

#### 将连续型变量变成离散型变量  借助决策树模型
data$temp_reg = 0
data$temp_reg[data$hour1 %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 23, 21, 22)] =
  1
data$temp_reg[data$hour1 %in% c(10:20)] = 2



###
data$dp_reg[data$hour1 %in% c(0, 1, 2, 3, 4, 5, 6, 23)] = 1
data$dp_reg[data$hour1 %in% c(10, 11, 21, 22)] = 2
data$dp_reg[data$hour1 %in% c(7, 9, 12, 13, 14, 15, 20)] = 3
data$dp_reg[data$hour1 %in% c(16, 19)] = 4
data$dp_reg[data$hour1 %in% c(8, 17, 18)] = 5

data$dp_cas = 0


data$dp_cas[data$hour1 %in% c(0, 1, 2, 3, 4, 5, 6, 7, 23)] = 1
data$dp_cas[data$hour1 %in% c(8, 9, 20, 21, 22)] = 2
data$dp_cas[data$hour1 %in% c(10, 11, 18, 19, 12)] = 3
data$dp_cas[data$hour1 %in% c(12, 13, 14, 15, 16, 17)] = 4


### 增加日期类型
data$daytype = ""
data$daytype[data$holiday == 1] = "holiday"
data$daytype[data$workingday == 1] = "workingday"
data$daytype[data$holiday == 0 & data$workingday == 0] = "weekend"
data$weekend = 0
data$weekend[data$daytype == "weekend"] = 1
## add test for temp*humidity

data$tlevel = 2
data$tlevel[data$hour1 %in% c(11:19)] = 1

###因子化 变量
data$hour1 = as.factor(data$hour1)
data$day = as.factor(data$day)
data$daytype = as.factor(data$daytype)

 

set.seed(1234)
 
train = data[data$label == 'train', ]
test = data[data$label == 'test', ]


str(train)
train$logreg = log(train$registered + 1)
train$logcas = log(train$casual + 1)
test$logreg=0
test$logcas=0
fit1 = ranger(
  formula = logreg ~ hour1 + workingday + day + weekend + holiday  + temp_reg + humidity +
    atemp + windspeed + season + weather + dp_reg +  year + tlevel + weatherCond,
  data = train,
  num.trees = 3000,
  num.threads = 8
)
fit2 = ranger(
  formula = logcas ~ hour1 + workingday + day + weekend  + holiday  + temp_reg + humidity +
    atemp + windspeed + season + weather + dp_cas +  year + tlevel + weatherCond,
  data = train,
  num.trees = 3000,
  num.threads = 8
)
p1 = predict(fit1, test)
test$logreg = p1$predictions
p2 = predict(fit2, test)
test$logcas = p2$predictions
head(test)
test$registered = exp(test$logreg)
test$casual = exp(test$logcas)
test$count = test$registered + test$casual
submit_final = data.frame(datetime = test$datetime, count = as.integer(test$count))
write.csv(submit_final, "sub4.csv")
############try with lasso
library(glmnet)
formula1=as.formula(logreg ~ hour1 + workingday + day + weekend + holiday  + temp_reg + humidity +
                      atemp + windspeed + season + weather + dp_reg +  year + tlevel + weatherCond)
x1=model.matrix(formula1,train)
y1=log(train$registered + 1)

formula2=as.formula(logcas ~ hour1 + workingday + day + weekend  + holiday  + temp_reg + humidity +
                      atemp + windspeed + season + weather + dp_cas +  year + tlevel + weatherCond)
x2=model.matrix(formula2,train)
y2=log(train$casual + 1)
set.seed(1234)
lassoreg=cv.glmnet(x1,y1,alpha=1)
lassocas=cv.glmnet(x2,y2,alpha=1)
lasso.predreg=predict(lassoreg,newx=model.matrix(formula1,test),s='lambda.min')
lasso.predcas=predict(lassocas,newx=model.matrix(formula2,test),s='lambda.min')
test$registered=exp(lasso.predreg)
test$casual=exp(lasso.predcas)
lasso_final = data.frame("datetime" = test$datetime, "count" = round(test$registered+test$casual))
write.csv(lasso_final, "lasso.csv")



