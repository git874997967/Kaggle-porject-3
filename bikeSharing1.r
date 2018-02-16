library(rpart)
library(rpart.plot)
library(mice)
library(VIM)
library(ranger)
library(randomForest)
library(car)
library(lubridate)
library(dplyr)
train = read.csv("train.csv")
test = read.csv("test.csv")
train$label='train'
test$label='test'
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
data$hour2 = substr(data$datetime, 12, 13)
data$day = weekdays(as.Date(data$datetime))
data$day[data$day == "星期六"] = "Saturday"
data$day[data$day == '星期日'] = "Sunday"
data$day[data$day == "星期一"] = "Monday"
data$day[data$day == '星期二'] = "Tuesday"
data$day[data$day == "星期三"] = "Wednesday"
data$day[data$day == '星期四'] = "Thursday"
data$day[data$day == "星期五"] = "Friday"

data$year = year(data$datetime)
## 产生皮尔逊相关参数
cor_data = data.frame(train$count,
                      train$temp,
                      train$humidity,
                      train$atemp,
                      train$windspeed)
cor(cor_data)
###分为四个季度????
data$month = month(data$datetime)
par(mfrow = c(2, 1))
boxplot(casual ~ hour1, data = train)
f = rpart(casual ~ hour1, data = train)
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
data$temp_reg[data$hour1 == 0 |
                data$hour1 == 1 |
                data$hour1 == 2 |
                data$hour1 == 3 |
                data$hour1 == 4 |
                data$hour1 == 5 |
                data$hour1 == 6 |
                data$hour1 == 7 |
                data$hour1 == 8 |
                data$hour1 == 9 |
                data$hour1 == 21 | data$hour1 == 22 |
                data$hour1 == 23] = 1
data$temp_reg[data$hour1 == 10 |
                data$hour1 == 11 |
                data$hour1 == 12 |
                data$hour1 == 13 |
                data$hour1 == 14 |
                data$hour1 == 15 |
                data$hour1 == 16 |
                data$hour1 == 17 |
                data$hour1 == 18 | data$hour1 == 19 |
                data$hour1 == 20] = 2




###
data$dp_reg[data$hour1 == 0 |
              data$hour1 == 1 |
              data$hour1 == 2 |
              data$hour1 == 3 |
              data$hour1 == 4 | data$hour1 == 5 | data$hour1 == 6 |
              data$hour1 == 23] = 1
data$dp_reg[data$hour1 == 10 |
              data$hour1 == 11 |
              data$hour1 == 21 | data$hour1 == 22] = 2
data$dp_reg[data$hour1 == 7 |
              data$hour1 == 9 |
              data$hour1 == 12 |
              data$hour1 == 13 |
              data$hour1 == 14 | data$hour1 == 15 |
              data$hour1 == 20] = 3
data$dp_reg[data$hour1 == 16 | data$hour1 == 19] = 4
data$dp_reg[data$hour1 == 8 |
              data$hour1 == 17 | data$hour1 == 18] = 5
# data$dp_reg[data$hour1>=20&data$hour1<22]=6
# data$dp_reg[data$hour1>=18&data$hour1<20]=7
data$dp_cas = 0
data$dp_cas[data$hour1 == 0 |
              data$hour1 == 1 |
              data$hour1 == 2 |
              data$hour1 == 3 |
              data$hour1 == 4 |
              data$hour1 == 5 | data$hour1 == 6 | data$hour1 == 7 |
              data$hour1 == 23] = 1
data$dp_cas[data$hour1 == 8 |
              data$hour1 == 9 |
              data$hour1 == 20 | data$hour1 == 21 |
              data$hour1 == 22] = 2
data$dp_cas[data$hour1 == 10 |
              data$hour1 == 11 |
              data$hour1 == 18 | data$hour1 == 19] = 3
data$dp_cas[data$hour1 == 12 |
              data$hour1 == 13 |
              data$hour1 == 14 |
              data$hour1 == 15 | data$hour1 == 16 |
              data$hour1 == 17] = 4
### 增加日期类型
data$daytype = ""
data$daytype[data$holiday == 1] = "holiday"
data$daytype[data$workingday == 1] = "workingday"
data$daytype[data$holiday == 0 & data$workingday == 0] = "weekend"
data$weekend=0
data$weekend[data$daytype=="weekend"]=1
 
 
###因子化 变量
data$hour1 = as.factor(data$hour1)
data$day = as.factor(data$day)
data$daytype = as.factor(data$daytype)
set.seed(1234)
train=data[data$label=='train',] 
test=data[data$label=='test',]

 
train$logreg = log(train$registered + 1)
train$logcas = log(train$casual + 1)
fit1 = randomForest(
  logreg ~ hour1 + workingday + day +weekend + holiday  + temp_reg + humidity +
    atemp + windspeed + season + weather + dp_reg +  year,
  data = train,
  ntree = 250,
  importance = T
)
fit2 = randomForest(
  logcas ~ hour1 + workingday + day+weekend  + holiday  + temp_reg + humidity +
    atemp + windspeed + season + weather + dp_cas +  year,
  data = train,
  ntree = 250,
  importance = T
)


p1=predict(fit1,test)
test$logreg=p1
p2=predict(fit2,test)
test$logcas=p2

test$registered=exp(test$logreg)
test$casual=exp(test$logcas)
test$count=test$registered+test$casual
submit_final=data.frame(datetime=test$datetime,count=as.integer(test$count))
write.csv(submit_final,"sub1.csv")
head(test$datetime)

head(submit_final$datatime)
head(submit_final)
t1=read.csv("randomForest.csv")
str(t1)
str(submit_final)