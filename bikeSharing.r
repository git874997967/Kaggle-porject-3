train<-read.csv('train.csv')
test<-read.csv('test.csv')
test$registered=0#由于测试集没有会员的租车的数量，所以为了下面进行合并，先补充测试集这个变量，并设置为0
test$casual=0#由于测试集没有非会员的租车的数量，所以为了下面进行合并，先补充测试集这个变量，并设置为0
test$count=0#由于测试集没有总租车的数量，所以为了下面进行合并，先补充测试集这个变量，并设置为0
data<-rbind(train,test)#合并数据集，方便进行特征工程
table(is.na(data))#查看是否有空值
#描述分析
hist(data$season)#画出直方图，查看数据变量的分布情况，针对名义型数据
hist(data$weather)
hist(data$humidity)
hist(data$windspeed)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
library(lubridate)#专门对时间进行处理的包
data$hour1<-as.factor(hour(data$datetime))#通过hour函数提取时间类型数据的小时，对名义型数据转为factor
data$hour2<-substr(data$datetime,12,13)#通过提取字符串的形式提取时间类型数据的小时
 train<-data[as.integer(day(data$datetime))<20,]#日期小于20号的属于训练集，再次划分出训练集
#对名义型数据，通过做箱型图来查看因变量分布情况，以此判断因变量与这些自变量是否有关
boxplot(train$count~train$hour1,xlab="hour",ylab="count")#查看小时与总租车的关系
boxplot(train$registered~train$hour1,xlab="registered",ylab="count")#查看小时与注册用户租车数的关系
boxplot(train$casual~train$hour1,xlab="casual",ylab="count")##查看小时与非注册用户租车数的关系
data$day<-weekdays(as.Date(data$datetime))# 提取星期属性
table(data$day)

table(day(data$datetime))
boxplot(train$count~train$day,xlab="day",ylab="count")#总租车数与星期的箱型图
boxplot(train$casual~train$day,xlab="day",ylab="casual")#非注册用户租车数与星期的箱型图
boxplot(train$registered~train$day,xlab="day",ylab="registered")#注册用户租车数与星期的箱型图
boxplot(train$registered~train$weather,xlab='day',ylab='registered')#注册用户租车数与天气的箱型图
boxplot(train$casual~train$weather,xlab='day',ylab='casual')#非注册用户租车数与天气的箱型图
boxplot(train$count~train$weather,xlab='day',ylab='count')#总租车数与天气的箱型图
head(train)
#针对数值型数据，可以直接用因变量和数值型数据的变量求相关系数，以判断它们之间的相关性
cor_data<-data.frame(train$count,train$register,train$casual,train$temp,train$atemp,train$humidity,train$windspeed)#提取注册用户租车数、非注册用户租车数、总租车数与温度、体感温度、湿度的数据集
 cor(cor_data)#通过皮尔逊相关系数的方法了解租车数与湿度、温度、风速、体感温度是否相关
# 相关性	负	正
# 无	−0.09 to 0.0	0.0 to 0.09
# 弱	−0.3 to −0.1	0.1 to 0.3
# 中	−0.5 to −0.3	0.3 to 0.5
# 强	−1.0 to −0.5	0.5 to 1.0
data$year<-year(data$datetime)#提取年份
 
boxplot(train$count~train$year,xlab='year',ylab='count')
#特征工程
library(rpart)
library(rpart.plot)
 #将小时变成整数型
#借助决策树，决策树可以得到某一属性的分裂特征值，更好地将某些连续型变量转为名义型变量，即将这些连续变量进行分类
d=rpart(registered~hour1,data=train)
rpart.plot(d)
 
#将注册用户的用车时间点分为7类
data$dp_reg<-0
data$dp_reg[data$hour1<7.5]=1
data$dp_reg[data$hour1>=22]=2
data$dp_reg[data$hour1>=9.5&data$hour1<18]=3
data$dp_reg[data$hour1>=7.5&data$hour1<8.5]=4
data$dp_reg[data$hour1>=8.5&data$hour1<9.5]=5
data$dp_reg[data$hour1>=20&data$hour1<22]=6
data$dp_reg[data$hour1>=18&data$hour1<20]=7
#将非注册用户的用车小时分为4类
data$dp_cas=0
f=rpart(casual~hour1,data=train)
rpart.plot(f)
data$dp_cas[data$hour1<8.5]=1
data$dp_cas[data$hour1>=8.5&data$hour1<10]=2
data$dp_cas[data$hour1>=20]=3
data$dp_cas[data$hour1>=10&data$hour1<20]=4
data$temp_reg<-0
g=rpart(registered~temp,data=train)
rpart.plot(g)
#将气温分为4类
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>=13&data$temp<23]=2
data$temp_reg[data$temp>=23&data$temp<30]=3
data$temp_reg[data$temp>=30]=4
#将年份分为4个季度 8 quarters in total
data$year_part=0
data$month<-month(data$datetime)
data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011'&data$month>3]=2
data$year_part[data$year=='2011'&data$month>6]=3
data$year_part[data$year=='2011'&data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012'&data$month>3]=6
data$year_part[data$year=='2012'&data$month>6]=7
data$year_part[data$year=='2012'&data$month>9]=8
#day属性，为工作日、节日、周末
data$day_type=""
data$day_type[data$holiday==0&data$workingday==0]='weekend'
data$day_type[data$workingday==1]='holiday'
data$day_type[data$holiday==0&data$workingday==1]='working day'
data$weekend=0
data$weekend[data$day=="Saturday"|data$day=="Sunday"]=1
data$hour1=as.factor(data$hour1)
data$day=as.factor(data$day)
data$day_type=as.factor(data$day_type)
#下面进行建模
library(randomForest)
set.seed(451)
sample_data=sample(2,nrow(data),replace = T,prob=c(0.15,0.85)) 
train=data[sample_data==2,]
test=data[sample_data==1,]
train$logreg<-log(train$registered+1)#因为从箱型图可以发现，租车数有很多离群点，所以采取对数操作，防止出现0,所以加1
train$logcas<-log(train$casual+1)#取对数操作，防止出现0,所以加1
fit1<-randomForest(logreg~hour1+workingday+day+holiday+day_type+temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year_part,data=train,importance=TRUE,ntree=250)#注册用户租车数模型，ntree表示构成随机森林的树的棵数，Importance为True表示考虑属性的重要性
test$logreg<-predict(fit1,test)
 
set.seed(451)
fit2<-randomForest(logcas~hour1+workingday+day+holiday+day_type+temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year_part,data=train,importance=TRUE,ntree=250)#非注册用户租车数模型
 
test$logcas<-pred2
test$registered=exp(test$logreg)-1#反取对数
test$casual=exp(test$logcas)-1#反取对数
test$count=test$registered+test$casual#总租车数
submit_final<-data.frame(datetime=test$datetime,count=as.integer(test$count))
submit_final
 
write.csv(submit_final,"randomForest.csv")
