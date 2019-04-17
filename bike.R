#Clear Environment-
rm(list=ls())
#Set working directory-
setwd("D:/R-programming/2.Project- Bike Rental-R_File")
#Check working directory-
getwd()

#load data-
data= read.csv("day.csv")

#------------------------------Exploratory Data Analysis-------------------------------------------#
class(data)
dim(data)
head(data)
names(data)
str(data)
summary(data)

#Remove the instant variable, as it is index in dataset.
data= subset(data,select=-(instant))
#Remove date variable as we have to predict count on seasonal basis not date basis-
data= subset(data,select=-(dteday))
#Remove casual and registered variable as count is sum of these two variables-
data= subset(data,select=-c(casual,registered))

#check the remaining variables-
names(data)

#Rename the variables-
names(data)[2]="year"
names(data)[3]="month"
names(data)[7]="weather"
names(data)[8]="temprature"
names(data)[10]="humidity"
names(data)[12]="count"

#Seperate categorical and numeric variables-
names(data)

#numeric variables-
cnames= c("temprature","atemp","humidity","windspeed","count")

#categorical varibles-
cat_cnames= c("season","year","month","holiday","weekday","workingday","weather")

#=================================Data Pre-processing==========================================#

#--------------------------------Missing Vlaue Analysis----------------------------------------#
#Check missing values in dataset-
sum(is.na(data))
#Missing value= 0
#No Missing values in data.

#-----------------------------------Outlier Analysis----------------------------------------------#
df=data
data=df

#create Box-Plot for outlier analysis-
library(ggplot2)    #Library for visulization-
for(i in 1:length(cnames)){
  assign(paste0("AB",i),ggplot(aes_string(x="count",y=(cnames[i])),d=subset(data))+
           geom_boxplot(outlier.color = "Red",outlier.shape = 18,outlier.size = 2,
                        fill="Purple")+theme_get()+
           stat_boxplot(geom = "errorbar",width=0.5)+
           labs(x="Count of Bike",y=cnames[i])+
           ggtitle("Boxplot of count of bikes with",cnames[i]))
}

gridExtra::grid.arrange(AB1,AB2,AB3,ncol=3)
gridExtra::grid.arrange(AB4,AB5,ncol=2)

#Replace outliers with NA-
for(i in cnames){
  print(i)
  outlier= data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(outlier))
  data[,i][data[,i] %in% outlier]=NA
}

sum(is.na(data))

#Impute outliers by median method-
data$humidity[is.na(data$humidity)]=median(data$humidity,na.rm=TRUE)
data$windspeed[is.na(data$windspeed)]=median(data$windspeed,na.rm=TRUE)

sum(is.na(data))

#---------------------------------Data Understanding----------------------------------------------#

#Barplot of bike rented with respect to working days-
ggplot(data, aes(x = reorder(weekday,-count), y = count))+
  geom_bar(stat = "identity",fill = "aquamarine3")+
  labs(title = "Number of bikes rented with respect to days", x = "Days of the week")+ 
  theme(panel.background = element_rect("antiquewhite"))+
  theme(plot.title = element_text(face = "bold"))

#->from bar plot we can see maximum bikes rented on day 5 least bikes on day 0.

#Bikes rented with respect to temp and humidity-
ggplot(data,aes(temprature,count)) + 
  geom_point(aes(color=humidity),alpha=0.5) +
  labs(title = "Bikes rented with respect to variation in temperature and hunidity", x = "Normalized temperature")+
  scale_color_gradientn(colors=c(dark blue,blue,light blue,light green,yellow,orange,red)) +
  theme_bw()

#->maximum bike rented between temp 0.50 to 0.75 and humidity 0.50 to 0.75

#Bikes rented with respect to temp and windspeed-
ggplot(data, aes(x = temprature, y = count))+
  geom_point(aes(color=weather))+
  labs(title = "Bikes rented with respect to temperature and weathersite", x = "Normalized temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

#->maximum bike rented with windspeed and normalized temp between 0.50 to 0.75

#Bikes rented with respect to temp and season-
ggplot(data, aes(x = temprature, y = count))+
  geom_point(aes(color=season))+
  labs(title = "Bikes rented with respect to temperature and season", x = "Normalized temperature")+
  #  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()
#->maximum bike rented in season4

#---------------------------------Feature Selection----------------------------------------------#
df=data
data=df

#correlation analysis for numeric variables-
library(corrgram)
corrgram(data[,cnames],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation plot for numeric variables")

#correlated variable= temprature & atemp

#Anova analysis for categorical variable with target numeric variable-
for(i in cat_cnames){
  print(i)
  Anova_result= summary(aov(formula = count~data[,i],data))
  print(Anova_result)
}

#Dimension Reduction-
data = subset(data,select=-c(atemp,holiday,weekday,workingday))

#---------------------------------Feature Scaling--------------------------------------------#
df=data
data=df

#update numeric variables after dimension reduction-
cnames= c("temprature","humidity","windspeed","count")

#skewness test for continuous variables-
library(propagate)
for(i in cnames){
  print(i)
  skew= skewness(data[,i])
  print(skew)
}
#No skewness in dataset.

#Normality check using histogram plot-
hist(data$temprature,col="Green",xlab="Temprature",ylab="Frequency",
     main="Histogram of Temprature")
hist(data$humidity,col="Red",xlab="Humidity",ylab="Frequency",
     main="Histogram of Humidity")
hist(data$windspeed,col="Purple",xlab="Windspeed",ylab="Frequency",
     main="Histogram of Windspeed")

#check summary of continuous variable to check the scaling- 
for(i in cnames){
  print(summary(data[,i]))
}
#as from summary, the data is already normalized, so no need for scaling.

#save the pre-processed data in drive-
write.csv(data,"Bike_Rental_count.csv",row.names=FALSE)

#====================================Model Devlopment==========================================#
#Sampling to divide data into test and train
tr.index= sample(1:nrow(data),0.8*nrow(data))
Train=dt[tr.index,]
Test=dt[-tr.index,]
#------------------DECISION TREE---------------------------------------------------------------------------------
dt_fit=rpart(casual~temp+hum+windspeed+season+yr+mnth+weekday+workingday+weathersit,data = Train,method = "anova")
#----------Predicting----------------
pr_DT=predict(dt_fit,Test[,1:11])
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}
MAPE(Test[,14], pr_DT)
#MAPE=22.5
#Accuracy=77.5
#-------------------Random Forest----------------------
rf_fit=randomForest(casual~temp+hum+windspeed+season+yr+mnth+weekday+workingday+weathersit,data = Train)
#----------Predicting----------------
pr_RF=predict(rf_fit,Test[,1:11])
MAPE(Test[,12], pr_RF)
#MAPE= 14.2
#Accuracy=85.8
#Test$casual=pr_rf
#rf_fit_r=randomForest(registered~temp+hum+windspeed+season+yr+mnth+weekday+workingday+weathersit,data = Train)
#pr_RF_r=predict(rf_fit_r,Test[,1:11])
#Test$registered=pr_RF_r
#Test$cnt=Test$Casual+Test$registered
##write.csv(Test,file="submit.csv",row.names=FALSE)
#----------------Linear Reression----------------------
lr_fit=lm(casual~temp+hum+windspeed+season+yr+mnth+weekday+workingday+weathersit,data = Train)
#prdicting for the test data
pr_LR=predict(lr_fit,Test[,1:11])
MAPE(Test[,14], pr_LR)
#MAPE=19.2
#Accuracy=80.8
