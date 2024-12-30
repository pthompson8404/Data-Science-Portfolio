getwd()
#import your .csv file to your global environment
data <- read.csv("hour.csv")
str(data)

data$season<-factor(data$season, levels = c("1", "2","3","4"), labels = c ("spring", "summer", "fall", "winter"))
data$holiday<-factor(data$holiday, levels = c("0", "1"), labels = c ("not a holiday", "holiday"))
data$workingday<-factor(data$workingday, levels = c("0", "1"), labels = c ("No", "Yes"))
data$weathersit<-factor(data$weathersit, levels = c("1", "2","3","4"), labels = c ("Clear", "Mist", "Light", "Heavy"))

library(ggplot2)

#temp
ggplot(data) +
  aes(x = "", y = temp) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


#atemp
ggplot(data) +
  aes(x = "", y = atemp) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#hum
ggplot(data) +
  aes(x = "", y = hum) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#hum
ggplot(data) +
  aes(x = "", y = hum) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#windspeed
ggplot(data) +
  aes(x = "", y = windspeed) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#casual 
ggplot(data) +
  aes(x = "", y = casual) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#registered
ggplot(data) +
  aes(x = "", y = registered) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
#cnt
ggplot(data) +
  aes(x = "", y = cnt) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
## measures of central tendency
mean(data$temp)
## [1] 0.4969872
mode_temp<- table(data$temp)
mode_temp[mode_temp==max(mode_temp)]
## 0.62 
##  726
median(data$temp)
## [1] 0.5
ct <- c("mean","mode", "median")
temp_v<- c(0.4969872,0.62,0.5)
barplot(temp_v,names.arg=ct,col="blue",main="atemp measures of central tendency chart")

mean(data$atemp)
## [1] 0.4757751
median(data$atemp)
## [1] 0.4848
mode_atemp<- table(data$atemp)
mode_atemp[mode_atemp==max(mode_atemp)]
## 0.6212 
##    988
ct <- c("mean","mode", "median")
atemp_v<- c(0.4757751,0.6212,0.4848)
barplot(atemp_v,names.arg=ct,col="blue", main="atemp measures of central tendency chart")

mean(data$hum)
## [1] 0.6272288
median(data$hum)
## [1] 0.63
mode_hum<- table(data$hum)
mode_hum[mode_hum==max(mode_hum)]
## 0.88 
##  657
ct <- c("mean","mode", "median")
hum_v<- c(0.6272288,0.63,0.88)
barplot(hum_v,names.arg=ct,col="blue", main="hum measures of central tendency chart")

# 
mean(data$temp)
## [1] 0.4969872
mode_wind<- table(data$windspeed)
mode_wind[mode_wind==max(mode_wind)]
##    0 
## 2180
median(data$windspeed)
## [1] 0.194
ct <- c("mean","mode", "median")
wind_v<- c(0.5,0,0.194)
barplot(wind_v,names.arg=ct,col="blue",main="windspeed measures of central tendency chart")
mean(data$casual)
## [1] 35.67622
mode_casual<- table(data$casual)
mode_casual[mode_casual==max(mode_casual)]
##    0 
## 1581
median(data$casual)
## [1] 17
ct <- c("mean","mode", "median")
casual_v<- c(35.67622,0,17)
barplot(casual_v,names.arg=ct,col="blue",main="casual measures of central tendency chart")

mean(data$registered)
## [1] 153.7869
mode_registered<- table(data$registered)
mode_registered[mode_registered==max(mode_registered)]
##   4 
## 307
median(data$registered)
## [1] 115
ct <- c("mean","mode", "median")
reg_v<- c(153.7869,4,115)
barplot(reg_v,names.arg=ct,col="blue",main="Registered measures of central tendency chart")

mean(data$cnt)
## [1] 189.4631
mode_cnt<- table(data$cnt)
mode_cnt[mode_cnt==max(mode_cnt)]
##   5 
## 260
median(data$cnt)
## [1] 142
ct <- c("mean","mode", "median")
cnt_v<- c(189.4631,5,142)
barplot(cnt_v,names.arg=ct,col="blue",main="cnt measures of central tendency chart")

## measures of dispersion
#temp
var(data$temp)
## [1] 0.03707786
range<-max(data$temp)-min(data$temp)
range
## [1] 0.98
sd(data$temp)
## [1] 0.1925561
v <- c("Variance","range", "Standard deviation")
temp_v1<- c(0.03707786,0.98,0.1925561)
barplot(temp_v1,names.arg=v,col="blue",main="temp measures of dispersion chart")

#atemp
var(data$atemp)
## [1] 0.0295325
range<-max(data$atemp)-min(data$atemp)
range
## [1] 1
sd(data$atemp)
## [1] 0.1718502
v <- c("Variance","range", "Standard deviation")
atemp_v1<- c(0.0295325,1,0.1718502)
barplot(atemp_v1,names.arg=v,col="blue",main="atemp measures of dispersion chart")

#hum
var(data$hum)
## [1] 0.03722192
range<-max(data$hum)-min(data$hum)
range
## [1] 1
sd(data$hum)
## [1] 0.1929298
v <- c("Variance","range", "Standard deviation")
hum_v1<- c(0.03722192,1,0.1929298)
barplot(hum_v1,names.arg=v,col="blue",main="hum measures of dispersion chart")

#windspeed
var(data$windspeed)
## [1] 0.01496713
range<-max(data$windspeed)-min(data$windspeed)
range
## [1] 0.8507
sd(data$windspeed)
## [1] 0.1223402
v <- c("Variance","range", "Standard deviation")
wind_v1<- c(0.01496713,0.8507,0.01496713)
barplot(wind_v1,names.arg=v,col="blue",main="Windspeed measures of dispersion chart")

#casual
var(data$casual)
## [1] 2430.986
range<-max(data$casual)-min(data$casual)
range
## [1] 367
sd(data$casual)
## [1] 49.30503
v <- c("Variance","range", "Standard deviation")
casual_v1<- c(2430.986,367,49.30503)
barplot(casual_v1,names.arg=v,col="blue",main="Casual measures of dispersion chart")

#registered
var(data$registered)
## [1] 22909.03
range<-max(data$registered)-min(data$registered)
range
## [1] 886
sd(data$registered)
## [1] 151.3573
v <- c("Variance","range", "Standard deviation")
registered_v1<- c(22909.03,886,151.3573)
barplot(registered_v1,names.arg=v,col="blue",main="registered measures of dispersion chart")

#cnt
var(data$cnt)
## [1] 32901.46
range<-max(data$cnt)-min(data$cnt)
range
## [1] 976
sd(data$cnt)
## [1] 181.3876
v <- c("Variance","range", "Standard deviation")
cnt_v1<- c(32901.46,976,181.3876)
barplot(cnt_v1,names.arg=v,col="blue",main="cnt measures of dispersion chart")

## Skewness and dispersion
# temp

#install.packages("moments")
library(moments)
skewness(data$temp)
## [1] -0.006020364
kurtosis(data$temp)
## [1] 2.058082
v3 <- c("skewness","kurtosis")
temp_v3<- c(-0.006020364,2.058082)
barplot(temp_v3,names.arg=v3,col="blue",main="temp skewness and kurtosis chart")

#atemp
skewness(data$atemp)
## [1] -0.09042105
kurtosis(data$atemp)
## [1] 2.154486
v3 <- c("skewness","kurtosis")
atemp_v3<- c(-0.09042105,2.154486)
barplot(atemp_v3,names.arg=v3,col="blue",main="atemp skewness and kurtosis chart")

#hum
skewness(data$hum)
## [1] -0.1112775
kurtosis(data$hum)
## [1] 2.173776
v3 <- c("skewness","kurtosis")
hum_v3<- c(-0.1112775,2.173776)
barplot(hum_v3,names.arg=v3,col="blue",main="hum skewness and kurtosis chart")

#windspeed
skewness(data$windspeed)
## [1] 0.5748556
kurtosis(data$windspeed)
## [1] 3.590305
v3 <- c("skewness","kurtosis")
wind_v3<- c(0.5748556,3.590305)
barplot(wind_v3,names.arg=v3,col="blue",main="windspeed skewness and kurtosis chart")

#casual
skewness(data$casual)
## [1] 2.499021
kurtosis(data$casual)
## [1] 10.56848
v3 <- c("skewness","kurtosis")
casual_v3<- c(2.499021,10.56848)
barplot(casual_v3,names.arg=v3,col="blue",main="casual skewness and kurtosis chart")

#registered
skewness(data$registered)
## [1] 1.55777
kurtosis(data$registered)
## [1] 5.748881
v3 <- c("skewness","kurtosis")
registered_v3<- c(1.55777,5.748881)
barplot(registered_v3,names.arg=v3,col="blue",main="registered skewness and kurtosis chart")

#cnt
skewness(data$cnt)
## [1] 1.277301
kurtosis(data$cnt)
## [1] 4.41645
v3 <- c("skewness","kurtosis")
cnt_v3<- c(1.277301,4.41645)
barplot(cnt_v3,names.arg=v3,col="blue",main="cnt skewness and kurtosis chart")

##Histogram
#temp
ggplot(data, aes(x=temp)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#atemp
ggplot(data, aes(x=atemp)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#hum
ggplot(data, aes(x=hum)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#windspeed
ggplot(data, aes(x=windspeed)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#casual
ggplot(data, aes(x=casual)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#registered
ggplot(data, aes(x=registered)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#cnt
ggplot(data, aes(x=cnt)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

##simple linear regression
simple_reg <- lm(cnt ~ holiday, data = data)

summary(simple_reg)

##multiple linear regression
multiple_reg <- lm(cnt ~ season + yr + holiday + mnth + hr + holiday +weekday+workingday +weathersit + temp + atemp + hum +windspeed, data = data)
summary(multiple_reg)

