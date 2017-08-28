
dataACT<-read.csv("activity.csv")
dataACT$date<-as.Date(dataACT$date,"%Y-%m-%d")
by_data<-dataACT %>% group_by(date)
s<-aggregate(steps~date,by_data,sum,na.rm = TRUE )
s$mean<-mean(s$steps)
s$median<-median(s$steps)

hist(s$steps,col="blue",main="Histogram of Total Steps taken per day",xlab="Total Steps taken per day",cex.axis=1,cex.lab = 1)

s_mean<-mean(s$steps)
s_median<-median(s$steps)

by_data_mean<-aggregate(steps~interval,data=dataACT,FUN=mean,na.rm=TRUE)	
plot(by_data_mean$interval,by_data_mean$steps,type='l', xlab = "5-minute Time Intervals ", ylab = "Mean number of steps taken a day", main = "Average number of steps a day (all Days)",  col = "brown")


by_data_mean[which.max(by_data_mean$steps),"interval"]

sum(!complete.cases(dataACT))

c_data<-dataACT

for (i in 1:nrow(c_data)) {
if (is.na(c_data[i,"steps"])) {
c_data[i,"steps"] <- by_data_mean$steps[by_data_mean$interval == c_data$interval[i]]
}
}

c_total<-aggregate(steps~date,c_data,sum )
c_mean<-mean(aggregate(steps~date,c_data,sum )$steps)
c_median<-median(aggregate(steps~date,c_data,sum )$steps)

library(lubridate)
library(lattice)


for (i in 1:nrow(c_data)) {
        c_data$dayName[i]<-ifelse(wday(as.Date(c_data$date[i]),label=TRUE)%in% c("Sat","Sun"),"weekend","weekday")
        }


c_data$dayName<-as.factor(c_data$dayName)
##weekday<-filter(c_data, dayName=="weekday")
##weekend<-filter(c_data, dayName=="weekend")
c_data_mean= aggregate(steps ~ interval + dayName, c_data, mean)
xyplot(steps ~ interval | factor(dayName), data = c_data_mean, aspect =1/2,
type = "l")


