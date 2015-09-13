
data<-read.csv("activity.csv")
library("dplyr")
library("ggplot2")


stepsperday<-group_by(data,date)
stepssum<-summarize(stepsperday,steps=sum(steps,na.rm = TRUE))
plot<-ggplot(stepssum,aes(date,steps)) + geom_histogram(stat="identity")
print(plot)
mean(stepssum$steps)
median(stepssum$steps)


stepsperinterval<-group_by(data,interval)
stepinterval<-summarize(stepsperinterval,steps=round(mean(steps,na.rm=TRUE)))
plot<-ggplot(stepinterval,aes(interval,steps)) + geom_line(stat = "identity")
print(plot)
stepinterval1<-arrange(stepinterval,desc(steps))
stepinterval1[1,1]


dataNA<-filter(data,is.na(data$steps))
nrow(dataNA)
dataNA1<-merge(dataNA,stepinterval,by.x = "interval",by.y = "interval")
dataNA1<-dataNA1[,c(4,3,1)]        
dataNA1<-arrange(dataNA1,date)
dataNA1<-rename(dataNA1,steps=steps.y)
datanew<-data[!is.na(data$steps),]
datanew<-rbind(datanew,dataNA1)
datanew<-arrange(datanew,date)

stepsperday<-group_by(datanew,date)
stepssum<-summarize(stepsperday,steps=sum(steps,na.rm = TRUE))
plot<-ggplot(stepssum,aes(date,steps)) + geom_histogram(stat="identity")
print(plot)
mean(stepssum$steps)
median(stepssum$steps)


Sys.setlocale("LC_ALL","English")
weekday<-weekdays(as.Date(datanew$date))
weekend<-grepl("Saturday|Sunday",weekday)
datanew<-cbind(datanew,weekday)
datanew<-cbind(datanew,weekend)
a<-filter(datanew,weekend)
b<-filter(datanew,!(weekend))
a$weekday<-"weekend"
b$weekday<-"weekday"

datanew<-rbind(a,b)
datanew<-arrange(datanew,date)
datanew<-datanew[,c(1,2,3,4)]

weekendinterval<-group_by(a,interval)
weekendintervals<-summarize(weekendinterval,steps=round(mean(steps,na.rm=TRUE)))

weekdayinterval<-group_by(b,interval)
weekdayintervals<-summarize(weekdayinterval,steps=round(mean(steps,na.rm=TRUE)))
weekendintervals[,3]<-"weekend"
weekdayintervals[,3]<-"weekday"
dataplot<-rbind(weekdayintervals,weekendintervals)

library("lattice")
xyplot(steps~interval|V3,data=dataplot,layout=c(1,2),type="l",xlab = "Interval",ylab = "Number of steps")






