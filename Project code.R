######### Question 1############
setwd("~/5310 Project 1")
dat= read.csv("weather-2021.csv", check.names = F)
dim(dat)
######### Question 2############
dat=dat[,-c(10)]
names(dat) <- c("Month", "Date", "MinTemp", "MaxTemp", "Rainfall",
                "Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed",
                "Temp9am", "Humidity9am", "Cloud9am", "WindDir9am",
                "WindSpeed9am", "Pressure9am", "Temp3pm", "Humidity3pm",
                "Cloud3pm", "WindDir3pm", "WindSpeed3pm", "Pressure3pm")
dim(dat);
names(dat)
data=dat
vnames <- colnames(data)
n <- nrow(data)
out <- NULL
for (j in 1:ncol(data)){
  vname <- colnames(data)[j]
  x <- as.vector(data[,j])
  n1 <- sum(is.na(x), na.rm=TRUE)  # NA
  n2 <- sum(x=="NA", na.rm=TRUE) # "NA"
  n3 <- sum(x==" ", na.rm=TRUE)  # missing
  nmiss <- n1 + n2 + n3
  nmiss <- sum(is.na(x))
  ncomplete <- n-nmiss
  out <- rbind(out, c(col.num=j, v.name=vname, mode=mode(x), n.level=length(unique(x)),
                      ncom=ncomplete, nmiss= nmiss, miss.prop=nmiss/n))
}
out <- as.data.frame(out)
row.names(out) <- NULL
out
apply(dat,2,FUN=function(x){table(x, useNA = "ifany")})
data[data$WindSpeed9am == "Calm", ]$WindSpeed9am <- 0
table(data$WindSpeed9am,useNA="ifany")
data$WindSpeed9am <- as.numeric(data$WindSpeed9am)
str(data)
data$RainToday = c(ifelse(data$Rainfall >1, 1,0))
data$RainTomorrow = c(data$RainToday[2:nrow(data)],NA) 
#write.csv(data,"newDataSet.csv")
######### Question 3############
data$RainTomorrow <- ifelse(data$RainTomorrow==0, "No", "Yes")
tab = table(data$RainTomorrow)
barplot(tab,names.arg=row.names(tab),col=c("blue","yellow"),ylab="Frequency",xlab="Rain Tomorrow",main="Plot of Rain Tomorrow")
boxplot<-boxplot(data$WindGustSpeed~data$RainTomorrow,xlab="Rain Tomorrow", ylab="Wind Gust Speed",main="Box plot of Wind Gust Speed on Rain Tomorrow",col=c("blue","yellow"))
data <- na.omit(data)
wilcox.test(data$RainTomorrow~data$WindGustSpeed, data=data, alternative = "two.sided",na.action = na.omit)
t.test(data$RainTomorrow~data$WindGustSpeed, data=data, alternative = "two.sided",na.action)

                 