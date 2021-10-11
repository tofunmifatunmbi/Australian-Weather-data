## Data Analysis of 2021 Australian Capital Territory, Canberra Weather data
### Problem 1 
## Data Scraping
setwd("~/Documents/APSU COURSE CODE")
dat= read.csv("weather-2021.csv", check.names = F)
dim(dat)

### Problem 2 
## Data Cleaning
#2a
dat=dat[,-c(10)]
#2b
names(dat) <- c("Month", "Date", "MinTemp", "MaxTemp", "Rainfall",
                "Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed",
                "Temp9am", "Humidity9am", "Cloud9am", "WindDir9am",
                "WindSpeed9am", "Pressure9am", "Temp3pm", "Humidity3pm",
                "Cloud3pm", "WindDir3pm", "WindSpeed3pm", "Pressure3pm")
dim(dat)
names(dat)
#2c
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
# 2d
apply(dat,2,FUN=function(x){table(x, useNA = "ifany")})
data[data$WindSpeed9am == "Calm", ]$WindSpeed9am <- 0
table(data$WindSpeed9am,useNA="ifany")
# 2e
# WindSpeed3pm data "Calm" cannot be change to 0 because of the presence
# of missing values in the dataset
# error missing values are not allowed in subscripted assignments of data frames
#data[data$WindSpeed3pm == "Calm", ]$WindSpeed3pm <- 0
data$WindSpeed9am <- as.numeric(data$WindSpeed9am)
data$WindSpeed3pm <- as.numeric(data$WindSpeed3pm)
str(data)
# 2f
data$RainToday = c(ifelse(data$Rainfall >1, 1,0))
data$RainTomorrow = c(data$RainToday[2:nrow(data)],NA) 
# 2g
write.csv(data,"newDataSet.csv")

### Problem 3 
## Exploratory Data Analysis
data$RainTomorrow <- ifelse(data$RainTomorrow==0, "No", "Yes")
tab=table(data$RainTomorrow,useNA="no")
tab
barplot(tab,names.arg=row.names(tab),col=c("blue","yellow"),ylab="Frequency",xlab="Rain Tomorrow",main="Plot of Rain Tomorrow")

# RainTomorrow on Wind Gust Speed
boxplot<-boxplot(data$WindGustSpeed~data$RainTomorrow,xlab="Rain Tomorrow", ylab="Wind Gust Speed",main="Box plot of Wind Gust Speed on Rain Tomorrow",col=c("blue","yellow"))
hist(data$WindGustSpeed,main="Histogram of Wind Gust Speed",xlab="Wind Gust Speed")
t.test(data$WindGustSpeed~data$RainTomorrow, data=data, alternative = "two.sided",na.action = na.omit)

# RainTomorrow on Humidity at 3pm
boxplot<-boxplot(data$Humidity3pm~data$RainTomorrow,xlab="Rain Tomorrow", ylab="Humidity at 3pm",main="Box plot of Humidity at 3pm on Rain Tomorrow",col=c("blue","yellow"))
hist(data$Humidity3pm,main="Histogram of Humidity at 3pm",xlab="Humidity at 3pm")
t.test(data$Humidity3pm~data$RainTomorrow, data=data, alternative = "two.sided",na.action = na.omit)

# RainTomorrow on Cloud3pm
boxplot<-boxplot(data$Cloud3pm~data$RainTomorrow,xlab="Rain Tomorrow", ylab="Cloud at 3pm",main="Box plot of Cloud at 3pm on Rain Tomorrow",col=c("blue","yellow"))
hist(data$Cloud3pm,main="Histogram of CLoud at 3pm",xlab="CLoud at 3pm")
wilcox.test(data$Cloud3pm~data$RainTomorrow, data=data, alternative = "two.sided",na.action = na.omit)

# RainTomorrow on Rain Today
set.seed(500)
tab2=table(data$RainTomorrow,data$RainToday,useNA = "no")
tab2
barplot(tab2,col=cm.colors(2),legend.text=c("No","Yes"),beside =T,
        xlab="Rain Today",ylab="Frequency",main="Multiple Bar chart of RainToday on RainTomorrow")
fisher.test(tab2,simulate.p.value = T)
chisq.test(tab2)

#####Month on Cloud ###
set.seed(500)
tab= table(data$Month,data$Cloud3pm)
tab
addmargins(tab)
fisher.test(tab,simulate.p.value = T)
