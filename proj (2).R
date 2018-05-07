library(openair)
library(readr)
Delhi_Air <- read_csv("G:/research work/sem 6/Delhi-Air.csv")
unclass(Delhi_Air$`Sampling Date`)

#worked
Delhi_Air$`Sampling Date`<-as.Date(Delhi_Air$`Sampling Date`, "%d/%m/%Y")
######
Delhi_Air$`Sampling Date`<-as.POSIXct(strptime(Delhi_Air$`Sampling Date`, "%Y-%m-%d %H:%M:%S"),origin = "1960-01-01")
data<-order(Delhi_Air$'Sampling Date')
       
?as.POSIXct.numeric
?plot(type)
#changing variable names 
names(Delhi_Air)[1]="Stncode"
names(Delhi_Air)[2]="Date"
names(Delhi_Air)[4]="City/Area"
names(Delhi_Air)[5]="Monitoring_Station_Location"
names(Delhi_Air)[6]="Agency"
names(Delhi_Air)[7]="Location_Type"
names(Delhi_Air)[10]="PM10"
names(Delhi_Air)[11]="PM25"

#Monthly Means
#NO2
par(mfrow=c(1,1))

no2_means <- aggregate(Delhi_Air["NO2"], format(Delhi_Air["Date"],"%Y-%m"), mean, na.rm = TRUE)

no2_means$Date <- seq(min(Delhi_Air$Date), max(Delhi_Air$Date), length = nrow(no2_means))

plot(no2_means$Date, no2_means[, "NO2"], type = "l",main = "Monthly NO2 Mean",lwd=5,col="#148991",xlab = "Months",ylab = "NO2 Concentration in ppm")

par(new=TRUE)
#SO2
so2_means <- aggregate(Delhi_Air["SO2"], format(Delhi_Air["Date"],"%Y-%m"), mean, na.rm = TRUE)

so2_means$Date <- seq(min(Delhi_Air$Date), max(Delhi_Air$Date), length = nrow(so2_means))

plot(so2_means$Date, so2_means[, "SO2"],axes=F, type = "l",main = "Monthly SO2 Mean",lwd=5,col="#241091",xlab = "Months",ylab = "SO2 Concentration in ppm")
par(new=TRUE)
#pm10
pm10_means <- aggregate(Delhi_Air["PM10"], format(Delhi_Air["Date"],"%Y-%m"), mean, na.rm = TRUE)

pm10_means$Date <- seq(min(Delhi_Air$Date), max(Delhi_Air$Date), length = nrow(pm10_means))

plot(pm10_means$Date, pm10_means[, "PM10"],axes=F, type = "l",main = "Monthly PM10 Mean",lwd=5,col="#64Fa91",xlab = "Months",ylab = "PM10 Concentration in ppm")
par(new=TRUE)
#pm2.5
pm2.5_means <- aggregate(Delhi_Air["PM25"], format(Delhi_Air["Date"],"%Y-%m"), mean, na.rm = TRUE)

pm2.5_means$Date <- seq(min(Delhi_Air$Date), max(Delhi_Air$Date), length = nrow(pm2.5_means))

plot(pm2.5_means$Date,legend(), pm2.5_means[, "PM25"],axes=F, type = "l",main = "Monthly PM 2.5 Mean",lwd=5,col="#474F91",xlab = "Months",ylab = "PM 2.5 Concentration in ppm")


#Combined Monthly Means 

####Columnwise
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
plot(no2_means$Date, no2_means[, "NO2"], type = "l",main = "Monthly NO2 Mean",lwd=5,col="#148991",xlab = "Months",ylab = "NO2 Concentration in ppm")
par(new=FALSE)
plot(so2_means$Date, so2_means[, "SO2"],axes=T, type = "l",main = "Monthly SO2 Mean",lwd=5,col="#241091",xlab = "Months",ylab = "SO2 Concentration in ppm")
par(new=FALSE)
plot(pm10_means$Date, pm10_means[, "PM10"],axes=T, type = "l",main = "Monthly PM10 Mean",lwd=5,col="#64Fa91",xlab = "Months",ylab = "PM10 Concentration in ppm")
par(new=FALSE)
plot(pm2.5_means$Date, pm2.5_means[, "PM25"],axes=T, type = "l",main = "Monthly PM 2.5 Mean",lwd=5,col="#474F91",xlab = "Months",ylab = "PM 2.5 Concentration in ppm")
par(new=FALSE)
mtext("Monthly Means of Pollutants",outer = TRUE,cex=2)


#####Overlapped
par(mfrow=c(1,1))
plot(no2_means$Date, no2_means[, "NO2"], axes=T,type = "l",main = "",lwd=3,col="#799591",xlab = "",ylab = "")
par(new=TRUE)
plot(so2_means$Date, so2_means[, "SO2"],axes=F, type = "l",main = "",lwd=3,col="#241091",xlab = "",ylab = "")
par(new=TRUE)
plot(pm10_means$Date, pm10_means[, "PM10"],axes=F, type = "l",main = "",lwd=3,col="#64Fa91",xlab = "",ylab = "")
par(new=TRUE)
plot(pm2.5_means$Date, pm2.5_means[, "PM25"],axes=F, type = "l",main = "",lwd=3,col="#854F91",xlab = "Months",ylab = "Concentration in PPM")
par(new=FALSE)
mtext("Monthly Means of Pollutants",outer = TRUE,cex=2)




##new dataset for the daily means
samp<-data.frame(c(Delhi_Air$Stncode),c(Delhi_Air$Date),c(Delhi_Air$NO2),c(Delhi_Air$SO2),c(Delhi_Air$PM25),c(Delhi_Air$PM10))

names(samp)[1]="Stncode"
names(samp)[2]="Date"
names(samp)[3]="NO2"
names(samp)[4]="SO2"
names(samp)[5]="PM25"
names(samp)[6]="PM10"


# calculate daily means 
par(mfrow=c(1,1))
#NO2
monthlyno2_means <- aggregate(samp["NO2"], format(samp["Date"],"%Y-%j"), mean, na.rm = TRUE)
# derive the proper sequence of dates 
monthlyno2_means[,"Date"] <- seq(min(samp[,"Date"]),max(samp[,"Date"]), length = nrow(monthlyno2_means))
# plot the means 
with(monthlyno2_means, plot(Date, NO2, type = "l",col="#912543",lwd=2,main = "Daily Time Series plot of NO2"))
#SO2
monthlyso2_means <- aggregate(samp["SO2"], format(samp["Date"],"%Y-%j"), mean, na.rm = TRUE)
# derive the proper sequence of dates 
monthlyso2_means[,"Date"] <- seq(min(samp[,"Date"]),max(samp[,"Date"]), length = nrow(monthlyso2_means))
# plot the means 
with(monthlyso2_means, plot(Date, SO2, type = "l",lwd=2,col="#204353",main = "Daily Time Series plot of SO2"))
#PM2.5
monthlypm2.5_means <- aggregate(samp["PM25"], format(samp["Date"],"%Y-%j"), mean, na.rm = TRUE)
# derive the proper sequence of dates 
monthlypm2.5_means[,"Date"] <- seq(min(samp[,"Date"]),max(samp[,"Date"]), length = nrow(monthlypm2.5_means))
# plot the means 
with(monthlypm2.5_means, plot(Date, PM25, type = "l",col="#854953",lwd=2,main = "Daily Time Series plot of PM2.5"))
#PM10
monthlypm10_means <- aggregate(samp["PM10"], format(samp["Date"],"%Y-%j"), mean, na.rm = TRUE)
# derive the proper sequence of dates 
monthlypm10_means[,"Date"] <- seq(min(samp[,"Date"]),max(samp[,"Date"]), length = nrow(monthlypm10_means))
# plot the means 
with(monthlypm10_means, plot(Date, PM10, type = "l",col="orange",lwd=2,main = "Daily Time Series plot of PM10"))
##

####Daily Means Columnwise
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
with(monthlyno2_means, plot(Date, NO2, type = "l",col="#912543",lwd=2,main = "Daily Time Series plot of NO2"))
par(new=FALSE)

with(monthlyso2_means, plot(Date, SO2, type = "l",lwd=2,col="#204353",main = "Daily Time Series plot of SO2"))
par(new=FALSE)

with(monthlypm2.5_means, plot(Date, PM25, type = "l",col="#7AE654",lwd=2,main = "Daily Time Series plot of PM2.5"))
par(new=FALSE)

with(monthlypm10_means, plot(Date, PM10, type = "l",col="orange",lwd=2,main = "Daily Time Series plot of PM10"))
par(new=FALSE)
mtext("Daily Pollutant Mean",outer = TRUE,cex=2)

#####Daily Means Overlapped
par(mfrow=c(1,1))
with(monthlyno2_means, plot(Date, NO2, type = "l",col="#FFF009",lwd=2,main = ""))
par(new=TRUE)

with(monthlyso2_means, axes=F,plot(Date, SO2, type = "l",lwd=2,col="#1E3F53",main = ""))
par(new=TRUE)

with(monthlypm2.5_means,axes=F, plot(Date, PM25, type = "l",col="#1CDA64",lwd=2,main = ""))
par(new=TRUE)

with(monthlypm10_means, axes=F,plot(Date, PM10, type = "l",col="Red",lwd=2,main = ""))
par(new=FALSE)
mtext("Daily Pollutant Mean",outer = TRUE,cex=2)
par(xpd=TRUE)
legend(x="topleft",inset=c(0,0),c("NO2","SO2","PM10","PM2.5"),lty=1,col=c("#FFF009","#1E3F53","#1CDA64","Red"),cex=.5)

###########
Delhi_Air$Date<-as.POSIXct(strptime(Delhi_Air$Date, "%Y-%m-%d "),origin = "1960-01-01")
library(openair)
names(Delhi_Air)[2]="date"
names(samp)[2]="date"
names(Delhi_Air)[2]="Date"

class(Delhi_Air$Date)
samp<-na.omit(samp)
samp$date<-names<-as.POSIXct(samp$date)

calendarPlot(samp, pollutant = "SO2",year= '2015')
calendarPlot(samp, pollutant = "NO2",year= '2015')
calendarPlot(samp, pollutant = "PM25",year= '2015')
calendarPlot(samp, pollutant = "PM10",year= '2015')

calendarPlot(samp, pollutant = "PM25", breaks = c(0, 40, 60, 80, 100),labels = c("Very low", "Low", "High", "Very High"), cols = "increment",statistic = "max",year = 2015)
calendarPlot(samp, pollutant = "PM10", breaks = c(0, 40, 60, 80, 100),labels = c("Very low", "Low", "High", "Very High"), cols = "increment",statistic = "max",year = 2015)
calendarPlot(samp, pollutant = "SO2", breaks = c(0, 4, 8, 12, 16),labels = c("Very low", "Low", "High", "Very High"), cols = "increment",statistic = "max",year = 2015)
calendarPlot(samp, pollutant = "NO2", breaks = c(0, 30, 50, 70, 90),labels = c("Very low", "Low", "High", "Very High"), cols = c("white","blue","orange","red"),statistic = "max",year = 2015)
?calendarPlot
plot(Delhi_Air$date,Delhi_Air$NO2,type = "h",xlab = "Months",main="Plot for NO2 Concentration",ylab = "Nitrogen Dioxide (ppb)")
install.packages('readxl')


########## Main Association Mining Starts here.Also install rattle package and its gui kit later 
library(arules)
library(readr)
samp$Stncode<-as.factor(samp$Stncode)
samp$date<-as.factor(samp$date)
samp$NO2<-as.factor(samp$NO2)
samp$SO2<-as.factor(samp$SO2)
samp$PM2.5<-as.factor(samp$PM2.5)
samp$PM10<-as.factor(samp$PM10)
write_csv(samp,path = "~/samp.csv")
apriori(samp)
??arules
summary(samp)

library(vcd)
#association plots b/w binary/nominal variables
assoc(~ Stncode + NO2, data=samp, shade=TRUE, abbreviate_labs=6)
assoc(~ Stncode + PM10, data=samp, shade=TRUE, abbreviate_labs=4)

library(ggmap)
ND <- get_map("New Delhi", zoom=11)
map<-ggmap(ND)


#####openair functions 
#Whisker Plots for pollutants
par(mfrow=c(2,2))
plot(as.factor(format(samp$date, "%m")), samp$NO2,main="NO2",xlab="Month",ylab="Concentration(PPM)",col="#1435E4")
plot(as.factor(format(samp$date, "%m")), samp$SO2,main="SO2",xlab="Month",ylab="Concentration(PPM)",col="#613F45")
plot(as.factor(format(samp$date, "%m")), samp$PM10,main="PM10",xlab="Month",ylab="Concentration(PPM)",col="#F31A45")
plot(as.factor(format(samp$date, "%m")), samp$PM2.5,main="PM2.5",xlab="Month",ylab="Concentration(PPM)",col="#1A4545")
mtext("Monthly Box & Whisker Plots",outer = TRUE,cex=2)


#Faulty currently
#Panel Plot
pairs(samp[sample(3:6,500), c(1, 2, 3, 4)], lower.panel = panel.smooth, upper.panel = NULL, col = "skyblue3")

##Weekly Day  wise 
weekly_means <- aggregate(sampday["SO2"], format(sampday["date"],"%w-%H"), mean, na.rm = TRUE) 
plot(weekly_means$SO2, xaxt = "n", type = "n", ylim = c(60, 270), xlab = "day of week", ylab = "nitrogen Dioxide (ppb)", main = "Nitrogen Dioxide by day of the week")
axis(1, at = seq(1, 169, 24), labels = FALSE)
# add some labels to x-axis
days = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat") loc.days = seq(13, 157, 24) # location of labels on x-axis
# write text in margin
mtext(days, side = 1, line = 1, at = loc.days) ylow = 60; yhigh = 270 # extent of shading in y direction xleft = seq(1, 145, 48) # left part of rectangles xright = xleft + 24 # right part of rectangles
# draw rectangles
rect(xleft, ylow, xright, yhigh, col = "lightcyan", border = "lightcyan")
# addline 
lines(means$nox, col = "darkorange2", lwd = 2)

#WEEKDAY Implementation
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# make data frame 
sampday = data.frame(PM10 = 5 * runif(70), PM2.5 = runif(70), weekday = as.factor(rep(weekdays, each = 10)))
# order the days i.e. do not want the default alphabetical 
sampday$weekday = ordered(sampday$weekday, levels = weekdays)
library(lattice)
# plot
xyplot(PM10 ~ PM2.5 | weekday, data = sampday, as.table = TRUE)

#TimePlot
library(latticeExtra) 
timePlot(selectByDate(samp, year = 2015), pollutant = c("NO2", "SO2", "PM25", "PM10"))
#Not Working 
trellis.last.object() +
  layer(poly.na(x, y, x, rep(0, length(x)), col = "green", alpha = 1), rows = 3)
trellis.last.object()+
  layer(poly.na(x, ifelse(y <20, NA, y), x, rep(20, length(x)), col = "yellow", alpha = 1), rows = 3) 
trellis.last.object() +
  layer(poly.na(x, ifelse(y <30, NA, y), x, rep(30, length(x)), col = "orange", alpha = 1), rows = 3) 
trellis.last.object() +
  layer(poly.na(x, ifelse(y <40, NA, y), x, rep(40, length(x)), col = "red", alpha = 1), rows = 3)

#Summary Plot
data(samp) ## make sure data that comes with openair is loaded
summaryPlot(samp)

#sorting data according to different seasons 
samp <- cutData(samp, type = "season") 
head(samp,200)
#sorting data according to different days of the week and seasons
sampday <- cutData(samp, type = "weekday") 
head(sampday)
#Time series Graphs
timeProp(selectByDate(samp, year = 2015), pollutant = "SO2", avg.time = "3 day",  date.breaks = 10, key.position = "top", key.columns = 8, ylab = "so2 (ug/m3)")
smoothTrend(samp, pollutant = "NO2", simulate = TRUE, ylab = "concentration (ppb)", main = "monthly mean no2 (bootstrap uncertainties)")
smoothTrend(samp, pollutant = "SO2", simulate = TRUE, ylab = "concentration (ppb)", main = "monthly mean no2 (bootstrap uncertainties)")
smoothTrend(samp, pollutant = "PM10", simulate = TRUE, ylab = "concentration (ppb)", main = "monthly mean no2 (bootstrap uncertainties)")
smoothTrend(samp, pollutant = "PM25", simulate = TRUE, ylab = "concentration (ppb)", main = "monthly mean no2 (bootstrap uncertainties)")

smoothTrend(samp, pollutant = c("NO2", "PM10", "SO2","PM25"), type = c("Stncode"), date.breaks = 2, lty = 0)
timeVariation(sampday, pollutant = c("NO2", "SO2", "PM25", "PM10"), normalise = TRUE)
timeVariation(samp, pollutant = "PM10", statistic = "median", col = "firebrick")
timeVariation(samp, pollutant = "PM25", statistic = "median", col = "orange")
timeVariation(samp, pollutant = "NO2", statistic = "median", col = "purple")
timeVariation(samp, pollutant = "SO2", statistic = "median", col = "magenta")
scatterPlot(samp, x = "NO2", y = "PM25", method = "hexbin", col= "jet")
scatterPlot(samp, x = "SO2", y = "PM25", method = "hexbin", col= "jet")
scatterPlot(samp, x = "PM10", y = "PM25", method = "hexbin", col= "jet")
scatterPlot(samp, x = "NO2", y = "PM10", method = "hexbin", col= "jet")
scatterPlot(samp, x = "NO2", y = "SO2", method = "hexbin", col= "jet")

#Seasonwise
scatterPlot(samp, x = "NO2", y = "SO2", method = "hexbin", col= "jet", type = c("season"))
scatterPlot(samp, x = "NO2", y = "PM25", method = "hexbin", col= "jet", type = c("season"))
scatterPlot(samp, x = "PM25", y = "PM10", method = "hexbin", col= "jet", type = c("season"))
#linear relation
linearRelation(samp, x = "NO2", y = "SO2")
#trendplot
trendLevel(samp,x="season",y="Stncode" ,pollutant = "NO2")

GoogleMapsPlot(samp, lat = "latitude", long = "longitude", pollutant = "NO2", maptype = "roadmap", col = "jet")


corPlot(samp, dendrogram = TRUE,type="season")


#modeling into transactions
trans <- as(samp, "transactions")
trans
summary(trans)
itemLabels(trans)


str(samp)

itemFrequencyPlot(trans, topN=50,  cex.names=.5)
trans <- dissimilarity(sample(trans, 150), method = "phi", which = "items")

#main association rules work starts here.
rules<-apriori(trans,parameter = list(supp=0.01,conf=0.1,minlen=2))
summary(trans)
summary(rules)
?`inspect,rules-method`
inspect(rules,ruleSep="--->",setStart=" ",setEnd="")
rules1<-apriori(trans,parameter = list(supp=0.1,conf=0.1))
inspect(rules1,ruleSep="--->",setStart=" ",setEnd="")
rules2<-apriori(trans,parameter = list(supp=0.05,conf=0.1))
inspect(rules2,ruleSep="--->",setStart=" ",setEnd="")
plot(rules,measure = "support", shading = "lift")
plot(rules,measure = "support", shading = "lift",method="matrix")
plot(rules, method="matrix", measure=c("lift", "confidence"), control=list(reorder=TRUE))
plot(rules, method="grouped")
plot(rules, method="paracoord")
onerule <- sample(rules, 1)
plot(onerule, method="doubledecker", data = samp)

par(mfrow=c(1,1))
plot(head(sort(rules, by="lift"), 117),method="graph", measure = "support", shading = "lift",control=list(cex=.7))
itemFrequencyPlot(items(rules))
#####


####Implementation of Eclat Algorithm

ec_rules<-eclat(trans,parameter = list(supp=0.05))
inspect(ec_rules)
itemsets<-eclat(trans,parameter = list(supp=0.01,tidLists=TRUE,target="frequent itemsets",minlen=1))
as(tidLists(itemsets),"list")
image(tidLists(itemsets),colors(distinct = TRUE))
inspect(itemsets)
plot(itemsets)


library(rattle)
library(arules)
library(arulesViz)
rattle()
?apriori
?inspect
save(samp,file="assorule.rda")
load("assorule.rda")
plot(as.factor(format(samp$date, "%m")), samp$NO2)
plot(Delhi_Air$Date,Delhi_Air$PM2.5,type = "h",xlab = "Months",main="Plot for PM 2.5 Concentration",ylab = "PM 2.5 (ppb)")
rattle()
plot(Delhi_Air$Date,Delhi_Air$SO2,type = "h",xlab = "Months",main="Plot for SO2 Concentration",ylab = "Sulphur Dioxide (ppb)")

plot(Delhi_Air$Date,Delhi_Air$PM10,type = "h",xlab = "Months",main="Plot for PM10 Concentration",ylab = "PM10 (ppb)")

