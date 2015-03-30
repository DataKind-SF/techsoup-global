library("readr")
library("dplyr")
library("lubridate")


tsdWide <- read_csv("data/techsoup_datakind_details_wide.csv")

budget <- c(1e5, 3e5, 5e5, 7e5, 1e6, 3e6, 5e6, 7e6, 1e7, 3e7, 5e7, 7e7, 1e8)#, 5e8, 1e9, 5e9, 1e10)

blen = length(budget)
b0 = rep(0,blen)
bucketRevenue = data.frame("2006"=b0, "2007"=b0, "2008"=b0, "2009"=b0, "2010"=b0, "2011"=b0, "2012"=b0, "2013"=b0, "2014"=b0, "2015"=b0)

for (j in 2006:2015){
  tsdj = tsdWide %>% filter(year(tsdWide$transaction_date)==j)
  revj = sum(tsdj$revenue)
  for (i in 1:length(budget)) bucketRevenue[i,j-2005] = sum(tsdj$revenue[tsdj$budget <= budget[i]], na.rm=TRUE)/revj
}

plot(3e7,1,cex=0, main="cumulative share of revenue by budget", xlim=c(0,3e7), ylim=c(0,1),xlab="Org Budget <=$X",ylab="% of annual TSG revenue")
lines(budget, bucketRevenue[,1], col="Red")
lines(budget, bucketRevenue[,2], col="Orange")
lines(budget, bucketRevenue[,3], col="Yellow")
lines(budget, bucketRevenue[,4], col="Green")
lines(budget, bucketRevenue[,5], col="Blue")
lines(budget, bucketRevenue[,6], col="Cyan")
lines(budget, bucketRevenue[,7], col="Violet")
lines(budget, bucketRevenue[,8], col="lightgrey")
lines(budget, bucketRevenue[,9], col="darkgrey")
lines(budget, bucketRevenue[,10], col="black")
legend(2.5e7, .9, colnames(bucketRevenue), lty=c(1,1), col=c("Red", "Orange", "Yellow", "Green", "Blue", "Cyan", "Violet", "lightgrey", "darkgrey", "black"), cex=.4)
