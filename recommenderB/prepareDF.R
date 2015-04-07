library("readr")
library("dplyr")
library("lubridate")
tsdWide <- read_csv("data/techsoup_datakind_details_wide.csv")
tsd2014 = tsdWide %>% filter(year(tsdWide$transaction_date)==2014)

item.f = factor(tsd2014$item)
dummies = data.frame(model.matrix(~item.f))

big = cbind(tsd2014, dummies) 
#probably easier to build the binary indicator of org x product from this matrix directly, but not sure how

#average the budget per org
avgBudget = tsd2014 %>%
  group_by(org) %>%
  summarise(avgBudget = mean(budget))

#for each org, get the record with the latest transaction date.  We'll use this for
#org_type and org_subtype and so on
lastOrgType = tsd2014 %>%
  group_by(org, org_type, org_subtype, reg_date) %>%
  summarise(transaction_date=max(transaction_date))



#Want to group by org & take max for each of the products.  Easier to take everything not not the products.



#put these together
df2014 = cbind(avgBudget, lastOrgType[,-c(1,5)])
#transform reg_date
df2014$reg_date = 2015 - year(df2014$reg_date)
names(df2014)[names(df2014) == 'reg_date'] <- 'yrsWith'
#want one row per org, with org attributes and binary on each product bought





#make one column per product
#first find unique products, bought more than 100 times

topItems=sort(table(tsd2014$item),decreasing=TRUE)
topItems = topItems[topItems > 100]
itemColumnNames = rep("", length(topItems))
for (i in 1:length(topItems)) itemColumnNames[i]=paste("item", names(topItems[i]), sep="")
df2014[,itemColumnNames] <- 0


#now, say whether each org bought each product in 2014 or not!!
#there is undoubtedly a MUCH more efficient way to do this. This would be 20 minutes or something.

for (i in 1:nrow(df2014)){
orgItems = tsd2014 %>% filter(org ==df2014$org[i]) %>% filter(item %in% names(topItems)) %>% select(item) 

  for (j in 1:nrow(orgItems)) {pos = which(names(topItems) == orgItems[j,][[1]])
    df2014[i,pos+5]=1
  }
}

write.csv(df2014, "output/org4pred2014.csv", row.names=FALSE)


#holdback 20% to test on, train on the rest
df2014$org_type <- as.factor(as.character(df2014$org_type))
df2014$org_subtype <- as.factor(df2014$org_subtype)
randRows = sample(1:nrow(df2014), size=0.8*nrow(df2014), replace=FALSE)
train = df2014[randRows,]
test = df2014[-randRows,]

