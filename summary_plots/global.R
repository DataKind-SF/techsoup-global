
## Does the fraction of non-photos in a group effect the communicativity of photoed PPTs
rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("global.vars",global.vars()))])
#rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("global.vars",global.vars()) | !(ls(all=TRUE) %in% c("global.vars",global.vars())) )])
assign("last.warning",NULL,envir=baseenv())

#.ts.data <- data.table(read.csv("../data/techsoup_datakind_sample.csv",stringsAsFactors=FALSE))
#.ts.data <- data.table(read.csv("../data/techsoup_datakind_details_wide.csv",stringsAsFactors=FALSE))

#.ts.data[,transaction_date:=as.POSIXct(transaction_date)]
#.ts.data[,reg_date:=as.POSIXct(reg_date)]
#.ts.data$budget <- as.numeric(.ts.data$budget)
#.ts.data <- .ts.data[!is.na(budget) & (budget>=0) & (budget < 1e8)]

load("../data/ts_data.RData")

to.plot <- .ts.data[country=="US",list(N=length(unique(org))),by=state]
to.plot$state <- reorder(to.plot$state,-to.plot$N)
p <- ggplot(to.plot,aes(x=state,y=N)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90,size=6)) + ylab("# unique orgs")
ggsave("./figures/bar_org_by_state.png",plot=p)

to.plot <- .ts.data[,list(N=length(unique(org))),by=country]
to.plot$country <- reorder(to.plot$country,-to.plot$N)
p <- ggplot(to.plot[country!="US"],aes(x=country,y=N)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90,size=6)) + ylab("# unique orgs")
ggsave("./figures/bar_org_by_country.png",plot=p)

to.plot <- .ts.data[category!="",list(N=length(unique(item))),by=category]
to.plot$category <- reorder(to.plot$category,-to.plot$N)
p <- ggplot(to.plot,aes(x=category,y=N)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90)) + ylab("# unique items")
ggsave("./figures/bar_unique_item_by_category.png",plot=p)

to.plot <- unique(.ts.data[category!="",list(rev_per_cat=mean(revenue)),by=category])
to.plot$category <- reorder(to.plot$category,-to.plot$rev_per_cat)
p <- ggplot(to.plot,aes(x=category,y=rev_per_cat)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90)) + ylab("Avg revenue")
ggsave("./figures/bar_revenue_per_cat.png",plot=p)

to.plot <- unique(.ts.data[,list(n_trans=.N),by=org_type])
to.plot$org_type <- reorder(to.plot$org_type,-to.plot$n_trans)
p <- ggplot(to.plot,aes(x=org_type,y=n_trans)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90)) + ylab("# trans")
ggsave("./figures/bar_trans_per_org_type.png",plot=p)



## yearly transaction trends
yearly.data <- data.table(read.csv("../data/org_year.csv",stringsAsFactors=FALSE))
yearly.data$org <- as.character(yearly.data$org)

library(reshape2)
suppressWarnings(yearly.data <- melt(yearly.data))

library(gplots)

png("./figures/gr_avg_trans_year.png")
plotmeans(value ~ variable,yearly.data[variable!="X2015"],n.label=FALSE)
dev.off();


.ts.data$budget_bins <- cut(.ts.data$budget,quantile(.ts.data$budget,probs=seq(0,1,by=0.1)),dig.lab=8,include.lowest=TRUE)
ot.data <- unique(.ts.data[,list(org_type,country,budget_bins),by=org])

ot.data$org <- as.character(ot.data$org)
yearly.data <- merge(yearly.data,ot.data,by="org")
yearly.data$org_type <- as.factor(yearly.data$org_type)


to.plot <- unique(yearly.data[,list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot,aes(x=variable,y=avg_trans,colour=org_type)) + geom_point() +
    geom_line()
                                        #    geom_errorbar(aes(ymin=avg_trans-1.96*se_trans,
#                      ymax=avg_trans+1.96*se_trans))
ggsave("./figures/gr_avg_trans_year_org_type.png",plot=p)


to.plot <- unique(yearly.data[country=="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable")])
p <- ggplot(to.plot[ variable!="X2015"],aes(x=variable,y=avg_trans,group=1)) + geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=avg_trans - 1.96*se_trans,
                      ymax=avg_trans + 1.96*se_trans)) +
    theme_bw() + scale_x_discrete(labels=2006:2014) + xlab("Year") + ylab("Avg # Transactions / Org")
ggsave("./figures/gr_avg_trans_year_us.png",plot=p)




to.plot <- unique(yearly.data[country!="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable")])
p <- ggplot(to.plot[ variable!="X2015"],aes(x=variable,y=avg_trans,group=1)) + geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=avg_trans - 1.96*se_trans,
                      ymax=avg_trans + 1.96*se_trans)) +
    theme_bw() + scale_x_discrete(labels=2006:2014) + xlab("Year") + ylab("Avg # Transactions / Org")
ggsave("./figures/gr_avg_trans_year_non_us.png",plot=p)


yearly.data[,is_us:=(country=="US")]
to.plot <- unique(yearly.data[,list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","is_us")])
p <- ggplot(to.plot[!is.na(is_us) & variable!="X2015"],aes(x=variable,y=avg_trans,colour=is_us,group=is_us)) + geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=avg_trans - 1.96*se_trans,
                      ymax=avg_trans + 1.96*se_trans)) +
    theme_bw() + scale_x_discrete(labels=2006:2014) + xlab("Year") + ylab("Avg # Transactions / Org") + ggtitle("Avg # Transactions / Org") +
scale_colour_discrete(name="Is US")
ggsave("./figures/gr_avg_trans_year_is_us.png",plot=p)


#png("./figures/gr_avg_trans_year_us.png")
#plotmeans(value ~ variable,yearly.data[variable!="X2015" & #country=="US"],n.label=FALSE)
#dev.off();


to.plot <- unique(yearly.data[country=="US" & variable!="X2015",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot[!is.na(org_type) & variable!="X2015"],aes(x=variable,y=avg_trans,colour=org_type,group=org_type)) + geom_point() +
    geom_line() +
    theme_bw() + scale_x_discrete(labels=2006:2014) + xlab("Year") + ylab("Avg # Transactions / Org")
ggsave("./figures/gr_avg_trans_year_org_type_us.png",plot=p)



#png("./figures/gr_avg_trans_year_non_us.png")
#plotmeans(value ~ variable,yearly.data[variable!="X2015" & #country!="US"],n.label=FALSE)
#dev.off();


to.plot <- unique(yearly.data[country!="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot[!is.na(org_type) & variable!="X2015"],aes(x=variable,y=avg_trans,colour=org_type,group=org_type)) + geom_point() +
    geom_line() +
    theme_bw() + scale_x_discrete(labels=2006:2014) + xlab("Year") + ylab("Avg # Transactions / Org")
ggsave("./figures/gr_avg_trans_year_org_type_non_us.png",plot=p)



# US only
to.plot <- unique(yearly.data[country=="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","budget_bins")])

p <- ggplot(to.plot[variable!="X2015"],aes(x=variable,y=avg_trans,colour=budget_bins,group=budget_bins)) + geom_point() +
    geom_line() +
        ggtitle("US transactions/year")


ggsave("./figures/gr_avg_trans_year_budget_bins_us.png",plot=p)

# non US only
to.plot <- unique(yearly.data[country!="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","budget_bins")])

p <- ggplot(to.plot[variable!="X2015"],aes(x=variable,y=avg_trans,colour=budget_bins,group=budget_bins)) + geom_point() +
    geom_line() +
    ggtitle("Non-US transactions/year")

ggsave("./figures/gr_avg_trans_year_budget_bins_non_us.png",plot=p)


## Revenue
yearly.data <- data.table(read.csv("../data/org_rev_by_year.csv",stringsAsFactors=FALSE))
yearly.data$org <- as.character(yearly.data$org)

suppressWarnings(yearly.data <- melt(yearly.data))

yearly.data <- merge(yearly.data,ot.data,by="org")
yearly.data$org_type <- as.factor(yearly.data$org_type)


# revenue per year
to.plot <- unique(yearly.data[variable!="rev_2015" & country=="US",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev,colour=org_type,group=org_type)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org")
ggsave("./figures/gr_avg_rev_year_org_type_us.png",plot=p)


to.plot <- unique(yearly.data[variable!="rev_2015" & country!="US",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev,colour=org_type,group=org_type)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org")
ggsave("./figures/gr_avg_rev_year_org_type_non_us.png",plot=p)



# US only
to.plot <- unique(yearly.data[country=="US" & variable!="rev_2015",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","budget_bins")])

p <- ggplot(to.plot[variable!="rev_2015"],aes(x=variable,y=avg_rev,colour=budget_bins,group=budget_bins)) + geom_point() +
    geom_line() + ylab("Avg Revenue") + xlab("year") +
        ggtitle("US revenue/year")


ggsave("./figures/gr_avg_rev_year_budget_bins_us.png",plot=p)

# non US only
to.plot <- unique(yearly.data[country!="US" & variable!="rev_2015",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","budget_bins")])

p <- ggplot(to.plot[variable!="rev_2015"],aes(x=variable,y=avg_rev,colour=budget_bins,group=budget_bins)) + geom_point() +
    geom_line() + ylab("Avg Revenue") + xlab("year") +
    ggtitle("Non-US revenue/year")

ggsave("./figures/gr_avg_rev_year_budget_bins_non_us.png",plot=p)





# JP only
to.plot <- unique(yearly.data[country=="JP" & variable!="rev_2015",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","budget_bins")])

p <- ggplot(to.plot[variable!="rev_2015"],aes(x=variable,y=avg_rev,colour=budget_bins,group=budget_bins)) + geom_point() +
    geom_line() + ylab("Avg Revenue") + xlab("year") +
        ggtitle("US revenue/year")


ggsave("./figures/crisis/gr_avg_rev_year_budget_bins_jp.png",plot=p)

to.plot <- unique(yearly.data[variable!="rev_2015" & country=="JP",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev,colour=org_type,group=org_type)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org")
ggsave("./figures/crisis/gr_avg_rev_year_org_type_jp.png",plot=p)


# Egypt
to.plot <- unique(yearly.data[country=="EG" & variable!="rev_2015",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","budget_bins")])

p <- ggplot(to.plot[variable!="rev_2015"],aes(x=variable,y=avg_rev,colour=budget_bins,group=budget_bins)) + geom_point() +
    geom_line() + ylab("Avg Revenue") + xlab("year") +
        ggtitle("Egypt revenue/year")
ggsave("./figures/crisis/gr_avg_rev_year_budget_bins_eg.png",plot=p)

to.plot <- unique(yearly.data[variable!="rev_2015" & country=="EG",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev,colour=org_type,group=org_type)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org (Egypt)")
ggsave("./figures/crisis/gr_avg_rev_year_org_type_eg.png",plot=p)

yearly.data[,is_eg:=(country=="EG")]
to.plot <- unique(yearly.data[variable!="rev_2015",list(
                                  avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","is_eg")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev,colour=is_eg,group=is_eg)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org")
ggsave("./figures/crisis/gr_avg_rev_year_is_eg.png",plot=p)


to.plot <- unique(yearly.data[variable!="rev_2015" & country=="EG",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org (Egypt)")
ggsave("./figures/crisis/gr_avg_rev_year_eg.png",plot=p)



to.plot <- unique(yearly.data[variable!="rev_2015" & country=="JP",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable")])

p <- ggplot(to.plot,aes(x=variable,y=avg_rev)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org (JP)")
ggsave("./figures/crisis/gr_avg_rev_year_jp.png",plot=p)



yearly.data[,is_us:=(country=="US")]

to.plot <- unique(yearly.data[variable!="rev_2015",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","is_us")])

p <- ggplot(to.plot[!is.na(is_us) & variable!="X2015"],aes(x=variable,y=avg_rev,colour=is_us,group=is_us)) + geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=avg_rev - 1.96*se_rev,
                      ymax=avg_rev + 1.96*se_rev)) +
    theme_bw() + scale_x_discrete(labels=2006:2014) + xlab("Year") + ylab("Avg Revenue / Org") + ggtitle("Avg Revenue / Org") +
scale_colour_discrete(name="Is US")

ggsave("./figures/gr_avg_rev_year_is_us.png",plot=p)


# AU?, NZ, RU,
yearly.data[,continent := ifelse(country %in% c("US","CA"),"North America",
                           ifelse(country %in% c("VI","PR","MX","BR","CO","CL","GF"),"South America",
                                  ifelse(country %in% c("CD","KE","GH","ZA","RW","CH","BW","MO","EG","TP"), "Africa",
                                         ifelse(country %in% c("BE","GB","PL","SE","CZ","MD","NO","DK","IT","AL","GE","LV","BA","ES","MK","IE","HU","FR","AT","LU","NL","RU","DE","HR","SI","UA","RO","SK","BG","ME","RS","YU","LT"), "Europe",
                                                ifelse(country %in% c("AU","NZ","AS","FM","GU","SB","PG"), "Oceana",
                                                       ifelse(country %in% c("MP","IN","HK","TW","JP","IL","PH","SG","TR","MY","ID","NP","VN","TH"), "Asia", "NA"))))))]


yearly.data <- yearly.data[continent!="NA"]

to.plot <- unique(yearly.data[variable!="rev_2015",list(avg_rev = mean(value),
                                    se_rev = sd(value)/sqrt(.N)),
                              by=c("variable","continent")])

p <- ggplot(to.plot[!is.na(continent)],aes(x=variable,y=avg_rev,color=continent,group=continent)) + geom_point() +
    geom_line() + xlab("year") + ylab("Avg Revenue") + ggtitle("Avg Revenue / org ") + theme(axis.text.x = element_text(angle=60,hjust=1))
ggsave("./figures/crisis/gr_avg_rev_year_continent.png",plot=p)


budget.data <- unique(.ts.data[,list(org_type,country,budget=median(budget,na.rm=T),revenue=mean(revenue,na.rm=T)),by=org])

png("./figures/gr_budget_org_type_us.png",plot=p)
plotmeans(budget ~ org_type,budget.data[country=="US"],connect=FALSE)
dev.off()

png("./figures/gr_budget_org_type_non_us.png",plot=p)
plotmeans(budget ~ org_type,budget.data[country!="US"],connect=FALSE)
dev.off()


png("./figures/gr_revenue_org_type_us.png",plot=p)
plotmeans(revenue ~ org_type,budget.data[country=="US"],connect=FALSE)
dev.off()

png("./figures/gr_revenue_org_type_non_us.png",plot=p)
plotmeans(revenue ~ org_type,budget.data[country!="US"],connect=FALSE)
dev.off()

.ts.data[,is_us:=(country=="US")]
.ts.data[,trans_year:=year(transaction_date)]
item.data <- unique(.ts.data[,list(n_unique_items = length(unique(item[!is.na(item)]))), by=c("trans_year","is_us")])
item.data <- item.data[!is.na(is_us)]

p <- ggplot(item.data,aes(x=trans_year,y=n_unique_items,colour=is_us)) + geom_point(size=6) + theme_bw() +
    ylab("# of unique items") + xlab("Transaction Year") +
           theme(axis.text = element_text(size=18),axis.title=element_text(size=22),legend.title = element_text(size=22),
                 legend.text=element_text(size=18)) + ggtitle("Unique items per year")
ggsave("./figures/gr_n_unique_items_per_year.png",plot=p,width=16,height=9)

item.data <- unique(.ts.data[country!="US",list(n_unique_items = length(unique(item))), by=year(transaction_date)])
p <- ggplot(item.data,aes(x=year,y=n_unique_items)) + geom_point(size=6) + theme_bw() + ylab("# of unique items") + xlab("Transaction Year") + theme(axis.text = element_text(size=18),axis.title=element_text(size=20),legend.text = element_text(size=18)) + ggtitle("Non-US")
ggsave("./figures/gr_n_unique_items_per_year_non_us.png",plot=p)
