
## Does the fraction of non-photos in a group effect the communicativity of photoed PPTs
rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("global.vars",global.vars()))])
#rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("global.vars",global.vars()) | !(ls(all=TRUE) %in% c("global.vars",global.vars())) )])
assign("last.warning",NULL,envir=baseenv())

#.ts.data <- data.table(read.csv("../data/techsoup_datakind_sample.csv",stringsAsFactors=FALSE))
.ts.data <- data.table(read.csv("../data/techsoup_datakind_details_wide.csv",stringsAsFactors=FALSE))

.ts.data[,transaction_date:=as.POSIXct(reg_date)]
.ts.data[,reg_date:=as.POSIXct(reg_date)]
.ts.data <- .ts.data[!is.na(budget) & budget>=0]

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

suppressWarnings(yearly.data <- melt(yearly.data))

library(gplots)

png("./figures/gr_avg_trans_year.png")
plotmeans(value ~ variable,yearly.data[variable!="X2015"],n.label=FALSE)
dev.off();

ot.data <- unique(.ts.data[,list(org_type,country),by=org])
ot.data$org <- as.character(ot.data$org)

yearly.data <- merge(yearly.data,ot.data,by="org")
yearly.data$org_type <- as.factor(yearly.data$org_type)


to.plot <- unique(yearly.data[,list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot,aes(x=variable,y=avg_trans,colour=org_type)) + geom_point() +
    geom_errorbar(aes(ymin=avg_trans-1.96*se_trans,
                      ymax=avg_trans+1.96*se_trans))
ggsave("./figures/gr_avg_trans_year_org_type.png",plot=p)



png("./figures/gr_avg_trans_year_us.png")
plotmeans(value ~ variable,yearly.data[variable!="X2015" & country=="US"],n.label=FALSE)
dev.off();


to.plot <- unique(yearly.data[country=="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot[variable!="X2015"],aes(x=variable,y=avg_trans,colour=org_type,group=org_type)) + geom_point() +
    geom_line()
ggsave("./figures/gr_avg_trans_year_org_type_us.png",plot=p)



png("./figures/gr_avg_trans_year_non_us.png")
plotmeans(value ~ variable,yearly.data[variable!="X2015" & country!="US"],n.label=FALSE)
dev.off();


to.plot <- unique(yearly.data[country!="US",list(avg_trans = mean(value),
                                    se_trans = sd(value)/sqrt(.N)),
                              by=c("variable","org_type")])

p <- ggplot(to.plot[variable!="X2015"],aes(x=variable,y=avg_trans,colour=org_type,group=org_type)) + geom_point() +
    geom_line()
ggsave("./figures/gr_avg_trans_year_org_type_non_us.png",plot=p)


## Revenue
