
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

