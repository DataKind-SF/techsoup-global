df = read.csv("item_time.csv")

df = df[ , !names(df) %in% c("X")]
df$reg_date = as.Date(df$reg_date)
df$transaction_date = as.Date(df$transaction_date)
df$reg_date = as.numeric(format(df$reg_date, "%Y"))
df$transaction_date = as.numeric(format(df$transaction_date, "%Y"))

freq.items = table(df$transaction_date, df$item)
freq.items = data.frame(freq.items)
names(freq.items) = c("year", "item", "freq")
freq.items = freq.items[freq.items$year != "2015", ]
top.items = ddply(freq.items, .(item), function(x) sum(x$freq))
names(top.items) = c("item", "freq")
top.items = top.items[order(top.items$freq, decreasing=TRUE),]
top.items = top.items[1:10, ]$item

freq.items = freq.items[freq.items$item %in% top.items, ]

ggplot (freq.items, aes(x=year, y=freq, group=item, fill=item)) + geom_area() +
        ggtitle("Top 10 Transacted Items") + xlab("Year") + ylab("Number of Transactions")

