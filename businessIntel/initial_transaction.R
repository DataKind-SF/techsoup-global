df_orig = read.csv("initial_transaction_by_org.csv")
df_orig$transaction_date = as.Date(df_orig$transaction_date)
        
df = df_orig[df_orig$country !="", ]
df = df_orig[!is.na(df_orig$org_type), ]

year = as.numeric(format(df$transaction_date, "%Y"))
catCountry.US_nonUS = rep("US", nrow(df))
catCountry.US_nonUS[df$country != "US"] = "nonUS"

mosaicplot(table(year, catCountry.US_nonUS), main="Customers Joined by Geographic Region Over
           Time", xlab="Join Date", ylab="Country Type", color=c("orange", "blue"))


topCountries = aggregate(df[, names(df) %in% c("country")], list(df$country), length)
colnames(topCountries) = c("country", "total")
topCountries = topCountries[order(topCountries$total, decreasing=TRUE), ]

topCountries = topCountries[1:10, 1]
df.top_countries = df[df$country %in% topCountries, ]
df.top_countries$country = droplevels(df.top_countries$country)
year.top_countries = as.numeric(format(df.top_countries$transaction_date, "%Y"))

table.top_countries = table(year.top_countries, df.top_countries$country)
table.top_countries = data.frame(table.top_countries)
colnames(table.top_countries) = c("year", "country", "freq")
table.top_countries = table.top_countries[table.top_countries$year != "2015", ]

ggplot(table.top_countries, aes(x=year, y=freq, group=country, fill=country)) + geom_area() + 
        ggtitle("Join History of Top 10 Countries") + xlab("Year") + ylab("Number of Joins")

#-----------------------------------------------------------------------
#Do certain OrgTypes transact earlier?
#-----------------------------------------------------------------------

df = df_orig[!is.na(df_orig$org_type), ]
year = as.numeric(format(df$transaction_date, "%Y"))

table.year = table(year, df$org_type)
df.year = as.data.frame(table.year)
colnames(df.year) = c("year", "org_type", "freq")

df.year = df.year[df.year$year != "2015", ]
ggplot(df.year, aes(x=year, y=freq, group=org_type, fill=org_type)) + geom_area() +
        ggtitle("First Transaction Date by OrgType") + xlab("Year") + ylab("Number of Transactions")

