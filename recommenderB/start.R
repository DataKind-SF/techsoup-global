library("readr")
library("dplyr")
library("lubridate")
tsdSample <- read_csv("data/techsoup_datakind_sample.csv")
#tsd_sample has info on each purchase
tsdDemo <- read_csv("data/techsoup_datakind_code_demo.csv")
#tsdDemo is like sample, but without revenue

# Pri_key – unique incrementing ID, Integer
# Org – internal organization ID, BigInt
# City – City Name of physical location, Varchar(50)
# State – State Code of physical location, Varchar(5)
# PostalCode – ZipCode.Postal Delivery code – Varchar(20)
# OrgType – Major Type of Nonprofit Org – Char(4)
# Org_SubType – Subtype Code of Nonprofit Org – Char(4)
# Budget – Annual Budget for organization in USD – Currency
# Reg_Date – First Date of Registration with TechSoup Global – Date/Time
# Contact – contact email – varchar(100)
# Role – ID for contact’s role at organization – varchar(50)
# Role_description – contact’s role at organization – varchar(100)
# OrderNum – Unique ID for order – BigInt
# Tax_ID_Number – tax ID number for matching to IRS BMF file – Varchar(100)
# Transaction_date – date of transaction – Date/Time
# Vendor – Unique ID for vendor of product – BigInt
# Item_ID – Unique ID for Item – Varchar(20)
# Category – type of product ** only on recent live products – varchar(100)
# Count_Licenses – Count of licenses of that product for this order – Bigint
# Revenue – Total amount of admin fee for donation – Currency
# Value – Total value of donated product in USD – Currency



#tsdWide is same as tsdSample, but 2M records - use n_max=whatever to limit
tsdWide <- read_csv("data/techsoup_datakind_details_wide.csv")
#BMF has more details on who the clients are. 1.5M of them
tsdBMF <- read_csv("data/big_bmf.csv")
BMFsample <- tsdBMF[1:100,]
#tsdWide %>% filter(tax_id_number %in% tsdBMF$EIN[1:100])
#this doesn't work because tax_id_numbers are in all kinds of formats. EIN is numbers.

#this works, because the particular tax id is known  Get's whole transaction history for this client
x=tsdWide %>% filter(tax_id_number == 122)

#what we really want is, given a tax_id_number, find the BMF record
y=tsdBMF %>% filter(EIN == 1076156) #but tax_id_number is this weird string, in general


#well another thing we can do is just add up revenue coming from transactions where budget <= x, and make a curve

tsd2010 = tsdWide %>% filter(year(tsdWide$transaction_date)==2010)
revenue2010 = sum(tsd2010$revenue)

budget <- c(1e5, 3e5, 5e5, 7e5, 1e6, 3e6, 5e6, 7e6, 1e7, 3e7, 5e7, 7e7, 1e8)#, 5e8, 1e9, 5e9, 1e10)
bucketRevenue = budget-budget

for (i in 1:length(budget)) bucketRevenue[i] = sum(tsd2010$revenue[tsd2010$budget <= budget[i]], na.rm=TRUE)/revenue2010
plot(budget, bucketRevenue, main="2010")

#2014 CDF
tsd2014 = tsdWide %>% filter(year(tsdWide$transaction_date)==2014)
revenue2014 = sum(tsd2014$revenue)

budget <- c(1e5, 3e5, 5e5, 7e5, 1e6, 2e6, 3e6, 5e6, 7e6, 1e7, 3e7)#, 5e7, 7e7, 1e8)#, 5e8, 1e9, 5e9, 1e10)
bucketRevenue14 = budget-budget

for (i in 1:length(budget)) bucketRevenue14[i] = sum(tsd2014$revenue[tsd2014$budget <= budget[i]], na.rm=TRUE)/revenue2014
plot(budget, bucketRevenue14, main="2014")

#2006 CDF
tsd2006 = tsdWide %>% filter(year(tsdWide$transaction_date)==2006)
revenue2006 = sum(tsd2006$revenue)

budget <- c(1e5, 3e5, 5e5, 7e5, 1e6, 2e6, 3e6, 5e6, 7e6, 1e7, 3e7)#, 5e8, 1e9, 5e9, 1e10)
bucketRevenue06 = budget-budget

for (i in 1:length(budget)) bucketRevenue06[i] = sum(tsd2006$revenue[tsd2006$budget <= budget[i]], na.rm=TRUE)/revenue2006
plot(budget, bucketRevenue06, main="2006")

plot(budget, bucketRevenue14, main="2014 dots vs. 2006 line")
lines(budget, bucketRevenue06)