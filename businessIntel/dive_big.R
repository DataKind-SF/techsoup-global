df_orig = read.csv("techsoup_big.csv")
df_orig$org = factor(df_orig$org)
df_orig$org_type = factor(df_orig$org_type)
df_orig$budget = as.numeric(as.character(df_orig$budget))
df_orig$vendor = as.factor(df_orig$vendor)
df_orig$order_num = as.factor(df_orig$order_num)
df_orig$item = as.factor(df_orig$item)
df_orig$transaction_date = as.Date(df_orig$transaction_date)
df_orig$reg_date = as.Date(df_orig$reg_date)
#-----------------------------------------------------------------------
#Revenue by OrgSubType
#-----------------------------------------------------------------------
filter = !(is.na(df_orig$revenue)) & (df_orig$revenue >= 0)

df = df_orig[filter, ]
df$revenue = df$revenue/1000

revenue_org_subtype = tapply(df$revenue, df$org_subtype, sum)
revenue_org_stype=data.frame(cbind(attributes(revenue_org_subtype)$dimnames[[1]], 
                                   revenue_org_subtype))
rownames(revenue_org_stype)=c(1:nrow(revenue_org_stype))
colnames(revenue_org_stype) = c("orgsubtype", "revenue")
revenue_org_stype$revenue = as.numeric(as.character(revenue_org_stype$revenue))
revenue_org_stype = revenue_org_stype[order(-revenue_org_stype$revenue), ]

top.revenue = c(1:10)
barplot(revenue_org_stype[top.revenue,]$revenue, names.arg=revenue_org_stype[top.revenue,]
        $orgsubtype, xlab="Org SubType", ylab="Revenue (in thousand USD)", col="red")
#------------------------------------------------------------------------
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#Revenue by OrgType
#-----------------------------------------------------------------------
filter = !(is.na(df_orig$revenue)) & (df_orig$revenue >= 0)

df = df_orig[filter, ]
df$revenue = df$revenue/1000

revenue_org_type = tapply(df$revenue, df$org_type, sum)
revenue_org_type = data.frame(cbind(attributes(revenue_org_type)$dimnames[[1]], 
                                    revenue_org_type))
rownames(revenue_org_type) = c(1:nrow(revenue_org_type))
colnames(revenue_org_type) = c("orgtype", "revenue")
revenue_org_type$revenue = as.numeric(as.character(revenue_org_type$revenue))
revenue_org_type = revenue_org_type[order(-revenue_org_type$revenue), ]

top.revenue = c(1:10)
barplot(revenue_org_type[top.revenue,]$revenue, names.arg=revenue_org_type[top.revenue,]
        $orgtype, xlab="Org Type", ylab="Revenue (in thousand USD)", col="red")
#------------------------------------------------------------------------
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#Budget Segmentation Analysis
#-----------------------------------------------------------------------
filter = !(is.na(df_orig$budget)) & (df_orig$budget >= 0) & 
        !(is.na(df_orig$revenue)) & (df_orig$revenue >= 0)

df = df_orig[filter, ]
df$org = as.character(df$org)

df$budget = df$budget/1000
df$revenue = df$revenue/1000

revenue = tapply(df$revenue, df$org, sum)
revenue = data.frame(cbind(attributes(revenue)$dimnames[[1]], 
                           revenue))
colnames(revenue) = c("org", "revenue")
revenue$revenue = as.numeric(as.character(revenue$revenue))
        
orgs = unique(df$org)
budget = df[match(orgs, df$org), colnames(df) %in% c("org", "org_type", "org_subtype", "budget")]
revenue_budget = cbind(budget, revenue[match(orgs, revenue$org), ]$revenue)
colnames(revenue_budget) = c("org", "org_type", "org_subtype", "budget", "revenue")
hist(budget)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#Item Time Analysis
#-----------------------------------------------------------------------
filter = !(is.na(df_orig$item)) & !is.na((df_orig$reg_date)) & !is.na((df_orig$transaction_date))

df = df_orig[filter, ]
df$org = as.character(df_orig$org)

# filter = !(is.na(df_orig$item)) & !is.na((df_orig$reg_date)) & !is.na((df_orig$transaction_date))
# 
# df_orig = df_orig[filter, ]
# df_orig$org = as.character(df_orig$org)
# df_orig = df_orig[, c("org", "reg_date", "transaction_date", "item", "org_type", "org_subtype", "country")]
# write.csv(df_orig, file="item_time.csv", quote=FALSE, col.names=FALSE)

#org_time = ddply(df[, names(df) %in% c("org", "reg_date", "transaction_date", "org_type", "org_subtype"
#                                           , "country")], 
#                   .(org), function(x) x[which.min(x$transaction_date),])


