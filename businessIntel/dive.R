df_orig = read.csv("techsoup_datakind_sample.csv")
df_orig$org = factor(df$org)
df_orig$org_type = factor(df$org_type)


#-----------------------------------------------------------------------
#Revenue by OrgSubType
#-----------------------------------------------------------------------
filter = !(is.na(df$revenue)) & (df$revenue >= 0)

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
filter = !(is.na(df$revenue)) & (df$revenue >= 0)

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
filter = !(is.na(df$budget)) & (df$budget >= 0)
df = df_orig[filter, ]
df$budget = df$budget/1000

orgs = unique(df$org)
budget = df[match(orgs, df$org), colnames(df)]$budget
hist(budget)
#-----------------------------------------------------------------------