org2014 <- read_csv("output/org4pred2014.csv")
top5 <- org2014[,6:10]
rowsum5 <- rowSums(top5)
nullH <- table(rowsum5)/length(rowsum5)

model1 = nullH-nullH + c(1.2040000e+04, 1.65700000e+04, 5.18700000e+03,1.3100000e+03,2.11000000e+02,2.90000000e+01)/(144921*.25)

plotit = data.frame(rbind(nullH, model1))

barplot(as.matrix(plotit, nr=2), beside=T, col=c("purple", "green"), 
        xlab="n purchases from top 5 recs", ylab="% of orgs",
        main="Improved Recommendations")
legend("topright", c("naive","model"), pch=15, 
       col=c("purple","green"), 
       bty="n")

######

nullSales = nullH %*% seq(from = 0, to=5)
model1Sales = model1 %*% seq(from = 0, to=5)
improvement = model1Sales / nullSales -1
# 83.4% improvement
