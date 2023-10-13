group1 <- c(27,	22,	29,	21,	19,	33	,16,	20,	24	,27	,28,	19)

group2 <- c(12,	12,	15,	9,	20,	18,	17,	14,	14,	2,	17,	19)

group3 <- c(18,	4,	22,	15,	18,	19,	22,	12,	12)


boxplot(group1,group2, group3,
        main="Comparison of the Logging counts on Groups of trees",
        ylab="Count",
        col=c("green","yellow", "pink"),
        names=c("Group1","Group2","Group3")) 


######

group1.quantile <- quantile(group1)
print(group1.quantile)

group2.quantile <- quantile(group2)
print(group2.quantile)

group3.quantile <- quantile(group3)
print(group3.quantile)

