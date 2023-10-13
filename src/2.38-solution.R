group1 <- c(27,	22,	29,	21,	19,	33	,16,	20,	24	,27	,28,	19)

group2 <- c(12,	12,	15,	9,	20,	18,	17,	14,	14,	2,	17,	19)

group3 <- c(18,	4,	22,	15,	18,	19,	22,	12,	12)

n <- 4134

## Findings 0.17 to 12.8Ug/l
##percentiles <- c(5,10,25,50,75,90,95)

mercuryLevel <- c(0.81, 0.99,1.35,1.86,2.52,3.33,4.02)





boxplot(mercuryLevel,
        main="Mercury Levels Report",
        ylab="Level",
        col=c("green"),
        names=c("Mercury")) 


######

mercuryLevel.quantile <- quantile(mercuryLevel)
print(mercuryLevel.quantile)



