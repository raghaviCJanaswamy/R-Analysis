######. Exercise 4.38 

plasticweight <- read.csv("/data/ex04-26.csv",head=T)

dim(plasticweight)

plot(plasticweight$Time..days.,plasticweight$Weight.lost..mg.,      
     xlab = "Time (Days)",
     ylab = "Weight Lost (mg)",
     main = "Plastic weight loss over time in a Bacteria solution",
     pch=19, col="dark orange")

data_lm <- lm(plasticweight$Weight.lost..mg. ~ plasticweight$Time..days.)

corr <- cor(plasticweight$Weight.lost..mg. , plasticweight$Time..days.)
corr 
 
# adds regr. line
abline(data_lm, col="blue",lwd=1) 

#. Square of the correlation is the confidence

conf <- (corr * corr) * 100
conf
# slope and intercept
coef(data_lm)