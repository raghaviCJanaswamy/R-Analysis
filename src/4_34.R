######. Exercise 4.34 

aspartame <- read.csv("/data/ex04-34.csv",head=T)

aspartame

plot(aspartame$Concentration..ppm.,aspartame$Survival.rate....,      
     xlab = "Concentration (ppm)",
     ylab = "Survival rate(%)",
     main = "aspartame survival rate study on Mice",
     pch=19, col="blue")

data_lm <- lm(aspartame$Survival.rate.... ~ aspartame$Concentration..ppm.)

# slope and intercept
coef(data_lm) 

# adds regr. line
abline(data_lm, col="dark green",lwd=1) 

