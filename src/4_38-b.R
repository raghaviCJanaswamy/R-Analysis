######. Exercise 4.38 

carnivores <- read.csv("/data/ta04-02.csv",head=T)

dim(carnivores)
plot(carnivores$Body.mass..kg.,carnivores$Abundance,      
     xlab = "Body Mass Index",
     ylab = "Abundance",
     main = "Carnivores Abundance based on the BMI",
     pch=19, col="red")

data_lm <- lm(carnivores$Abundance ~ carnivores$Body.mass..kg.)

corr <- cor(carnivores$Abundance , carnivores$Body.mass..kg.)
corr 

#. Square of the correlation is the confidence

conf <- (corr * corr) * 100
conf
# slope and intercept
coef(data_lm) 

# adds regr. line
abline(data_lm, col="blue",lwd=1) 

