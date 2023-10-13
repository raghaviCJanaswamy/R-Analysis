######. Exercise 4.28

#### a)

Calories <- read.csv("/data/ex04-28.csv",head=T)

Calories$Nitrates
Calories$Absorbance

plot(Calories$Nitrates,Calories$Absorbance,      xlab = "Nitrate",
     ylab = "Absorbance",
     main = "Nitrate absorbance plot",
     pch=19, col="blue")

# How does the correlation coefficient look? --
cor_coeff <- cor(Calories$Nitrates,Calories$Absorbance)
cor_coeff

#####. b) 

## Mean and Standard deviations for Nitrates and Absorbance observations
mean(Calories$Nitrates)
sd(Calories$Nitrates)

mean(Calories$Absorbance)
sd(Calories$Absorbance)

### Slope

## y_hat = a + bx
## b = r * sy/sx
## a = y_hat - bx

# Finding Slope
b_val_Slope <- cor_coeff * (sd(Calories$Absorbance)/sd(Calories$Nitrates))

b_val_Slope

# Finding a --
a_val <- mean(Calories$Absorbance) - b_val_Slope * mean(Calories$Nitrates)
a_val 

# equation for the regression line --
### Pred_y <- a_val + b_val_Slope * tobe_X


Pred_y <- a_val + b_val_Slope * pred_x

pred_x <- (40 - a_val) /b_val_Slope
pred_x

# finding predicted y for a given x
# all regression lines pass trhough
# (mean_x,mean_y), so let's find y_hat
# for mean_x and check if it equals 
# mean_y.

y_hat = a_val + b_val_Slope * mean(Calories$Nitrates)
y_hat

y_hat == mean(Calories$Absorbance)

# plotting the regression line --
abline(a_val,b_val_Slope,col="green",lwd=4)

