######. Exercise 4.31

#### a)

social <- read.csv("/data/ex03-36.csv",head=T)

social$Social.distress
social$Change.in.brain.activity


plot(social$Social.distress,social$Change.in.brain.activity,      
     xlab = "Social Distress",
     ylab = "Brain Activity",
     main = "Social Exclusion vs Brain Activity",
     pch=19, col="blue")

# correlation coefficient 
cor_coeff <- cor(social$Social.distress,social$Change.in.brain.activity)
cor_coeff

## Mean and Standard deviations for Nitrates and Absorbance observations
mean(social$Social.distress)
sd(social$Social.distress)

mean(social$Change.in.brain.activity)
sd(social$Change.in.brain.activity)

### Slope

## y_hat = a + bx
## b = r * sy/sx
## a = y_hat - bx

# Finding Slope
b_val_Slope <- cor_coeff * (sd(social$Change.in.brain.activity)/sd(social$Social.distress))

b_val_Slope

# Finding a --
a_val <- mean(social$Change.in.brain.activity) - b_val_Slope * mean(social$Social.distress)
a_val 

# equation for the regression line --
#Pred_y <- a_val + b_val_Slope * pred_x

Pred_y <- a_val + b_val_Slope * 2

Pred_y

# finding predicted y for a given x
# all regression lines pass trhough
# (mean_x,mean_y), so let's find y_hat
# for mean_x and check if it equals 
# mean_y.

y_hat = a_val + b_val_Slope * mean(social$Social.distress)
y_hat

y_hat == mean(social$Change.in.brain.activity)

# plotting the regression line --
abline(a_val,b_val_Slope,col="green",lwd=4)

