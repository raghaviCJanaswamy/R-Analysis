
plasticweight <- read.csv("/data/ex04-26.csv",head=T)

plot(plasticweight$Time..days.,plasticweight$Weight.lost..mg.,      
     xlab = "Time (Days)",
     ylab = "Weight Lost (mg)",
     main = "Plastic weight loss over time in a Bacteria solution",
     pch=19, col="dark orange")

# How does the correlation coefficient look? --
cor_coeff <- cor(plasticweight$Time..days., plasticweight$Weight.lost..mg.)
cor_coeff

## Mean and Standard deviations for Nitrates and Absorbance observations
mean(plasticweight$Time..days.)
sd(plasticweight$Time..days.)

mean(plasticweight$Weight.lost..mg.)
sd(plasticweight$Weight.lost..mg.)

### Slope

## y_hat = a + bx
## b = r * sy/sx
## a = y_hat - bx
 
# Finding Slope
b_val_Slope <- cor_coeff * (sd(plasticweight$Weight.lost..mg.)/sd(plasticweight$Time..days.))

b_val_Slope

# Finding a --
a_val <- mean(plasticweight$Weight.lost..mg.) - b_val_Slope * mean(plasticweight$Time..days.)
a_val 

# equation for the regression line --
### Pred_y <- a_val + b_val_Slope * tobe_X

Pred_y <- a_val + b_val_Slope * 100
Pred_y




