######. Exercise 4.33 a

#### a)

set_a <- read.csv("/data/ta04-01d.csv",head=T)

# correlation coefficient 
cor_coeff <- cor(set_a$x,set_a$y)
cor_coeff

## Mean and Standard deviations for x and y
mean(set_a$x)
sd(set_a$x)

mean(set_a$y)
sd(set_a$y)

# Finding Slope
b_val_Slope <- cor_coeff * (sd(set_a$y)/sd(set_a$x))
b_val_Slope

# Finding a --
a_val <- mean(set_a$y) - b_val_Slope * mean(set_a$x)
a_val 

# equation for the regression line --
#Pred_y <- a_val + b_val_Slope * pred_x

Pred_y <- a_val + b_val_Slope * 10
Pred_y




