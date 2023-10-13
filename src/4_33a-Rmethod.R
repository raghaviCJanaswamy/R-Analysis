######. Exercise 4.33 a - R functions

#### set d

set_d <- read.csv("/data/ta04-01d.csv",head=T)

data_lm <- lm(set_d$y ~ set_d$x)

# slope and intercept
coef(data_lm) 



