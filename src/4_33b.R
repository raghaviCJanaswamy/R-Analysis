######. Exercise 4.33 b

#### set a

set_a <- read.csv("/data/ta04-01a.csv",head=T)

plot(set_a$x,set_a$y,      
     xlab = "x",
     ylab = "y",
     main = "Data Set A",
     pch=19, col="blue")

data_lm <- lm(set_a$y ~ set_a$x)

# slope and intercept
coef(data_lm) 

# adds regr. line
abline(data_lm, col="dark green",lwd=1) 

# predicted y-values for all xs
fitted(data_lm)   

#### set b

set_a <- read.csv("/data/ta04-01b.csv",head=T)

plot(set_a$x,set_a$y,      
     xlab = "x",
     ylab = "y",
     main = "Data Set B",
     pch=19, col="blue")

data_lm <- lm(set_a$y ~ set_a$x)

# slope and intercept
coef(data_lm) 

# adds regr. line
abline(data_lm, col="dark green",lwd=1) 

# predicted y-values for all xs
fitted(data_lm)

#### set c

set_a <- read.csv("/data/ta04-01c.csv",head=T)

plot(set_a$x,set_a$y,      
     xlab = "x",
     ylab = "y",
     main = "Data Set C",
     pch=19, col="blue")

data_lm <- lm(set_a$y ~ set_a$x)

# slope and intercept
coef(data_lm) 

# adds regr. line
abline(data_lm, col="dark green",lwd=1) 

# predicted y-values for all xs
fitted(data_lm)


#### set d

set_a <- read.csv("/data/ta04-01d.csv",head=T)

plot(set_a$x,set_a$y,      
     xlab = "x",
     ylab = "y",
     main = "Data Set D",
     pch=19, col="blue")

data_lm <- lm(set_a$y ~ set_a$x)

# slope and intercept
coef(data_lm) 

# adds regr. line
abline(data_lm, col="dark green",lwd=1) 

# predicted y-values for all xs
fitted(data_lm)
