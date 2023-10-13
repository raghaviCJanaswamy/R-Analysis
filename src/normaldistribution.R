# 

pnorm(0)
pnorm(1)
pnorm(-1)


pnorm(0,1,1)
pnorm(0,1,1)



pnorm(200, mean=222, sd=37, lower.tail=TRUE) 
pnorm(240, mean=222, sd=37, lower.tail=TRUE) 
pnorm(1500, mean = 2750, sd = 560)


library("dplyr")
library(ggplot2)


largefev <- read.csv("/data/Large_FEV.csv",head=T)

girlfev <- largefev %>%
  filter(sex == 0 & age >= 14) 
 
boyfev <- largefev %>%
  filter(sex == 1 & age >= 14 ) 

#create Q-Q plot for Girls
ggplot(girlfev ,
       aes(sample=fev), main = "Birth Weight (in grams)") +
  stat_qq() + 
  stat_qq_line(col = "blue") +
  ggtitle("FEV for girls for age Above 14") 



#create Q-Q plot for Boys
ggplot(boyfev, aes(sample=fev)) +
  stat_qq() + 
  stat_qq_line( col = "red") +
  ggtitle("FEV for boys for age Above 14") 

