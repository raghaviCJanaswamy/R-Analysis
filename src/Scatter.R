
mydata <- read.csv("/Users/ragavahini/0_BioInformatics_Educations/00_Coursework/ABT-720/Book_Exercises/3.21/ex03-21.csv",head=T) 


#mydata <- read.table(file.choose(), header = T)
names(mydata)
dim(mydata)
plot(mydata$Grazers, mydata$Net.growth,
     main="Plot Grazers vs Net growth",
     xlab="Number of Grazers ",
     ylab = "Net Growth Rate",
     pch = 19,
     col = "blue")

# Mean of Grazers
mean(mydata$Grazers)
# Mean of Net Growth
mean(mydata$Net.growth)

# Standard Deviation of Net Growth
sd(mydata$Net.growth)

# Standard Deviation of Grazers
sd(mydata$Grazers)

# Correlation
cor(mydata$Grazers, mydata$Net.growth)

