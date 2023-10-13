install.packages("multcompView")
install.packages("dplyr")
install.packages("titanic")
library(stats)
library(multcompView)
library(titanic)
install.packages("car")
library(dplyr)

###. #27.36


mydata <- read.csv("/data/ex27-36.csv",head=T)


data <- data.frame(
  yeast = c(mydata$yeast),
  eggs = c(mydata$eggs)
)

# Calculate the mean reproductive output for each diet richness level
mean_reproductive_output <- data %>%
  group_by(yeast) %>%
  summarize(mean_output = mean(eggs))

library(ggplot2)

# Create a bar plot to visualize the mean reproductive output for each diet richness level
ggplot(mean_reproductive_output, aes(x = factor(yeast), y = mean_output)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Reproductive Output vs. Diet Richness",
       x = "Diet Richness",
       y = "Mean Reproductive Output")


## ONE WAY ANOVA

anova_result <- aov(eggs ~ factor(yeast), data = data)
summary(anova_result)

# Perform Tukey's HSD test
hsd_result <- TukeyHSD(anova_result)

# Print the post-hoc test results
print(hsd_result)


## 27.37

mydata <- read.csv("/data/ex27-37.csv",head=T)

# Create a data frame with the provided data
data <- data.frame(
  Pre = c(mydata$Pre),
  Post = c(mydata$Post),
  Diff = c(mydata$Diff)
)

# Perform a paired t-test to compare the ELISA count before and after the infusion
ttest_result <- t.test(data$Post, data$Pre, paired = TRUE)

# Print the t-test result
print(ttest_result)


## 27.39
 
mydata <- read.csv("/data/ex27-39.csv",head=T)


data <- data.frame(
  nems = c(mydata$nems),
  growth = c(mydata$growth)
)

# Load the necessary library for ANOVA
library(stats)
 
# Perform the Kruskal-Wallis test to compare the growth among different nematode treatments
kruskal_test_result <- kruskal.test(growth ~ nems, data = data)

# Print the Kruskal-Wallis test result
print(kruskal_test_result)



### 27.42


# Load the necessary library for plotting
library(ggplot2)
library(titanic)
library(car)

mydata <- read.csv("/data/ex27-42.csv",head=T)
mydata
# Create a data frame with the provided data
data <- data.frame(
  Vaccine1 = c(mydata$Vaccine1),
  Vaccine2 = c(mydata$Vaccine2),
  Vaccine3 = c(mydata$Vaccine3),
  Vaccine4 = c(mydata$Vaccine4),
  Sham = c(mydata$Sham)
)
data

# melt the data to be long for boxplot
data_long <- reshape2::melt(data)

# plot boxplot
ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Antibody Production by Vaccine variants",
       x = "Vaccine",
       y = "Antibodies")

# Perform the ANOVA test to compare the antibody productions among different vaccine variants
anova_result <- aov(Vaccine1 + Vaccine2 + Vaccine3 + Vaccine4 + Sham ~ 1, data = data)
anova_result

# Extract the Vaccine2 data
vaccine2_data <- data$Vaccine2
vaccine2_data

# Perform the Kruskal-Wallis test on Vaccine2
kruskal_test_result <- kruskal.test(vaccine2_data, data)

kruskal_test_result




# Print the Kruskal-Wallis test result
print(kruskal_test_result)



#### 27.43

mydata <- read.csv("/data/ex27-43.csv",head=T)
# Create a data frame with the provided data
data <- data.frame(
  group = c(rep(1, 39), rep(2, 33)),
  mass = c(mydata$mass)
)

# Load the necessary library for t-test
library(stats)

# Perform an independent samples t-test to compare seed masses between the two groups
ttest_result <- t.test(mass ~ group, data = data)

# Print the t-test result
print(ttest_result)



#### 27.45

mydata <- read.csv("/data/ex27-45.csv",head=T)

# Create a data frame with the provided data
data <- data.frame(
  Smoke = c(rep("Active", 10), rep("Passive", 10), rep("Neither", 10)),
  Cotinine = c(mydata$Cotinine)
)

# Load the necessary library for plotting
library(ggplot2)

# Create a boxplot to visualize the cotinine levels for each smoke group
ggplot(data, aes(x = Smoke, y = Cotinine)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cotinine Levels by Smoker Group",
       x = "Smoke Group",
       y = "Cotinine Levels (ng/mL)")

# Calculate the sample standard deviation for each smoke group
standard_deviations <- data %>%
  group_by(Smoke) %>%
  summarise(sample_std_dev = sd(Cotinine))
standard_deviations

# Kruskal-Wallis test
kruskal.test(Cotinine ~ Smoke, data = data)







