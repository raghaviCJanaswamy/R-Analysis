library("ggplot2")
library("dplyr")

#### 18.26 (a) 

Rat_Chow <- read.table("/data/Rat_Chow.txt",head=T)
Rat_Chow
Rat_Wts = c(Rat_Chow$Chow_Only,Rat_Chow$Chow_1caf,Rat_Chow$Chow_24caf)
Rat_Wts
Access = c(rep("Caf_No",15),rep("Caf_1hr",15),rep("Caf_24hr",15))
Access
Rat_Study = data.frame(Rat_Wts,Access)
Rat_Study

plot(Rat_Wts ~ Access, data=Rat_Study, col=c("dark green","dark red", "dark blue"), main="Comparison of Rat Weights")

sd(Rat_Chow$Chow_Only) [1]  
sd(Rat_Chow$Chow_1caf) [1] 
sd(Rat_Chow$Chow_24caf)

33.64*2

my_results = aov(Rat_Wts ~ Access, data=Rat_Study)
my_results

anova(my_results)
TukeyHSD(my_results,conf.level = 0.95)


chi.data = matrix(c(115,30,85,70), 2, 2, byrow=TRUE,dimnames=list(c("Black","Red"), c("Industrial","Rural")))
chi.data


barplot(chi.data, beside=T, col=c("Black","Red"),
        ylim=c(0,125),legend=T)

chisq.test(chi.data)


############ Exercises #############

## 21.21 d)

total_sample <- 787+277
observed <- c(787,277)
expected <- c(total_sample * 0.75, total_sample * 0.25)

#chi_square <- chisq.test(matrix(c(peaexperiment_dominant, peaexperiment_expected), nrow=2))
observed
expected

chi_square <- chisq.test(observed, p = expected / sum(expected))
chi_square

# Stats

cat("Chi Statistic:  ", chi_square$statistic)




## 21.24

# Given data 
total_sample <- 5 + 100
observed <- c(5, 100)
expected <- c(total_sample * 3/64, total_sample * 61/64)

observed
expected

# Calculate the chi-square test statistic manually
chi_square_statistic <- sum((observed - expected)^2 / expected)
chi_square_statistic

chi_square <- chisq.test(observed, p = expected / sum(expected))
chi_square


## 21.25
# Given data. a) 

total_sample <- 87
observed <- c(0,87)
expected <- c(13.2,73.8)

observed
expected

# Calculate the chi-square test statistic manually
chi_square_statistic <- sum((observed - expected)^2 / expected)
chi_square_statistic

chi_square <- chisq.test(observed, p = expected / sum(expected))
chi_square

# Given data. b) 

total_sample <- 35
preg <- c(6, 29)
nopreg <- c(7.1,27.9)

preg
nopreg

# Calculate the chi-square test statistic manually
chi_square_statistic <- sum((preg - nopreg)^2 / nopreg)
chi_square_statistic


chi_square <- chisq.test(preg, p = nopreg / sum(nopreg))
chi_square

#####  21.29



# Install and load the dplyr package
#install.packages("dplyr")
#library(dplyr)
library(ggplot2)

mydata <- read.csv("/data/ex21-29.csv",head=T)

#  the data frame
ethnicity_data <- data.frame(
  Ethnicity = c(mydata$Ethnicity),
  Count_in_study = c(mydata$Count.in.study),
  Percent_in_US = c(mydata$Percent.in.U.S.)
)
ethnicity_data$Count_in_study <- as.numeric(gsub(",", "", ethnicity_data$Count_in_study))
total_count <- sum(ethnicity_data$Count_in_study)

# the chi-square goodness-of-fit test
observed_counts <- ethnicity_data$Count_in_study
expected_counts <- sum(observed_counts) * (as.numeric(gsub("%", "", ethnicity_data$Percent_in_US)) / 100)

chi_square_statistic <- sum((observed_counts - expected_counts)^2 / expected_counts)
chi_square_statistic

chi_square <- chisq.test(observed_counts, p = expected_counts / sum(expected_counts))
chi_square


## 21.34


# Install and load the dplyr package
#install.packages("dplyr")
#library(dplyr)
library(ggplot2)

total_sample <- 102


observed <- c(25,45,32)
expected <- c(102*1/4, 102*2/4, 102*1/4)
df <- 2

observed
expected

chi_square <- chisq.test(observed, p = expected/ sum(expected))
chi_square

# Stats

cat("Chi Statistic:  ", chi_square$statistic)




 


######

install.packages("ggplot2")  # Install the ggplot2 package if not already installed
library(ggplot2)            # Load the ggplot2 package

# Assuming you have already defined the data as a data frame:
data <- data.frame(
  Group = c("Control", "Hornworm", "Lead bug", "Flea beetle"),
  EmissionRate = c(9.22, 31.03, 18.97, 27.12)
)

mean_emission <- aggregate(EmissionRate ~ Group, data, mean)

# Create the bar plot
barplot <- ggplot(mean_emission, aes(x = Group, y = EmissionRate)) +
  geom_bar(stat = "identity", fill = "green") +
  ylab("Mean Emission Rate") +
  xlab("Group") +
  ggtitle("Comparison of Mean Emission Rates for Four Groups")

# Display the bar plot
print(barplot)

n <- 8
sqrt(n)


# 24.29  a) 

library(ggplot2)
biomass <- read.csv("/data/ex24-29.csv",head=T)

data <- data.frame(
  Treatment = c(rep("winter", 6), rep("spring", 6), rep("control", 6)),
  Biomass = c(biomass$Biomass)
)

mean_biomass <- aggregate(Biomass ~ Treatment, data, mean)
sd_biomass <- aggregate(Biomass ~ Treatment, data, sd)


ggplot(data, aes(x = Treatment, y = Biomass, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Treatment", y = "Biomass", title = "Biomass for Different Treatments") +
  geom_text(aes(label = round(Biomass, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_minimal()

mean_biomass
sd_biomass


######  C) Anova

model <- aov(Biomass ~ Treatment, data = data)
summary(model)
anova_results <- summary(model)
test_statistic <- anova_results[[1]]$F[1]
p_value <- anova_results[[1]]$"Pr(>F)"[1]

test_statistic
p_value
 

#### 24.30 a)


mydata <- read.csv("/data/ex24-30.csv",head=T)
# Enter the data into a data frame
data_0 <- data.frame(
  ACEA = c(mydata$ACEA),
  Naloxone = c(mydata$Naloxone),
  ACEA_plus_naloxone = c(mydata$ACEA.plus.naloxone),
  Control = c(mydata$Control)
)

#Convert to single-column data frame
single_column_df <- gather(data_0, key = "Treatment", value = "Value", ACEA, Naloxone, ACEA_plus_naloxone, Control)

data <- single_column_df.frame(
  Treatment = c(rep("ACEA", 6), rep("Naloxone", 6), rep("ACEA_plus_naloxone", 6), rep("Normal", 6)),
  Value = c(single_column_df$Value)
)

ggplot(single_column_df, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Treatment", y = "", title = "Receptor function for Different Treatments") +
  theme_minimal()

##########

model <- aov(Value ~ Treatment, data = single_column_df)
summary(model)
anova_results <- summary(model)
test_statistic <- anova_results[[1]]$F[1]
p_value <- anova_results[[1]]$"Pr(>F)"[1]

test_statistic
p_value



#### 24.31
library(ggplot2)
bacteria <- read.csv("/data/ex24-31.csv",head=T)
data <- data.frame(
  Ph = c(rep("winter", 6), rep("spring", 6), rep("control", 6)),
  Relatfit = c(bacteria$relatfit)
)

mean_1 <- aggregate(Relatfit ~ Ph, data, mean)
sd_1 <- aggregate(Relatfit ~ Ph, data, sd)


ggplot(data, aes(x = Ph, y = Relatfit, fill = Ph)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Ph", y = "Relatfit", title = "Relatfit for different Ph") +
  geom_text(aes(label = round(Relatfit, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_minimal()

mean_1
sd_1


######  C) Anova

model <- aov(Relatfit ~ Ph, data = data)
summary(model)
anova_results <- summary(model)
test_statistic <- anova_results[[1]]$F[1]
p_value <- anova_results[[1]]$"Pr(>F)"[1]

test_statistic
p_value

#####



# Install and load the dplyr package
#install.packages("dplyr")
#library(dplyr)
library(ggplot2)

mydata <- read.csv("/data/Large_Fruitfly.csv",head=T)


data <- data.frame(
  Companion_type = c(mydata$Companion.type),
  Number_of_companions = c(mydata$Number.of.companions),
  Thorax_length_mm = c(mydata$Thorax.length..mm.), 
  Lifespan_days = c(mydata$Lifespan..days.)
)

#ggplot(data_0, aes(x = Companion_type, y = LifeSpan, fill = Companion_type)) +
#geom_bar(stat = "identity", position = "dodge") +
#labs(x = "Sex condition", y = "", title = "Lifespans lengths by Sex condition") +
#theme_minimal()



# Example:
# One-way ANOVA for Lifespan_days
anova_lifespan <- aov(Lifespan_days ~ Number_of_companions + Companion_type, data = data)

# One-way ANOVA for Thorax_length_mm
anova_thorax <- aov(Thorax_length_mm ~ Number_of_companions + Companion_type, data = data)

# Step 3: Extract the ANOVA results
summary(anova_lifespan)
summary(anova_thorax)















