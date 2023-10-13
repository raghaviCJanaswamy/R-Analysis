library("dplyr")
library("ggplot2")
# 1.	If a person is selected at random, what is the probability that he or she will have an IQ below 108? (use R to answer questions 1-17; n.b., IQ tests routinely have a mean of 100 and a standard deviation of 15)

mu <- 100   
sd <- 15    
x <- 108     

#Probability is 
prob <- pnorm(x, mu, sd)
prob

# 2.	If a person is selected at random, what is the probability that he or she will have an IQ above 108?

compProb <- 1 - prob
compProb

# 3.	If a person is selected at random, what is the probability that his or her IQ will fall between 95 and 108?

x1 <- 95
x2 <- 108


prob_diff <- pnorm(x2, mu, sd) - pnorm(x1, mu, sd)
  
prob_diff
# 4.	If 10 people are selected at random, what is the probability that the mean of their IQs will be above 108?

n <- 10
sem <- sd/sqrt(n)

z_score <- (108 - mu)/sem

probabilty_iq_above108 <- 1 - pnorm(z_score)
probabilty_iq_above108


# 5.	If 10 people are selected at random, what is the probability that the mean of their IQs will fall between 95 and 108?

x1 < 95
x2 <- 108

z_score_1 <- (x1-mu) /sem
z_score_2 <- (x2-mu) /sem

probabilty_iq_above95 <- pnorm(z_score_1)
probabilty_iq_below108 <- pnorm(z_score_2)

Probability_95_108 <- probabilty_iq_below108 - probabilty_iq_above95

Probability_95_108

# 6.	You are testing a hypothesis Ho: μ = 10 against Ha: μ < 10 based on a SRS of 20 observations from a Normal population. 
#The data give sample mean of 8 and a sample standard deviation of 4. Find the value of t. 
#(use R and the numbers provided to answer this question – do not create simulated data)

#Ho: μ = 10
#Ha: μ < 10

sample_mean <- 8
sample_sd <- 4
sample_size <- 20
hypothized_mean <- 10

### Standard Error

standard_error <- sample_sd / sqrt(sample_size)
t_score <- (sample_mean - hypothized_mean)/standard_error


t_score

# 7.	What is the area to the left of the t value obtained in question 6?

degree_of_freedom = sample_size -1 

prob_left = pt(t_score, df=degree_of_freedom)

prob_left

# 8.	Use R to compare the p-value obtained in question 7 to a significance level of 0.05 (hint: Ask R if one value is greater than (>) the other. The answer should be TRUE or FALSE.).

significance_level <- 0.05

p_value <- pt(t_score, df = degree_of_freedom, lower.tail = TRUE)
p_value

if (p_value < significance_level) {
  print("FALSE")
} else {
  print("TRUE")
}

# 9.	Use the MHEALTH data to test the hypothesis that men have an average cholesterol level of 300. 

mhealth <- read.table("/data/MHEALTH.txt",head=T)
mu_chol_men <- mean(mhealth$CHOL)

mu_chol_men
t.test(mhealth$CHOL, mu=300)
mhealth$CHOL

# 10.	Use the FHEALTH data to test the hypothesis that women have an average cholesterol level of 300.
fhealth <- read.table("/data/FHEALTH.txt",head=T)
mu_chol_women <- mean(fhealth$CHOL)

mu_chol_women
t.test(fhealth$CHOL, mu=300)

# 11.	Do men have significantly higher cholesterol levels than women?
t_result <- t.test(mhealth$CHOL, fhealth$CHOL)
t_result
if (t_result$p.value < 0.05) {
  print( "Men have significantly higher cholesterol level than women")
} else {
  print( "There is no significant difference between and men and women cholestrol levels")
}

# 12.	Do men have significantly different diastolic blood pressures than women?
t_result_diastolic <- t.test(mhealth$DIAS, fhealth$DIAS)

if (t_result_diastolic$p.value < 0.05) {
  print( "Men have significantly higher diastolic blood pressure levels than women")
} else {
  print( "There is no significant difference between and men and women with the diastolic blood pressure")
}


# 13.	Are women significantly shorter than men? 
t_result_height <- t.test(mhealth$HT, fhealth$HT)

t_result_height
if (t_result_height$p.value < 0.05) {
  print( "Woman are significantly shorter than men")
} else {
  print( "There is no significant difference between and men and women height")
}

# 
# 14.	Students who performed poorly on a City of New York Skills Assessment Test were enrolled in a summer math program. Scores of 10 students before and after the summer program were recorded. They were:
# Before = 18,18,21,18,18,20,23,23,21,17
# After    = 24,25,33,29,33,36,34,36,34,27
# (Scores for individual students are in the same order in each list.)
# Did the NYC summer math program improve student performance significantly?

before <- c(18,18,21,18,18,20,23,23,21,17)
after <- c(24,25,33,29,33,36,34,36,34,27)

t_nyc_result <- t.test(after, before )

t_nyc_result

if (t_nyc_result$p.value < 0.05) {
  print( "NYC Summer math program significantly improved the student performance")
} else {
  print( "NYC Summer math program didnot show any significant improvement ")
}
# 15.	How many points did student scores improve on average? (hint: consult the output from question 14; it’s OK to simply write this answer – it did come from R output.)
## The mean difference from  #14 is 

31.1-19.7

diff <- after - before
average_diff = mean(diff)

average_diff

# 16.	What is the 95% confidence interval for the parameter of average score improvement? (hint: consult the output from question 14; it’s OK to simply write this answer – it did come from R output.)

  print("95 percent confidence interval:")
  print(" 9.132821 13.667179")

# 17.	Download the ozone dataset and import it into R. (hint: use the proper strategy to import a file of this format.)
oszone <- read.csv("/data/ozone.csv",head=T, stringsAsFactors = FALSE)
oszone$Ozone
oszone$Garden

# 18.	Partition the data into two data sets, one for observations from Garden A and the other for observations from Garden B.

garden_a <- oszone %>%
  filter(Garden == 'A') 
print(garden_a)  

garden_b <- oszone %>%
  filter(Garden == 'B') 
print(garden_b)  

# 19.	Create 2 separate histograms with equal sized x-axes that are displayed together in a single figure. Color the histogram for Garden A goldenrod1 and the histogram for Garden B cornflowerblue. Label the x-axes “Garden A – Ozone” and “Garden B – Ozone” and provide one suitable title for the whole figure (with no title over the lower histogram).

# Example data
data <- data.frame(
  Ozone = c(oszone$Ozone),
  Garden = c(oszone$Garden)
)

# Set the colors
color_A <- "goldenrod1"
color_B <- "cornflowerblue"

# Create the figure
ggplot() +
  geom_histogram(data = subset(data, Garden == "A"), aes(x = Ozone), fill = color_A, color = "black") +
  geom_histogram(data = subset(data, Garden == "B"), aes(x = Ozone), fill = color_B, color = "black") +
  labs(x = "", y = "Frequency", title = "Ozone Levels in Gardens A and B") +
  scale_x_continuous(breaks = seq(1, 7, by = 1), labels = c("1", "2", "3", "4", "5", "6", "7"),
                     expand = c(0.02, 0)) +
  facet_grid(Garden ~ ., scales = "free_x", switch = "x") +
  theme_minimal()



# 20.	Test whether the ozone levels differ significantly between the two Gardens.
t_result_ozone <- t.test(garden_a$Ozone, garden_b$Ozone)

t_result_ozone
if (t_result_ozone$p.value < 0.05) {
  print( "The ozone levels differ significantly between Garden A and Garden B")
} else {
  print( "There is no significant difference in the ozone between Garden A and Garden B")
}


