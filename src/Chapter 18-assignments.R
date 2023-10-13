library("ggplot2")
library("dplyr")

#### 18.26 (a) 

csv_data <- read.csv("/data/ex18-26.csv",head=T)

# Create a data frame to store the dot plot data
dotplot_data <- data.frame(
  X = rep(1:ncol(csv_data), each = nrow(csv_data)),
  Value = as.vector(csv_data)
)

color_A <- "goldenrod1"
color_B <- "cornflowerblue"

ggplot() +
    geom_histogram(data = subset(dotplot_data, X == "1"), aes(x = Value.Obese), fill = color_A, color = "black") + 
    geom_histogram(data = subset(dotplot_data, X == "2"), aes(x = Value.Healthy.weight), fill = color_B, color = "black") +
    labs(x = "", y = "Testosterone", title = "Testosterone levels between healthy Male  vs Obese Male by weight") +
   facet_grid(X ~ ., scales = "free_x", switch = "x") +
   theme_minimal()
 

#### b) 

t_result <- t.test(csv_data$Obese, csv_data$Healthy.weight)
t_result

t_result_ozone
if (t_result_ozone$p.value < 0.05) {
  print( "The ozone levels differ significantly between Garden A and Garden B")
} else {
  print( "There is no significant difference in the ozone between Garden A and Garden B")
}




#### 18.29 

mg_1_mean <- 76
mg_1_sd <- 13
mg_1_sample <- 32


mg_2_mean <- 156
mg_2_sd <- 42
mg_2_sample <- 32

### Hypothesis is Higher dose results in higher concentration
hypothized_mean <- 10


mu = mg_1_mean - mg_2_mean
sd = sqrt(mg_1_sd^2/mg_1_sample + mg_2_sd^2/mg_2_sample)

t_result <- mu/sd

t_result

## Degree of freedome

df <- ((mg_1_sd^2 / mg_1_sample) + (mg_2_sd^2 / mg_2_sample))^2 / (((mg_1_sd^2 / mg_1_sample)^2 / (mg_1_sample - 1)) + ((mg_2_sd^2 / mg_2_sample)^2 / (mg_2_sample - 1)))

df

## p-value

p_value <-  2 * (1 - pt(abs(t_result), df))
p_value


#18.30

n1 <- 32
n2 <- 32

mean1 <- 3.16
mean2 <- 3.15

sd1 <- 0.72
sd2 <- 0.39

# Perform the t-test
result <- t.test(x = rnorm(n1, mean1, sd1), y = rnorm(n2, mean2, sd2), alternative = "two.sided")

result

# Get the p-value from the t-test result
p_value <- result$p.value

# Print the p-value
print(p_value)


#18.39 a)


csv_data <- read.csv("/data/ta18-05.csv",head=T)

# Filter the data by the control variable
group_c_data <- csv_data %>%
  filter(Group == "C")

group_t_data <- csv_data %>%
  filter(Group == "T")


# Perform the t-test for the values at week 0.

data_c_0 <- c(group_c_data$Week.0)
data_t_0 <- c(group_t_data$Week.0)

t_result_before <- t.test(data_c_0, data_t_0 )
t_result_before


### b)

mean_c_0 <- mean(group_c_data$Week.0)
sd_c_0 <- sd(group_c_data$Week.0)

mean_c_0
sd_c_0

mean_t_0 <- mean(group_t_data$Week.0)
mean_t_0
sd_t_0 <- sd(group_t_data$Week.0)
sd_t_0


#Test stats after the ink applications

csv_data <- read.csv("/data/ta18-05.csv",head=T)

# Filter the data by the control variable
group_c_data <- csv_data %>%
  filter(Group == "C")

group_t_data <- csv_data %>%
  filter(Group == "T")

data_c_1 <- c(group_c_data$Week.1)
data_t_1 <- c(group_t_data$Week.1)

t_result_after <- t.test(data_c_1, data_t_1 )
t_result_after


## 18.40 a)

mean_m <- 5.2
sd_m <- 3.2
n_m <- 29

mean_p <- 1.1
sd_p <- 1.6
n_p <- 21

# Perform the t-test
result <- t.test(x = rnorm(n_m, mean_m, sd_m), y = rnorm(n_p, mean_p, sd_p), alternative = "two.sided")
result


##### b)

mean <- 5.2
sd <- 3.2
n <- 29


# Calculate the margin of error
marginOfError <- qnorm(0.975) * (sd / sqrt(n))

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- mean - marginOfError
upper_bound <- mean + marginOfError

# Compute the mean at 95% confidence level
mean_conf <- c(lower_bound, upper_bound)

mean_conf


### 18.41 a)

## Magnetic device vs placebo

mean_m <- 9.6
sd_m <- 0.7
n_m <- 29

mean_p <- 9.5
sd_p <- 0.8
n_p <- 21

# Perform the t-test
result <- t.test(x = rnorm(n_m, mean_m, sd_m), y = rnorm(n_p, mean_p, sd_p), alternative = "two.sided")
result





