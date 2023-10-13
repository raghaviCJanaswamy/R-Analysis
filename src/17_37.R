
# 17.37 a

treatment <- read.csv("/data/ex17-37.csv",head=T)

ggplot(treatment, aes(x = treatment$Days)) +                           
  geom_dotplot(color="black", fill="orange") +
  labs(title = "New cell Multiplication Rate - Cancer treatmnet ",
       x = "Days",
       y = "Cell rate")

# 17.30 b
treatment.n = length(treatment$Days)
treatment.n

treatment.mean <- mean(treatment$Days)
treatment.mean

treatment.sd <- sd(treatment$Days)
treatment.sd

### Standard error

treatment.se <- wine.sd/sqrt(wine.n)
treatment.se

# Calculate the mean and standard error
l.model <- lm(treatment$Days ~ 1, wine)

# Calculate the confidence interval
confint(l.model, level=0.95)


