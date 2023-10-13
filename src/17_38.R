
# 17.38 a

cells <- read.csv("/data/ex17-38.csv",head=T)

ggplot(cells, aes(x = Difference)) +                           
  geom_dotplot(color="black", fill="green") +
  labs(title = "Cell count difference after infusion ",
       x = "Difference",
       y = "Days")

# 17.37 b
treatment.n = length(treatment$Days)
treatment.n

treatment.mean <- mean(treatment$Days)
treatment.mean

treatment.sd <- sd(treatment$Days)
treatment.sd

### Standard error

treatment.se <- treatment.sd/sqrt(treatment.n)
treatment.se

# Calculate the mean and standard error
l.model <- lm(treatment$Days ~ 1, treatment)

# Calculate the confidence interval
confint(l.model, level=0.90)


