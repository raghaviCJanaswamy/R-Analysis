
# 17.42 a

radioactive <- read.csv("/data/ex17-42.csv",head=T)
radioactive

ggplot(radioactive, aes(x = Cesium)) +                           
  geom_dotplot(color="black", fill="orange") +
  labs(title = "Radio Active Cesium in Dry tissue of Sea LivingBeings ",
       y = "Cesium",
       x = "Dry tissue")

# 17.42 b

# 17.37 b
treatment.n = length(radioactive$Cesium)
treatment.n

treatment.mean <- mean(radioactive$Cesium)
treatment.mean

treatment.sd <- sd(radioactive$Cesium)
treatment.sd

### Standard error

treatment.se <- treatment.sd/sqrt(treatment.n)
treatment.se

# Calculate the mean and standard error
l.model <- lm(treatment$Days ~ 1, treatment)

# Calculate the confidence interval
confint(l.model, level=0.90)