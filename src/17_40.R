
# 17.40 a

calabsorption <- read.csv("/data/ex17-40.csv",head=T)
calabsorption



ggplot(calabsorption, aes(x = Oligofructose-Control)) +                           
  geom_dotplot(color="black", fill="red") +
  labs(title = "Oligofructose and Calcium Absorption ",
       x = "Difference",
       y = "Count")

# 17.40 b

calabsorption_diff <- c(rnorm(calabsorption$Oligofructose-calabsorption$Control, mean=mean(calabsorption$Oligofructose-calabsorption$Control), sd=sd(calabsorption$Oligofructose-calabsorption$Control)))

t.test(calabsorption_olgi,calabsorption_Control,   paired = FALSE)
