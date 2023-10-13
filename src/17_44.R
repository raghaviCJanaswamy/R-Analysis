 

absorption <- read.csv("/data/ta17-04.csv",head=T)
 

# 17.44

absorption.n = length(absorption$Generic-absorption$Reference)
absorption.n

absorption.mean <- mean(absorption$Generic-absorption$Reference)
absorption.mean

absorption.sd <- sd(absorption$Generic-absorption$Reference)
absorption.sd

### Standard error

absorption.se <- absorption.sd/sqrt(absorption.n)
absorption.se

# Calculate the mean and standard error
l.model <- lm(absorption$Generic-absorption$Reference ~ 1, absorption)

# Calculate the confidence interval
confint(l.model, level=0.95)

drug_diff <- c(rnorm(absorption$Generic-absorption$Reference, mean=absorption.mean, sd=absorption.sd))

Generic_drug <- c(rnorm(absorption$Generic, mean=mean(absorption$Generic), sd=sd(absorption$Generic)))
reference_drug <- c(rnorm(absorption$Reference, mean=mean(absorption$Reference), sd=sd(absorption$Reference)))


t.test(Generic_drug,reference_drug,   paired = TRUE)


