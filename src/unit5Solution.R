install.packages("gridExtra")
install.packages("MASS")
library(ggplot2)
library(multcomp)
library(stats)
library(gridExtra)
library(dplyr)

# 1.	Create three vectors (see Cats, Dogs, and Cows, below) and combine them into a matrix called animals, where each list becomes a row:
#   Cats   = 2,2,4
#   Dogs  = 1,2,1
#   Cows = 30,35,41


Cats <- c(2, 2, 4)
Dogs <- c(1, 2, 1)
Cows <- c(30, 35, 41)
animals <- rbind(Cats, Dogs, Cows)

#   2.	Add column names of “Farm A”, Farm B”, and “Farm C”.

colnames(animals) <- c("Farm A", "Farm B", "Farm C")

animals
#   3.	Transpose the rows and columns in animals and store the result to a matrix called farms.
farms <- t(animals)

#   4.	Display the contents of farms.
farms

#   5.	A study of 543 African children assessed the presence or absence of an allele for sickle cell anemia and the presence or absence of heavy malarial infection. The data are summarized below:
#     Sickle-Cell Allele	No Sickle-Cell Allele
#   Heavy Malaria – Yes	36	152
#   Heavy Malaria - No	100	255
#   Generate two side-by-side bar plots in a single figure to graphically illustrate the proportions of heavy malarial infections (color the bars for present black, and those for absent absent red) for people who do and do not have the sickle-cell allele.


data <- matrix(c(36, 100, 152, 255), nrow = 2, byrow = TRUE)
rownames(data) <- c("Yes", "No")
colnames(data) <- c("Sickle-Cell Allele", "No Sickle-Cell Allele")



#   6.	Do these data suggest a significant association between the sickle-cell allele and susceptibility to heavy malarial infection?

barplot(data, beside = TRUE, col = c("black", "red"), xlab = "Sickle-Cell Allele",
        ylab = "Count of Heavy Malaria Infections",
        main = "Proportions of Heavy Malaria Infections by Sickle-Cell Allele",
        legend.text = rownames(data), args.legend = list(x = "topleft"))

legend("topright", legend = colnames(data), fill = c("black", "red"))

## Chi-square test

result <- chisq.test(data)
result

print("The P-value is less than significance level 0.05, so null hypothesis is rejected, which indicates that there is a siginificant associate between the sickle-cell allele and susceptability to heavy malarial infection")


#     7.	What is the value of the test statistic? (hint: consult the output from question 6; it’s OK to simply write this answer – it did come from R output.)

print("The test=statistic is :"  )
print("4.8")

#   8.	What is the p-value? (hint: consult the output from question 6; it’s OK to simply write this answer – it did come from R output.) 

print( "The p-value is 0.02 ")

#   9.	Obtain the Bromeliad.txt dataset from CANVAS and upload it into R as a data frame (answer all questions using R)

mydata <- read.table("/data/Bromeliads.txt",head=T)
mydata

#   10.	Display the contents of Bromeliads.txt (background on this data set is available on p. 627, Exercise 24.35 – data have been slightly modified for the skills test).
df <- data.frame(mydata$Neither, mydata$Nitrogen, mydata$Phorphorus, mydata$Both)

#   11.	Create a single numerical vector that holds all quantitative data from Bromeliads.txt (n.b., please consult and make use of “Module 5 - Week 10 Resources” available on CANVAS, as needed).

single_data <- c(mydata$Neither, mydata$Nitrogen, mydata$Phorphorus, mydata$Both)

#   12.	Create a second vector of the same length that holds the names of all categories under study.

category_names <- rep(names(df), each = nrow(df))

#   13.	Combine the above two vectors into a new data frame, with one column holding the category names and the second holding the quantitative data.

# Step 3: Combine the above two vectors into a new data frame
new_df <- data.frame(Category = category_names, Value = single_data)

new_df

#   14.	Generate side-by-side boxplots of the four categories (conditions) under which bromeliad new leaf growth was evaluated.

ggplot(new_df, aes(x = Category, y = Value, fill = Category)) +
  geom_boxplot() +
  labs(title = "Side-by-Side Boxplots of the Four Categories",
       x = "Category",
       y = "Value") +
  theme_minimal()


#   15.	 Generate QQ-plots for data in each of the four categories.

# reusable Function to create QQ-plot for each category
create_qqplot <- function(data, category) {
  ggplot(data, aes(sample = Value)) +
    geom_qq() +
    labs(title = paste("QQ-Plot for", category),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
}

# Create QQ-plots for each category
qq_plots_list <- lapply(names(df), function(category) {
  create_qqplot(new_df %>% filter(Category == category), category)
})


grid.arrange(grobs = qq_plots_list, ncol = 2)

#   16.	Compare the standard deviations for the data in each of the four categories.
category_stdev <- sapply(df, sd)
print(category_stdev)

#   17.	Fit an ANOVA model to the data.
anova_model <- aov(Value ~ Category, data = new_df)
print(summary(anova_model))


#   18.	Produce an ANOVA table.
anova_table <- summary(anova_model)
anova_table
#   19.	Interpret the ANOVA table.

print("The p-value is < 0.05 significance level, we reject null hypothesis. This indicates the there is evidence")
print("that atleast one category mean is different from others")
print("The F-vlaue is 3.8, which indicates the variation between categories is greater than variation with in the categories.")


#   20.	Implement a Tukey’s HSD multiple comparison test to evaluate which individual categories differ significantly from one another.


tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

#   21.	Interpret the multiple comparison results. 

print( "There is a huge difference between Nitrogen and Neigher categories. 2.375 versus -1.375")   
print( "There is a not much  difference between Phosphorous  and Nitrogen categories.")   




#####.  GC CONTENT

sequence <- "ATTACATGGGCCAAGCCCGATTCAGGTAAGTATCAGCCTGGTTTGGTAATTACT"

#  Locate the start codon, donor splice site (GT), and acceptor splice site (AG)
start_codon <- "ATG"
donor_site <- "GT"
acceptor_site <- "AG"

#  the exonic regions 
exon1 <- "ATGGGCCAAGCCCGATTCAG"
exon2 <- "CCTGGTTTGG"

exon2
exon1

# Calculate the lengths and GC content of each exonic region
exon1_length <- nchar(exon1)
exon1_gc_content <- sum(str_count(exon1, c("G", "C"))) / exon1_length * 100

exon2_length <- nchar(exon2)
exon2_gc_content <- sum(str_count(exon2, c("G", "C"))) / exon2_length * 100

# GC Content
cat("Exonic region 1: Length =", exon1_length, "bp, GC content =", round(exon1_gc_content, 2), "%\n")
cat("Exonic region 2: Length =", exon2_length, "bp, GC content =", round(exon2_gc_content, 2), "%\n")



