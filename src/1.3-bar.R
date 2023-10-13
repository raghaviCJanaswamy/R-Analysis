####################################
### How to make a pie chart in R ###
####################################

# Please see the "Simple Graphs in R" handout for further
# details.

# Let's create a vector of percentages that sum to 100.

# As we've discussed elsewhere, we'll store the data 
# values to a newly created vector using the c( ) 
# function.

library(plotrix)


mydata <- read.csv("/Users/ragavahini/0_BioInformatics_Educations/00_Coursework/ABT-720/Assignments/CSV/Chapter 1/eg01-03.csv",head=T) 

dim(mydata)

# We can use these values to create a pie chart right 
# now, but the results will be rather displeasing. 

pie(mydata) 
names(mydata)

# add title
barplot(mydata$Percent, 
    main = "Percent of Infectious diseases in California in 2014")

# Vector of Label of the chart

piece_labels <- c("Chlamydia",
                  "Gonorrhea",
                  "Pertussis",
                  "Campylobacteriosis",
                  "Early Syphilis",
                  "Salmonellosis",
                  "Other")

# Add labels for the bar chart
barplot(mydata$Percent, 
    main = "Percent of Infectious diseases in California in 2014",
    ylab ="Percent of infectious diseases - Count",
    names = piece_labels)

/# Add choice of colors of the pie blocks

piece_colors <- c("blue","orange","green",
                  "yellow","purple","brown","red")

# Apply colors to the chart

# Plot the chart.

# Add labels for the pie chart
barplot(mydata$Percent, 
        main = "Percent of Infectious diseases in California in 2014",
        ylab ="Percent of infectious diseases - Count",
        names = piece_labels,
        col = piece_colors)
=

