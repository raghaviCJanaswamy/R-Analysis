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

mydata <- read.csv("/Users/ragavahini/0_BioInformatics_Educations/00_Coursework/ABT-720/Book_Exercises/1.29/ex01-29.csv",head=T) 




     
dim(mydata)

# We can use these values to create a pie chart right 
# now, but the results will be rather displeasing. 

names(mydata)

# Vector of Label of the chart

piece_labels <- c("WatercraftCollisions",
                  "Perinatal",
                  "Natural",
                  "Cold Stress",
                  "Flood gate or canal lock",
                  "Other human",
                  "Undetermined")

# Add labels for the bar chart
barplot(mydata$Manateesrecovered, 
    main = "Count of Cause of Manatees Deaths in Manatees Recovered in 2012",
    ylab ="Count of Manatanee Deaths",
    names = piece_labels)

# Add choice of colors of the pie blocks

piece_colors <- c("blue","orange","green", "yellow","purple","brown","red")

# Apply colors to the chart

# Plot the chart.

data2  <- mydata[order(mydata[,1],decreasing=TRUE),]

dim(data2)


# Add labels for the pie chart
#barplot(=mydata, 
#        main = "Count of Cause of Manatees Deaths in Manatees Recovered in 2012",
#        ylab ="Count of Manatanee Deaths",
        #names.arg = data2[,2])


# Add labels for the bar chart
barplot(mydata[order(mydata[,2],decreasing=TRUE),][,2],
        names.arg=mydata[order(mydata[,2],decreasing=TRUE),][,1],
        main = "Count of Cause of Manatees Deaths in Manatees Recovered in 2012",
        #las=2,
        col = piece_colors,
        ylab ="Count of Manatanee Deaths")

