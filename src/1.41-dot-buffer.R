########################################
### How to make a Wilkinson Dot Plot ###
########################################

# First, Let's enter into 2 vectors some data that we 
# can use to produce a couple Wilkinson Dot Plots

buffer <- c(9.1, 8.1, 7.8, 7.0, 6.8, 5.4, 5.4, 4.1, 3.8, 3.3)

nano <- c(4.1, 3.5, 2.1, 2.1, 1.8, 1.8, 1.4, 1.2, 1.1, 1.1)

# We can create a simple dotplot for the buffer data
# using the stripchart command, as follows:

stripchart(buffer)

# The dotplot would be more attractive if it had a 
# title. As with other graphs in R, we can add a title 
# using the main = argument. 

# Also, notice that there are 10 values in the buffer 
# vector, but only 9 plotting symbols in the dotplot. 

# This is because there are two values of 5.4 and the
# plotting symbols for these identical values are 
# superimposed in the dotplot.

# These problems can be avoided using the main = and 
# method = "stack" arguments as follows:

stripchart(buffer, 
           main = "Distribution of buffer data", 
           method = "stack")

# I am not fond of hollow plotting symbols. Let's
# make them solid circles and give them some color
# using the pch = and col = arguments, as follows:

stripchart(buffer, 
           main = "Distribution of buffer data", 
           method = "stack",
           pch = 19,
           col = "royalblue3")

# That's better! Now let's make a similar dotplot 
# for the nano data.

stripchart(nano, 
           method = "stack", 
           main = "Distribution of nanoparticle data",
           pch = 17,
           col = "green3")

# If we wished to compare these two dotplots, we may 
# be mislead because the x-axis scales are not the same.

# We can correct this problem using the xlim = c( )
# argument, as follows:

stripchart(buffer, 
           main = "Distribution of buffer data", 
           method = "stack",
           pch = 19,
           col = "royalblue3",
           xlim = c(1, 10))

stripchart(nano, 
           method = "stack", 
           main = "Distribution of nanoparticle data",
           pch = 17,
           col = "green3",
           xlim = c(1,10))

# Lastly, we can combine these two graphs into a single
# 2-panel figure using par( ) function, as follows:

par(mfrow = c(2,1))

stripchart(buffer, 
           main = "Distribution of buffer data", 
           method = "stack",
           pch = 19,
           col = "royalblue3",
           xlim = c(1, 10))

stripchart(nano, 
           method = "stack", 
           main = "Distribution of nanoparticle data",
           pch = 15,
           col = "green3",
           xlim = c(1,10))

# Lastly, let's return the par function to its default 
# setting of one graph per figure, as follows.

par(mfrow = c(1,1))

