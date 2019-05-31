

library(ggplot2)
library(readr)
library(reshape2)

kmertestplot <- read_table2("C:/Users/Pankaj Mishra/Downloads/kmertestplot/kmertestplot.txt", 
                            col_names = FALSE)

# Checking missing value
summary(kmertestplot)

kmertestplot = na.omit(kmertestplot)

# We need to melt the dataset as it has various columns, It will melt the dataset into two columns.
# One would be for the data and the secodn one will be for the variable names. Then this will be used for the multiple plottings.


melted.data = melt(kmertestplot) # It's a function of library reshape2

###### Below is the Four versions of the plot########
# Please note the plot is hihgly dependent on the scale of the data. As some of the 
# Columns have huge numbers while others have small number. Hence, plotting all together is very difficult

### This is a limited plot, where we restrict the values between 1, 5 #####
ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 1, position = "stack", alpha = 0.8) +
  xlim(1, 5)+
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 1, position = "identity", alpha = 0.3) +
  xlim(1, 5)+
  theme_grey()
ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 1, position = "fill", alpha = 0.7) +
  xlim(1, 5)+
  theme_grey()

####### Graphs below are without restricted values of variables #########

# Plotting with position as Stack
ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 1000, position = "stack") +
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 10000, position = "stack") +
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 100000, position = "stack") +
  theme_grey()


# Plotting with postion as identity
ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 1000, position = "identity") +
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 10000, position = "identity") +
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 100000, position = "identity") +
  theme_grey()


# Plotting with position as fill

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 1000, position = "fill") +
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 10000, position = "fill") +
  theme_grey()

ggplot(data = melted.data) +
  aes(x = value, fill = variable) +
  geom_density(adjust = 100000, position = "fill") +
  theme_grey()


# Suggested plot for the single variables


## For single plots
library(gridExtra)  # Make sure to install this library
plot.new()

d1 <-ggplot(data = kmertestplot) +
  aes(x = X1) +
  geom_density(adjust = 6, fill = "#9ecae1", position = "identity") +
  theme_minimal()

d2 <-ggplot(data = kmertestplot) +
  aes(x = X2) +
  geom_density(adjust =1000, fill = "#9ecae1", position = "identity") +
  theme_minimal()

d3<-ggplot(data = kmertestplot) +
  aes(x = X3) +
  geom_density(adjust =100, fill = "#9ecae1", position = "identity") +
  theme_minimal()

d4<-ggplot(data = kmertestplot) +
  aes(x = X4) +
  geom_density(adjust =100, fill = "#9ecae1", position = "identity") +
  theme_minimal()

d5 <-ggplot(data = kmertestplot) +
  aes(x = X5) +
  geom_density(adjust =100, fill = "#9ecae1", position = "identity") +
  theme_minimal()

d6<-ggplot(data = kmertestplot) +
  aes(x = X6) +
  geom_density(adjust =100, fill = "#9ecae1", position = "identity") +
  theme_minimal()

d7 <-ggplot(data = kmertestplot) +
  aes(x = X7) +
  geom_density(adjust =100, fill = "#9ecae1", position = "identity") +
  theme_minimal()

# Final all plotting.
grid.arrange(d1,d2, d3, d4, d5, d6, d7, nrow = 3, ncol =3)
