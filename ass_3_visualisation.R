install.packages("tidyverse") #install tidyverse package
install.packages("Stat2Data") # install package Stat2Data

#Loading required libraries
library(tidyverse)
library(Stat2Data)
data("Hawks")

#Dropping NA and selecting columns of interest
hawksSmall<-drop_na(select(Hawks,Age,Day,Month,Year,CaptureTime,Species,Wing,Weight,Tail))

head(hawksSmall)


# For each of the following variables say whether they continuous, discrete or categorical:  https://rdrr.io/cran/Stat2Data/man/Hawks.html
#   
# Month - Discrete
# Species - Categorical
# Age - Continuous
# Wing - Continuous
# Weight - Continuous

#Whats wrong with this plot?
#1. Seems cluttered
#2. Too many data points
#3. Too much information in 1 chart


plot <- ggplot(data = hawksSmall, aes(x=Weight))+xlab("Weight") + geom_histogram(binwidth = 100) + ylab("Count")
plot

# Aesthetic 
# Weight - Horizontal position
#Its Trimodal

plot <- ggplot(data = hawksSmall, aes(x = Tail)) + xlab("Tail Length") + geom_density() + ylab("Density")
plot

plot <- ggplot(data = hawksSmall, aes(x = Tail)) + xlab("Tail Length") + geom_density(adjust = 0.5) + ylab("Density")
plot

plot <- ggplot(data = hawksSmall, aes(x = Tail)) + xlab("Tail Length") + geom_density(adjust = 1) + ylab("Density")
plot

plot <- ggplot(data = hawksSmall, aes(x = Tail, color = Species)) + xlab("Tail Length") + geom_density(adjust = 1) + ylab("Density")
plot

plot <- ggplot(data = hawksSmall, aes(x = Tail,y = Species ,fill = Species)) + xlab("Tail Length") + geom_violin() + ylab("Species")
plot

plot <- ggplot(data = hawksSmall, aes(x = Tail,y = Weight ,color = Species, shape = Species)) + xlab("Tail(mm)") + geom_point() + ylab("Weight(gm)")
plot

#aesthetics 
# Tail - Horizontal position
# Weight - Vertical Position
# Color - Species
# Shape - Species
# Glyph - points 

ggplot(data = hawksSmall,aes(x = Tail,y = Weight ,color = Species ))    + geom_point()  + xlab("Tail(mm)") + ylab("Weight(gm)") + geom_smooth(method = "lm") +facet_wrap(~Species, scales = "free")

















