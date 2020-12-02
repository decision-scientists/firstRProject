library(palmerpenguins)
library(ggpubr)
library(tidyverse)

data("penguins")


effect_size_one_sample_t_test <- function(x, mu){
  diffs <- x - mu
  ybar <- mean(diffs, na.rm =T)
  s <- sd(diffs, na.rm =T)
  return(ybar/s)
}
  
bill_adelie <- penguins$bill_length_mm

effect_size_one_sample_t_test(bill_adelie,40) 
  
#The bill length experiment will have moderate to large effect
  
library(PairedData)
data("Barley")

#significance level - 0.01  

#Null hypothesis is there is no change between the two yields
#Alternative hypothesis is there is a change between the two yields

diff = abs(Barley$Glabron -Barley$Velvet)

#cumulative density function
ggdensity(diff, 
          main = "Density plot of diffs",
          xlab = "Diffs")

#qqplot 
ggqqplot(diff)

#The diff is a Guassian Distribution

t.test(x = Barley$Glabron, y = Barley$Velvet,paired =T)

#We accept the null hypothesis

library(PairedData)
library(rstatix)
data("Corn")

w_out <- wilcox.test(x = Corn$Crossed, y = Corn$Self,paired = T, exact = T)
v_stat <- as.numeric(w_out["statistic"])
sample_size <- dim(Corn)[1]

wilcox_t_stat <- 2*v_stat - sample_size * (sample_size + 1)/2 

wilcox_t_stat

wilcox_r <- wilcox_t_stat /(sample_size * (((sample_size + 1) * (2*sample_size + 1))/6)^0.5)


wilcoxon_stats <- function(df,col1, col2){
  
  w_out <- wilcox.test(x = df[,col1], y = df[,col2],paired = T, exact = T)
  
  v_stat <- as.numeric(w_out["statistic"])
  
  sample_size <- dim(df)[1] 
  
   T_stat  <- 2*v_stat - sample_size * (sample_size + 1)/2  
   
   effect_size <- T_stat  /(sample_size * (((sample_size + 1) * (2*sample_size + 1))/6)^0.5) 
   
  return(data.frame(T_stat,sample_size,v_stat,effect_size))
  
}

wilcoxon_stats(Corn,"Crossed","Self")  
  
  





