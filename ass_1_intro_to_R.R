
# title: "Assignment 1"
# author: "Noah"
# date: "01/10/2020"
# output: R code 

animals <- c("Snake","Ostrich","Cat","Spider") #vector of animals

class(animals) # class of animals 

num_legs <- c(0,2,4,8) # vector of num_legs

data.frame(animals,num_legs) # we can create dataframe with animals and num_legs as columns

city_name <- c( "Bristol", "Manchester", "Birmingham", "London") # vector of city names
population <- c(0.5,0.5,1,9) # vector of populations
cities_populations_df <-data.frame(city_name,population) # we can generate a data frame like this

x <- seq(12,2,-2) #sequence from 12 to 2 with step size -2

X <- matrix(x,2) #matrix of dim (2,3)

Y <- matrix(seq(1,4),2) #matrix of numbers from 1:4 with dim (2,2)

solve(Y)%*%X # (Y)inverse %*% X 
























