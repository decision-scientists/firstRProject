install.packages("readxl")

library(tidyverse)
library(readxl)

impute_by_mean<-function(x){
  
  mu<-mean(x,na.rm=1) # first compute the mean of x
  
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

impute_by_median<-function(x){
  
  mu<-median(x,na.rm=1) # first compute the mean of x
  
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with mean
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}


v<-c(1,2,NA,4)
impute_by_median(v)

df_xy <- data.frame(x = seq(0.0,9.9,0.1),y = (5 * x) + 1)

df_xy%>%head(5)

df_xy%>%
  mutate(z=map2_dbl(x,y,~.x+.y))%>%
  head(5)

sometimes_missing <- function(index, value){
  if(index %% 5 == 0){
    return(NA)
  }else{
    return(value)
  }
}


sometimes_missing(14,25)
sometimes_missing(15,25)

df_xy_missing <- df_xy %>% 
  mutate(y=map2_dbl(row_number(),y,sometimes_missing))


df_xy_missing%>%
  head(10)


df_xy_impute <-
  df_xy_missing %>%
  mutate(y = impute_by_median(y))


df_xy<-df_xy%>%
  mutate(source="original")

df_xy_missing<-df_xy_missing%>%
  mutate(source="corrupted")

df_xy_impute<-df_xy_impute%>%
  mutate(source="imputed")

df_combined<-rbind(df_xy,df_xy_missing,df_xy_impute)


ggplot(df_combined,aes(x=x,y=y,color=source))+geom_point()+
  facet_wrap(~source)+geom_smooth(method="lm")


folder_path<-"E:\\Git\\firstRProject\\" # set this to the name of the directory containing "HockeyLeague.xlsx"

file_name<-"HockeyLeague.xlsx" # set the file name

file_path<-paste(folder_path,file_name,sep="") # create the file_path 

wins_data_frame<-read_excel(file_path,sheet="Wins") # read of a sheet from an xl file

wins_data_frame %>%
  select(1:5)%>%
  head(3)

wins_tidy <- 
  wins_data_frame %>%
   pivot_longer(!...1,names_to = "Year",values_to = "Wins") %>%
    separate(Wins, into = c("Wins",NA, sep = "of")) %>%
     rename(Total = of, Team = ...1)


wins_tidy%>% dim() # check the dimensions


wins_tidy%>%head(5) # inspect the top 5 rows

loss_data_frame<-read_excel(file_path,sheet="Losses") # read of a sheet from an xl file

losses_tidy <- 
  loss_data_frame %>%
  pivot_longer(!...1,names_to = "Year",values_to = "Losses") %>%
  separate(Losses, into = c("Losses",NA, sep = "of")) %>%
  rename(Total = of, Team = ...1)

hockey_df <- 
  inner_join(wins_tidy,losses_tidy)

hockey_df[,3:5] <- sapply(hockey_df[,3:5],as.numeric)


hockey_df <- 
  hockey_df %>%
  mutate(Draws = (Total - (Wins + Losses)), 
         Wins_rt = Wins/Total,
         Losses_rt = Losses/Total,
         Draws_rt = Draws/Total) 
  
hockey_df%>%
  select(-Wins,-Draws,-Losses)%>%
  group_by(Team)%>%
  summarise(across(starts_with(c("Wins","Losses","Draws")),list(md=median,mn=mean),
                   .names="{substring(.col,1,1)}_{.fn}"))%>%
  arrange(desc(W_md))


max_cor_var<-function(df,col_name){ # function to determine the variable with maximal correlation
  
  v_col<-df%>%select(all_of(col_name)) # extract variable based on col_name
  
  df_num<-df%>%
    select_if(is.numeric)%>%
    select(-all_of(col_name)) # select all numeric variables excluding col_name
  
  correlations<-unlist(map(df_num,
                           function(x){cor(x,v_col,use="complete.obs")})) # compute correlations with all other numeric variables
  
  max_abs_cor_var<-names(which(abs(correlations)==max(abs(correlations)))) # extract the  variable name
  cor<-as.double(correlations[max_abs_cor_var]) # compute the correlation
  
  return(data.frame(var_name=max_abs_cor_var,cor=cor)) # return dataframe
}





