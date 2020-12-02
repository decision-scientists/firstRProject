library(tidyverse)

calcofi_data_original <- read.csv("194903-201911_Bottle.csv")

# selecting 6 columns from the calcofi original dataset
calcofi_data <- calcofi_data_original %>%
  select(Depth_ID,Depthm,T_degC,Salnty,'Oxy_µmol.Kg',ChlorA) %>%
  rename(depth = Depthm, temperature = T_degC,salinity = Salnty, oxygen = 'Oxy_µmol.Kg', chlorophyll = ChlorA)



get_year <- function(Depth_ID){
  return(paste(substr(Depth_ID,1,2),substr(Depth_ID,4,5),sep =""))
}

get_cast_type <- function(Depth_ID){
  return(substr(Depth_ID,11,12))
}

test<- sapply(calcofi_data["Depth_ID"], get_cast_type)

calcofi_data <- calcofi_data %>%
  mutate(year = get_year(Depth_ID),
         cast_type = get_cast_type(Depth_ID),
  ) %>%
  select(-Depth_ID)

calcofi_data$decade <- as.numeric(calcofi_data$year) - (as.numeric(calcofi_data$year) %% 10)

test <- (calcofi_data %>%
      filter((depth == 100) &  
             (between(decade,1950,2000))))

test1 <- test %>%
  select(temperature, oxygen, salinity, decade) %>%
  group_by(decade) %>%
  summarise(across(everything(),
                   list(mn = ~mean(.x,na.rm = T),
                        md = ~median(.x,na.rm = T),
                        sd = ~sd(.x,na.rm = T))))
            
abbreviate(names(test1)[-1], minlength =6, use.classes = T, strict = T, method = "both.sides")
  
abbreviate(names(test1)[-1]) 


test2 <- str_split(names(test1)[-1],pattern ="_") 
paste(substr(test2[[1]][1],1,3), substr(test2[[1]][2],1,3), sep ="_")

test2[[1]][1]

shorten_colnames <- function(col_name){
  return(paste(substr(col_name,1,3),substr(col_name,nchar(col_name)-2,nchar(col_name)), sep = ""))
}

names(test1)[-1] <- sapply(names(test1)[-1], shorten_colnames)

#Section 2
library(tidyverse)


df_2015 = read.csv("E:\\MScDS\\Assignments\\SCEM\\coursework\\ab20962_EMATM0061_B_dir\\2015.csv") 

df_2015 <- df_2015 %>%
  select(Country,Happiness.Score) %>%
  rename(Score_2015 = Happiness.Score)

df_2019 = read.csv("E:\\MScDS\\Assignments\\SCEM\\coursework\\ab20962_EMATM0061_B_dir\\2019.csv") 

df_2019 <- df_2019 %>% 
  select(Country.or.region,Score) %>%
  rename(Country = Country.or.region, Score_2019 =Score)

head(df_2015)
head(df_2019)

final_df <- inner_join(df_2015,df_2019)

final_df$diff <- (final_df$Score_2015 -final_df$Score_2019)


head(final_df) 











