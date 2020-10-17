#loading required libraries
library(tidyverse)
library(Stat2Data)

#loading data
data("Hawks")

# Filtering and selecting columns
hSF <- Hawks %>% filter(Species == "RT", Weight >= 1000 ) %>% select("Wing","Weight","Tail") 

# Displaying first 5 rows of df sorted by ascending order of Wing size
head(hSF %>% arrange(Wing))
  

#Creating a df with full names of Species code and Species full name

species_code <- c('RT','SS','CH')
species_name_full <- c("Red-tailed", "Sharp-shinned", "Cooper's")

species_full_name <- data.frame(species_code, species_name_full)


hawksFullName <- Hawks %>% 
  left_join( species_full_name, by = c("Species" = "species_code")) %>%
  select(-Species) %>%
  rename("Species" = "species_name_full" )

head(hawksFullName %>% 
       select("Species","Wing","Weight"))

hawksWithBMI <- Hawks %>% 
  mutate(bird_BMI = 1000 * (Weight/Wing^2)) %>% 
  select(Species,bird_BMI) %>%
  arrange(desc(bird_BMI))
  
  
  
  
  
  
  








