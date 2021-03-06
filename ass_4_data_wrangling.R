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

hawksWithBMI %>% filter(!bird_BMI > 100) %>% ggplot(aes(x = bird_BMI,y = Species ,fill = Species)) + xlab("Bird BMI") + geom_violin() + ylab("Species")
  
hawksFullName %>% 
  group_by(Species) %>%
  summarize(
    num_rows = n(), min_wing = min(Wing, na.rm = T), avg_wing = mean(Wing, na.rm = T), max_wing = max(Wing, na.rm = T), avg_tail_wing_ratio = mean(Wing/Tail, na.rm = T)
  )
  
  
hawksFullName %>% 
  group_by(Species) %>%
  summarize(
    Wing = sum(is.na(Wing)),Weight = sum(is.na(Weight)),Culmen = sum(is.na(Culmen)),Hallux = sum(is.na(Hallux)), Tail = sum(is.na(Tail)), StandardTail = sum(is.na(StandardTail)), Tarsus = sum(is.na(Tarsus)), Crop = sum(is.na(Crop))
    )
  
  








