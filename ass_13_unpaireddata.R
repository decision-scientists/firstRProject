library(tidyverse)
library(palmerpenguins)

t_test_function <- function(data, val_col, group_col){
  t.test(body_mass_g~species, data=peng_AC,var.equal = TRUE)
}

  



peng_AC<-penguins%>%
  drop_na(species,body_mass_g)%>%
  filter(species!="Gentoo")













