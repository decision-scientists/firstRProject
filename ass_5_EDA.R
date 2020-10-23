library(tidyverse)
library(Stat2Data)
data("Hawks")

Hawks %>%
  summarise(Wing_mean = mean(Wing,na.rm = T), Wing_t_mean = mean(Wing,na.rm = T, trim = 0.1), Wing_med = median(Wing,na.rm = T), Weight_mean = mean(Weight,na.rm = T), Weight_t_mean = mean(Weight,na.rm = T, trim = 0.1), Weight_med = median(Weight,na.rm = T))


Hawks %>%
  group_by(Species) %>%
  summarise(Wing_mean = mean(Wing,na.rm = T), Wing_t_mean = mean(Wing,na.rm = T, trim = 0.1), Wing_med = median(Wing,na.rm = T), Weight_mean = mean(Weight,na.rm = T), Weight_t_mean = mean(Weight,na.rm = T, trim = 0.1), Weight_med = median(Weight,na.rm = T))


hal<-Hawks$Hallux # Extract the vector of hallux lengths
hal<-hal[!is.na(hal)] # Remove any nans1


outlier_val<-100
num_outliers<-10
corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))

mean(hal)

mean(corrupted_hal)

num_outliers_vect<-seq(0,1000)
means_vect<-c()

for(num_outliers in num_outliers_vect){
  corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))
  means_vect<-c(means_vect,mean(corrupted_hal))
}

num_outliers_vect<-seq(0,1000)
medians_vect<-c()

for(num_outliers in num_outliers_vect){
  corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))
  medians_vect<-c(medians_vect,median(corrupted_hal))
}


num_outliers_vect<-seq(0,1000)
t_means_vect<-c()

for(num_outliers in num_outliers_vect){
  corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers))
  t_means_vect<-c(t_means_vect,mean(corrupted_hal, trim = 0.1))
}

df_means_medians<-data.frame(num_outliers=num_outliers_vect,
                             mean=means_vect,t_mean=t_means_vect,
                             median=medians_vect)

df_means_medians%>%
  pivot_longer(!num_outliers, names_to = "Estimator", values_to = "Value")%>%
  ggplot(aes(x=num_outliers,color=Estimator,
             linetype=Estimator,y=Value))+
  geom_line()+xlab("Number of outliers")

plot <- ggplot(data = Hawks, aes(x = Species, y = Weight)) + xlab("Species") + geom_boxplot() + ylab("Weight")
plot

num_outliers <- function(data) {
  lower = quantile(data,na.rm = T)[2]
  upper = quantile(data, na.rm = T)[4]
  iqr = upper - lower
  thres_upper = (iqr *1.5) + upper
  thres_lower = lower  - (iqr *1.5)
  result <- length(which(data > thres_upper | data < thres_lower))
  print(thres_lower)
  return(result)
}

Hawks %>%
   group_by(Species) %>%
    summarise(num_outliers_weight=num_outliers(Weight))





