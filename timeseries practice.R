library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(fpp3)
library(tsibble)
library(tsibbledata)
library(forecast)


data("vic_elec")

jan14_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  group_by(Date = as_date(Time)) %>%
  summarise( Demand = sum(Demand), Temperature = max(Temperature) )

par(mfrow =c(2,2))
plot(jan14_vic_elec$Temperature, jan14_vic_elec$Demand)
model <- lm(Demand ~ Temperature, data = jan14_vic_elec)
summary(model)


residuals <- residuals(model)
plot(jan14_vic_elec$Temperature, residuals)

plot(Demand~Temperature, data=vic_elec, main= "Demand vs. Temp")
 
