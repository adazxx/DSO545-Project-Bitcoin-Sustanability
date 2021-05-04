setwd("/Users/adazhong/Library/Mobile Documents/com~apple~CloudDocs/DSO 545/project")

library(data.table)
library(fuzzyjoin)
library(dplyr)
library(ggplot2)

df1 = fread("https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt",
            skip = "last 1000 years)")
str(df1)
df2 = fread("https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt",
            skip = "Timescale EDC3_gas_a") %>%
  rename(Age = "Age(yrBP)", CO2 = "CO2(ppmv)")

appx = function(x, y) {
  ifelse(abs(x-y) < 100, T, F)
}

df3 = fuzzy_inner_join(df1, df2, by = c("Age"),
                       match_fun = list(`appx`))

# linear regression - co2(ppmv)~temperature
lin = lm(df3$Temperature~df3$CO2)
x = list(df3$CO2)
pred = predict(lin, x)
df3$Pred.Temperature = pred

cols = c("Pred.Temperature" = "red", "Temperature" = "blue")
ggplot(df3, aes(Age.x))+
  geom_line(aes(y = Pred.Temperature, color = "Pred.Temperature"),alpha = 0.7) +
  geom_line(aes(y = Temperature, color = "Temperature"), alpha = 0.7) +
  scale_x_reverse() +
  scale_color_manual(name = "", values = cols) +
  ylab("Temperature in Celsius ") +
  xlab("Number of Years Before Present") +
  ggtitle("Assessing the Quality of Linear Reg. Model")

######################### abandoned project below ######################

# annual co2 emission
# source: https://ourworldindata.org/co2-emissions
world = c("Africa", "Asia", "Oceania", "Europe", "North America", "South America")
emission = read_csv("absolute-change-co2.csv") %>% 
  filter(Entity %in% world) %>%
  group_by(Year) %>%
  summarise(Global_CO2_Growth = sum(`Annual CO2 growth (abs)`))

# annual co2 level in atmosphere
df4 = fread("https://www.esrl.noaa.gov/gmd/webdata/ccgg/trends/co2/co2_annmean_mlo.txt",
            skip = "abbreviated as ppm") %>%
  rename(Year = "#", CO2 = year)
df4 = df4[,1:2]
