library(tsibbledata)
library(ggplot2)
library(fpp2)
library(fpp3)

#Ch. 2
#1a
#gafa_stock
?gafa_stock
autoplot(gafa_stock, Volume)+ggtitle("Gafa Stock Volume")

#PBS
?PBS
head(PBS, 10) #~1992
tail(PBS, 10) #2008
y <- ts(PBS, start = 1992, end = 2008, frequency = 12)
autoplot(y[, "Cost"])+ggtitle("Monthly Medicare Australia Prescription Data")+ylab("Cost")

#vic_elec
?vic_elec
autoplot(vic_elec, Demand)+ggtitle("Half-hourly Electricity Demand for Victoria, Austraila")

#pelt
?pelt
autoplot(pelt)+ggtitle("Pelt Trading Records")
autoplot(pelt, Lynx)+ggtitle("Pelt Trading Records")

#3a
setwd("/Users/alanachiusano/Desktop/MTH 4130")
tute1 <- readr::read_csv("tute1.csv")
View(tute1)

#3b converting data into time series
mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

#3c time series plots
mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, color = name)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y")

#8
?aus_retail
set.seed(12345678)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
autoplot(myseries, Turnover)
gg_season(myseries, Turnover)
gg_subseries(myseries, Turnover)
gg_lag(myseries, Turnover)
ACF(myseries, Turnover) %>% autoplot()

#Ch. 3
#5
?aus_production
head(aus_production)
autoplot(aus_production, Tobacco)

tob <- na.exclude(aus_production[,"Tobacco"]) #define column as a vector, then remove the NAs
tobts <- ts(tob, frequency = 4, start = c(1956,1)) #redefine as a quarterly time series started in Q1 of 1956
lambda_tob <- BoxCox.lambda(tobts)
autoplot(BoxCox(tobts, lambda_tob))+xlab("Quarterly")
lambda_tob

?ansett
head(ansett, 10)
autoplot(ansett, Passengers)
z <- ansett %>%
  filter(Class == 'Economy', Airports == 'MEL-SYD')
autoplot(z, Passengers)

pass <- na.exclude(z[, "Passengers"])
passts <- ts(pass, frequency = 52, start=c(1989, 1))
lambda_pass <- BoxCox.lambda(passts)
autoplot(BoxCox(passts, lambda_pass))+xlab("Weekly")
lambda_pass

?pedestrian
head(pedestrian, 10)
autoplot(pedestrian, Count)
x <- pedestrian %>%
  filter(Sensor == 'Southern Cross Station') %>%
  index_by(Date = yearweek(Date_Time)) %>%
  summarise(Count = sum(Count))
autoplot(x, Count)

lambda_sens <- x %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)
x %>%
  autoplot(BoxCox(Count, lambda_sens))
lambda_sens
