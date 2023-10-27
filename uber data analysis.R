library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(12)
# adds the variables weekday, Hour, minute, seconds and of the day to the uber raw data
addfactors <- function(df){
  df = rename(df, Date.Time = `Date/Time`)
  df$Date.Time <- as.POSIXct(df$Date.Time, format = "%m/%d/%Y %H:%M:%S")
  df$Base <- factor(df$Base)
  df$Date <- mdy(format(df$Date.Time, "%m/%d/%Y"))
  df$Hour <- factor(hour(df$Date.Time), labels = hour_levels())
  df$Minute <- minute(df$Date.Time)
  df$Seconds <- second(df$Date.Time)
  df$Weekday <- factor(wday(df$Date.Time, label = TRUE))
  df$Month <- factor(month(df$Date.Time, label = TRUE))
  return(df)
}
#a function returning a vector of all the labels representing the hours in a day
hour_levels <- function(){
  lvls <- 0:23
  for (ti in lvls) {
    lvls[ti+1] <- sprintf("%d:00-%d:59", ti, ti)
      #cat(start_ti, end_ti, sep = "")
  }
  return(lvls)
}

#returns the average number of uber rides scheduled per hour of the day for any month from april-aug in 2014
avg_num_rides_per_hour <- function(df){
  return (length(df$Hour)/nlevels(factor(df$Hour)))
}

#returns a data frame with the variables total number of rides for each hour of the day
rides_per_hour <- function(df) {
  return (df %>% group_by(Hour) %>% dplyr::summarise(num_rides = n()))
}

#returns a data frame with the variables total number of rides for each hour of the day and base
rides_per_hour_base <- function(df){
  return (df %>% group_by(Hour, Base) %>% dplyr::summarise(num_rides = n()))
}

#returns a data frame with the variables total number of rides for each hour of the day and month
rides_per_hour_month <- function(df){
  return (df %>% group_by(Hour, Month)  %>% dplyr:: summarise(num_rides = n()))
}

#returns a data frame with the variables total number of rides for each hour of the day, base and month
rides_per_hour_base_month <- function(df){
  return (df %>% group_by(Hour, Base, Month)  %>% dplyr:: summarise(num_rides = n()))
}

#returns a data frame with the variables total number of rides that left each base
rides_per_base <- function(df){
  return (df %>% group_by(Base)  %>% dplyr:: summarise(num_rides = n()))
}

#returns a data frame with the variables total number of rides for each month
rides_per_month <- function(df){
  return (df %>% group_by(Month)  %>% dplyr:: summarise(num_rides = n()))
}

#returns a data frame with the variables total number of rides by base and month
rides_per_base_month <- function(df){
  return (df %>% group_by(Base, Month)  %>% dplyr:: summarise(num_rides = n()))
}
#added hour of day, month ride occured in and so on to the  data
aug14 <- addfactors(uber_raw_data_aug14)
jul14 <- addfactors(uber_raw_data_jul14)
may14 <- addfactors(uber_raw_data_may14)
apr14 <- addfactors(uber_raw_data_apr14)
jun14 <- addfactors(uber_raw_data_jun14)
sep14 <- addfactors(uber_raw_data_sep14)


april_sep_14 <- rbind(aug14, sep14, jul14, may14, jun14, apr14)


#data sets showing number of rides that occurred during x hour of day using data from april-september 2014
rides_h <- rides_per_hour(april_sep_14)

#data sets showing number of rides that occurred per hour of day and base using data from april-september 2014
rides_hb <- rides_per_hour_base(april_sep_14)

#data sets showing number of rides that occurred per hour of day and month with data from april-september 2014
rides_hm <- rides_per_hour_month(april_sep_14)

#data sets showing number of rides per hour of day, base and month with data from april-september 2014
rides_hbm <- rides_per_hour_base_month(april_sep_14)
train_rhbm <- rides_hbm %>% sample_frac(0.7)
test_rhbm <- rides_hbm %>% anti_join(train_rhbm)

#data sets showing rides per base with data from april-september 2014
rides_base <- rides_per_base(april_sep_14)

#data sets showing rides per base and month with data from april-september 2014
rides_base_month <- rides_per_base_month(april_sep_14)

#data sets showing rides per month with data from April-September 2014
rides_month <- rides_per_month(april_sep_14)

#linear model with explanatory variable being hour 
#trained on data set with explanatory variables being hour of the day, month and base
num_rides <- length(test_rhbm$Hour)
rh_lm <- lm(num_rides ~ Hour, data=train_rhbm)
rh_predict_vars <- select(test_rhbm, Hour)
rh_predict <- predict(rh_lm, rh_predict_vars)
SSE_rh <- sum( (rh_predict - test_rhbm$num_rides)^2)
#linear model with explanatory variables being hour and month
#trained on data set with explanatory variables being hour of the day, month and base
rhm_lm <- lm(num_rides ~ Hour + Month, data=train_rhbm)
rhm_pred <- predict(rhm_lm, select(test_rhbm, Hour, Month))
SSE_rhm <- sum( (rhm_pred - test_rhbm$num_rides)^2)
#linear model with explanatory variables being hour and base
#trained on data set with explanatory variables being hour of the day, month and base
rhb_lm <- lm(num_rides ~Hour + Base, data=train_rhbm)
rhb_pred <- predict(rhb_lm, select(test_rhbm, Hour, Base))
SSE_rb <- sum( (rhb_pred - test_rhbm$num_rides)^2)
#linear model with explanatory variables being hour, base and month
#trained on data set with explanatory variables being hour of the day, month and base
rhbm_lm <- lm(num_rides ~Hour + Base + Month, data=train_rhbm)
rhbm_pred <- predict(rhbm_lm, test_rhbm)
SSE_rhbm <- sum( (rhbm_pred - test_rhbm$num_rides)^2)

#sum of squared errors of each model
SSE_rhm
SSE_rh
SSE_rhbm
SSE_rb
#will calculate AIC

# tests whether adding base makes the model significantly better
anova(rh_lm,rhm_lm)
anova(rhm_lm, rhb_lm)
anova(rhb_lm, rhbm_lm)
#one-way anova done twice with explanatory variables being base and month, interaction is part of error
#trained on data set with explanatory variables being hour of the day, month and base
rides_base_month_anova <- aov(num_rides ~ Base + Month + Base*Month, data=rides_hbm)

#the bar charts showing the number of rides that occurred per hour of the day,
#second one is the same bar chart with base included

#box plot, scatterplot and histogram of number of rides per hour of day
ggplot(data=rides_hbm) +
  geom_point(mapping = aes(x=Hour, y = num_rides))
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Hour)) + labs(y = "number of rides")
ggplot(data=rides_hbm) +
  geom_boxplot(mapping = aes(y=num_rides, color = Hour)) + labs(y = "total number of rides")
#boxplot and histogram of number of rides per hour of day and base
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Hour, color=Base)) + labs(y = "number of rides")
ggplot(data=rides_hbm) +
  geom_boxplot(mapping = aes(y=num_rides,fill=Hour, color=Base)) + labs(y = "number of rides")
#boxplot and histogram of number of rides per hour of day, month and base
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Hour, color=Base, fill=Month)) + labs(y = "number of rides")
ggplot(data=rides_hbm) +
  geom_boxplot(mapping = aes(y=num_rides,shape=Hour, color=Base, fill=Month)) + labs(y = "total number of rides")
#boxplot and histogram of number of rides per hour of day and base
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Base, color=Month)) + labs(y = "number of rides")
#boxplot and histogram of number of rides per hour of day and month
ggplot(data=rides_hbm) +
  geom_boxplot(mapping = aes(y=num_rides,fill=Hour, color=Month)) + labs(y = "total number of rides")
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Hour, color=Month)) + labs(y = "number of rides")
#boxplot and histogram of number of rides per hour of day and base
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Base), color = "red") + labs(y = "number of rides")
ggplot(data=rides_hbm) +
  geom_boxplot(mapping = aes(y=num_rides,fill=Base), color = "red") + labs(y = "total number of rides")
#boxplot and histogram of number of rides per month
ggplot(data=april_sep_14) +
  geom_bar(mapping = aes(x=Month)) + labs(y = "number of rides")
ggplot(data=rides_hbm) +
  geom_boxplot(mapping = aes(y=num_rides,color=Month)) + labs(y = "total number of rides")