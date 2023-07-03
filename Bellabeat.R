install.packages('tidyverse')

install.packages('ggplot2')

install.packages('dplyr')

library('tidyverse')

library('ggplot2')

library('dplyr')

dailyActivity<-read.csv('csvs/dailyActivity_merged.csv')

dailyIntensities<-read.csv('csvs/dailyintensities_merged.csv')

#gather data for each ID from dailyintensities: count the number of FairlyActiveMinutes and VeryActiveMinutes per day as countintensitiesminutes
countIntensitiesMinutes <- dailyIntensities %>%
  select(Id, ActivityDay, FairlyActiveMinutes, VeryActiveMinutes)

#and then add these all together per Id

totalIntensitiesMinutes <- countIntensitiesMinutes %>%
  group_by(Id) %>%
  summarise(totalActive=sum(FairlyActiveMinutes + VeryActiveMinutes, na.rm = TRUE))
    
#There are roughly 31 days worth of data in this dataset. The CDC recommends 2 hrs 30 minutes of moderate intensity exercise per week
#to combat heart disease. Since the dataset doesn't go week-to week I needed to do some calcs to get an idea of what this works out to per month.
#To get a rough idea of this, I divided 150 minutes by 7 to get 21.4 minutes of moderate to intense exercise needed per day.
#This was then multiplied by 31 days, giving us 664.3 minutes needed per month to meet the CDC's monthly minimum requirements to combat heart disease.

#This calc is to declare 664.3 as a variable. cdcRecommendedMonthlyActiveMinutes.
 cdcRecommendedMonthlyActiveMinutes <- 664.3


#the below calculation will count the number of individual people who get greater than or equal to 664.3 minutes. 

totalIntensitiesMinutes %>%
  filter(totalActive>=cdcRecommendedMonthlyActiveMinutes)
  
