install.packages("tidyverse")

install.packages("ggplot2")

install.packages("dplyr")

library("tidyverse")

library("ggplot2")

library("dplyr")

dailyIntensities <- read.csv("csvs/dailyintensities_merged.csv")

countIntensitiesMinutes <- dailyIntensities %>%
  select(Id, ActivityDay, FairlyActiveMinutes, VeryActiveMinutes)

totalIntensitiesMinutes <- countIntensitiesMinutes %>%
  group_by(Id) %>%
  summarise(totalActive = sum(FairlyActiveMinutes + VeryActiveMinutes, na.rm = TRUE))

# Context for further analysis: There are roughly 31 days worth of data in this dataset. 
# The CDC recommends 2 hrs 30 minutes of moderate intensity exercise per week
# to combat heart disease. Since the dataset doesn't go week-to week I needed 
# to do some calculations to get an idea of what this works out to per month.
# To get a rough idea of this, I divided 150 minutes by 7 to get 21.4 minutes of 
# moderate to intense exercise needed per day.
# This was then multiplied by 31 days, giving me 664.3 minutes needed per month 
# to meet the CDC's monthly minimum requirements to combat heart disease.

cdcRecommendedMonthlyActiveMinutes <- 664.3

metCdcGuidance <- totalIntensitiesMinutes %>%
  filter(totalActive >= cdcRecommendedMonthlyActiveMinutes)

# Context for further analysis: 17 out of 33 individuals met or exceeded the 
# CDC's recommendations, which ends up being roughly 52% of Bellabeat's users. 
# According to a Time magazine article only 23% of Americans get the CDC's 
# recommended amount of exercise. 
# https://time.com/5324940/americans-exercise-physical-activity-guidelines/.

nonuserCountMetPercentCdc <- 23

bellabeatCountMetCdc <- nrow(metCdcGuidance)

bellabeatAllCount <- nrow(totalIntensitiesMinutes)

bellabeatCountMetPercentCdc <- round((bellabeatCountMetCdc / bellabeatAllCount) * 100)

subgroup <- c("nonuser", "bellabeat user")
percent <- c(nonuserCountMetPercentCdc, bellabeatCountMetPercentCdc)

compareBellabeatVsNonusersMetCdc <- data.frame(subgroup, percent)

ggplot(compareBellabeatVsNonusersMetCdc, aes(x = subgroup, y = percent, fill = subgroup)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylim(0, 100) +
  labs(title = stringr::str_wrap("Percent of People who met CDC's Exercise Guidelines", width = 45)) +
  scale_fill_manual(values = c(
    "lightcoral",
    "dimgray"
  )) +
  geom_text(aes(label = percent), position = position_dodge(width = .9), vjust = -.25)
theme(aspect.ratio = 8 / 8)
