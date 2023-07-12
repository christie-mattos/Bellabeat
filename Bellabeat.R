# Step 1: Install and load the necessary packages for this analysis. ggplot2 will be used for visualizations later and dplyr for the analysis process.

install.packages("tidyverse")

install.packages("ggplot2")

install.packages("dplyr")

library("tidyverse")

library("ggplot2")

library("dplyr")

# Step 2: Pull in the necessary CSV's for analysis.

# I don't know that I need this: dailyActivity<-read.csv('csvs/dailyActivity_merged.csv')

dailyIntensities <- read.csv("csvs/dailyintensities_merged.csv")

# Step 3: Clean & Filter the data for analysis.

# gather data for each ID from dailyintensities: count the number of FairlyActiveMinutes and VeryActiveMinutes per day as countintensitiesminutes
countIntensitiesMinutes <- dailyIntensities %>%
  select(Id, ActivityDay, FairlyActiveMinutes, VeryActiveMinutes)

# and then add together all the count intensity minutes  per individual person - or Id

totalIntensitiesMinutes <- countIntensitiesMinutes %>%
  group_by(Id) %>%
  summarise(totalActive = sum(FairlyActiveMinutes + VeryActiveMinutes, na.rm = TRUE))

# Step 4: Analyze the data.

# Context for further analysis: There are roughly 31 days worth of data in this dataset. The CDC recommends 2 hrs 30 minutes of moderate intensity exercise per week
# to combat heart disease. Since the dataset doesn't go week-to week I needed to do some calcs to get an idea of what this works out to per month.
# To get a rough idea of this, I divided 150 minutes by 7 to get 21.4 minutes of moderate to intense exercise needed per day.
# This was then multiplied by 31 days, giving us 664.3 minutes needed per month to meet the CDC's monthly minimum requirements to combat heart disease.

# This calc is to declare 664.3 as a data frame to use in the next steps of analysis. cdcRecommendedMonthlyActiveMinutes.
cdcRecommendedMonthlyActiveMinutes <- 664.3


# The below calculation will filter the number of individual people who get greater than or equal to 664.3 minutes of fair to active minutes of activity.
# assign name to the filtered list as metCdcGuidance.
metCdcGuidance <- totalIntensitiesMinutes %>%
  filter(totalActive >= cdcRecommendedMonthlyActiveMinutes)


# Context for further analysis: 17 out of 33 individuals met or exceeded the CDC's recommendations, which ends up being roughly 52% of Bellabeat's users. According to a Time magazine article
# only 23% of Americans get the CDC's recommended amount of exercise. https://time.com/5324940/americans-exercise-physical-activity-guidelines/

# create data frame for the number of americans that get the CDC's recommended amount of exercise. Presented as a percent (23%).
nonuserCountMetPercentCdc <- 23

# Get the number of individual IDs who met the CDC guidance - this will be used later to calculate a percent.
bellabeatCountMetCdc <- nrow(metCdcGuidance)


# count total Bellabeat users - this will be used later to calculate a percent.
bellabeatAllCount <- nrow(totalIntensitiesMinutes)

# count Bellabeat users who didn't meet CDC guidance. I don't think I need this so I'm commenting it out.
# bellabeatCountDidntMeetCdc<-bellabeatAllCount-bellabeatCountMetCdc

# calculate a percent of Bellabeatusers who met CDC guidance by comparing it to all unique bellabeat users in dataset.
bellabeatCountMetPercentCdc <- round((bellabeatCountMetCdc / bellabeatAllCount) * 100)

# create dataframe for Bellabeat comparative bar chart. This will pull in percentages for nonusers as well as bellabeat users.
# Subgroup and Percent create columns that will be used in the next dataframe.
subgroup <- c("nonuser", "bellabeat user")
percent <- c(nonuserCountMetPercentCdc, bellabeatCountMetPercentCdc)
# comparebellabeatusersvscdc adds subgroup and percent into a dataframe which will be used to construct the bar chart.
compareBellabeatVsNonusersMetCdc <- data.frame(subgroup, percent)

# Step 5: Data Visualization: Construct the Bar Chart.

# demonstrate the above statistics into a comparative barchart. Since it's pulling percents set data range up to 100 with ylim function.

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
