library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(devtools)
library(ggthemr)

burger_king_menu <- read.csv("data/burger-king-menu.csv")
deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
obesity <- read.csv("data/obesity-percents.csv")

dietary_habits <- read.csv("data/Nutrition__Physical_Activity__and_Obesity.csv")

df <- dietary_habits %>% 
  group_by(Question) %>% 
  distinct(Question)

population <- read.csv("data/world_population.csv")

types_of_mortality_vs_fried_food_consumption_frequency <- read.csv("data/Types of Mortality vs. Fried Food consumption Frequency-mean.csv")
fried_food_consumption_and_mortality <- read.csv("data/Fried food consumption and mortality_ prospective cohort study.csv")

frequency_of_visiting_fast_food <- read.csv("data/average-fast-food-consumption-per-week-in-2016-2018.csv", sep = ";")

frequency_of_visiting_fast_food_modified <- frequency_of_visiting_fast_food %>% 
  mutate(X2016 = X2016/100, X2017 = X2017/100, X2018 = X2018/100) %>% 
  pivot_longer(cols = c(X2016, X2017, X2018), 
               names_to = "Year", 
               values_to = "PercentageShare") %>%
  mutate(Year = as.factor(gsub("X", "", Year)))

ggthemr('light')

frequency_of_visiting_fast_food_modified %>% 
  ggplot(aes(y = Answer, x = PercentageShare, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single"), width = 0.5) +
  labs(title = "How often do you eat fast food?", x = "Share of respondents (%)", y = "Answer") 
