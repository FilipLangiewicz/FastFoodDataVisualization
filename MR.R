library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(devtools)
library(ggthemr)
library(tidyverse)
library(ggthemes)
library(openxlsx)


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

deaths_obesity_in_2015_per_country <-deaths_obesity %>% 
  filter(Year == 2015) %>% 
  rename(CCA3 = Code) %>% 
  inner_join(population, by = 'CCA3' ) %>% 
  select(c('Entity', 'Deaths', 'X2015.Population')) %>% 
  rename(c(Country = Entity, Population = X2015.Population)) %>% 
  mutate(Deaths_due_to_obesity_per_mille = (Deaths/Population) * 1000) %>% 
  select(Country, Deaths_due_to_obesity_per_mille) %>% 
  remove_rownames()

# write.xlsx(deaths_obesity_in_2015_per_country, file = "data/deaths_obesity_in_2015_per_country.xlsx")

# wczytuję ramkę z poprawionymi ręcznie nazwami krajów

deaths_obesity_in_2015_per_country <- read.xlsx("data/deaths_obesity_in_2015_per_country.xlsx")

obesity_in_2015_per_country <- obesity %>% 
  filter(Sex == 'Both sexes', Year == 2015) %>% 
  select(Country, Obesity_percent) %>% 
  remove_rownames()

# write.xlsx(obesity_in_2015_per_country, file = "data/obesity_in_2015_per_country.xlsx")

# wczytuję ramkę z poprawionymi ręcznie nazwami krajów

obesity_in_2015_per_country <- read.xlsx("data/obesity_in_2015_per_country.xlsx")

world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>% 
  rename(Country = region) %>% 
  left_join(obesity_in_2015_per_country, by = 'Country')

map_obesity_percent <- countries %>% 
  ggplot(aes(fill = Obesity_percent, map_id = Country)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  scale_fill_viridis_c(na.value = "grey", option = "inferno", direction = -1) +
  theme_map() +
  labs(fill = "Obesity among adults (%)") +
  theme(legend.background = element_rect(fill = "#18191C"), legend.text = element_text(color = "white"), 
        legend.title = element_text(color = "white"))
  
  
ggsave("plots/map_obesity_percent.pdf", plot = map_obesity_percent, width = 6, height = 4)

countries2= world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>% 
  rename(Country = region) %>% 
  left_join(deaths_obesity_in_2015_per_country, by = 'Country')

map_obesity_deaths_per_mille <- countries2 %>% 
  ggplot(aes(fill = Deaths_due_to_obesity_per_mille, map_id = Country)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  scale_fill_viridis_c(na.value = "grey", option = "inferno", direction = -1) +
  theme_map() +
  labs(fill = "Deaths due to obesity (‰)", na.value = "No data") +
  theme(legend.background = element_rect(fill = "#18191C"), legend.text = element_text(color = "white"), 
        legend.title = element_text(color = "white"))

ggsave("plots/map_obesity_deaths_per_mille.pdf", plot = map_obesity_deaths_per_mille, width = 6, height = 4)

