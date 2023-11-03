library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)


burger_king_menu <- read.csv("data/burger-king-menu.csv")
deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
obesity <- read.csv("data/obesity-percents.csv")
mc_in_Europe <- read.csv("data/mcdonalds_in_europe.csv", sep = ";")
mcZestaw <- read.csv("data/McZestawBigMac.csv")
zdrowyObiad <- read.csv("data/zdrowyObiad.csv")
spendings <- read.csv("data/spendings_fast_food_USA_2004_2022.csv", sep = ";")
usa_population <- read.csv("data/population_usa.csv")
population <- read.csv("data/world_population.csv")




zdrowy_vs_fastfood <- mcZestaw %>% 
  filter(Danie == "Zestaw") %>% 
  select(-c(porcja,Blonnik..g.,Cukry..g.,Kwasy.tluszczowe.nasycone..g.)) %>% 
  rows_insert(y = zdrowyObiad %>% 
                select(-porcja))





obesity %>% 
  filter(Country == "United States of America",
         Sex == "Both sexes") %>% 
  ggplot(aes(x = Year, y = Obesity_percent, color = Sex)) +
  geom_point()

quick_service_restaurants_us %>% 
  ggplot(aes(x = year, y = number_of_restaurants)) +
  geom_point()

mcZestaw %>% 
  group_by(porcja) %>%
  summarise(across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>% 
  pivot_longer(cols = -porcja, names_to = "Kolumna", values_to = "Suma") %>% 
  ggplot(aes(x = Kolumna, y = Suma, fill = porcja)) +
  geom_col(position = "dodge")

zdrowy_vs_fastfood %>% 
  pivot_longer(cols = -Danie, values_to = "Wartosc", names_to = "Skladnik") %>% 
  ggplot(aes(x = Skladnik, y = Wartosc, fill = Danie)) +
  geom_col(position = "dodge")

spendings %>% 
  left_join(usa_population, by = "Year") %>% 
  mutate(person_year_spending = Spending.billion.dollars. * 1000000000 / Population) %>% 
  ggplot(aes(x = Year, y = person_year_spending)) +
  geom_line()

deaths_obesity %>% 
  filter(Code == "USA") %>% 
  mutate(dead = Deaths / (usa_population %>% 
           filter(Year %in% 1990:2019))$Population) %>% 
  ggplot(aes(x = Year, y = dead)) + 
  geom_point()

# barplot


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


frequency_of_visiting_fast_food_modified %>% 
  filter(Year == 2018) %>%
  filter(Answer != "Prefer not to say") %>% 
  mutate(pozycja = case_when(Answer == "I don't eat at fast food restaurants" ~ "A",
                             Answer == "Less than once per week" ~ "B",
                             Answer == "One to three times per week" ~ "c",
                             Answer == "Four to six times per week" ~ "D",
                             Answer == "Seven to nine times per week" ~ "E",
                             Answer == "Ten times or more per week" ~ "F",
                             TRUE ~ "Z")) %>% 
  mutate(Answer = fct_reorder(Answer, pozycja)) %>% 
  ggplot(aes(y = Answer, x = PercentageShare, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single"), width = 0.5) +
  labs(title = "How many times a week do you eat fast food?", 
       x = "Share of respondents (%)",
       y = element_blank(),
       legend = element_blank()) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = '#18191C'),
        panel.grid.major = element_blank(),
        title = element_text(colour = "white",
                             family = "mono"),
        axis.text = element_text(colour = "white",
                                 family = "mono",
                                 size = 15,
                                 face = "bold"),
        panel.grid.major.x = element_line(),
        
        legend.position = "none") +
  scale_fill_manual(values = c("#E4D00A")) +
  scale_x_continuous(expand = expansion(c(0,0), c(0, 3)))
x# scale_y_discrete(labels = c("4 - 6",
  #                             "0",
  #                             "< 1",
  #                             "1 - 3",
  #                             "7 - 9",
  #                             "> 9")) +
   


    
  

  


