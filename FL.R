library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


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

# choosing colors

frequency_of_visiting_fast_food_modified %>% 
  # filter(Year == 2018) %>% 
  ggplot(aes(y = Answer, x = PercentageShare, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.5, preserve = "single"), width = 0.5) +
  labs(title = "How many times a week do you eat fast food?", 
       x = "Share of respondents (%)",
       y = element_blank()) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = '#18191C'),
        panel.grid.major = element_blank(),
        title = element_text(colour = "white",
                             family = "mono"),
        axis.text = element_text(colour = "white",
                                 family = "mono"),
        legend.text = element_text(colour = "white",
                                   family = "mono")) +
  scale_fill_manual(values = c("#E4D00A",
                               "#0a1ee4",
                               "#d00ae4")) +
  scale_y_discrete(labels = c("4 - 6",
                              "0",
                              "< 1",
                              "1 - 3",
                              "?",
                              "7 - 9",
                              "> 9")) +
  geom_vline(xintercept = 0, color = "white") -> plott


    
  

  


