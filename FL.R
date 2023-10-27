library(dplyr)
library(tidyr)
library(ggplot2)


burger_king_menu <- read.csv("data/burger-king-menu.csv")
deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
obesity <- read.csv("data/obesity-percents.csv")
mc_in_Europe <- read.csv("data/mcdonalds_in_europe.csv", sep = ";")
mcZestaw <- read.csv("data/McZestawBigMac.csv")
zdrowyObiad <- read.csv("data/zdrowyObiad.csv")

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




  


