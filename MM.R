library(dplyr)
library(tidyr)
library(ggplot2)

burger_king_menu <- read.csv("data/burger-king-menu.csv")
deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
obesity <- read.csv("data/obesity-percents.csv")
number_of_fastfood_restaurants_worldwide <- read.csv("data/Number_of_fastfood_restaurants_worldwide.csv", sep = ";")
deaths_obesity %>% 
  filter(Entity == "United States") %>% 
  ggplot(aes(x=Year, y = Deaths)) +
  geom_point()
colnames(deaths_obesity)[1] <- "Country"
deaths_obesity %>% 
  inner_join(obesity, by=(c("Country","Year")))


# library(reshape2)
# melted_data <- melt(dane, id.vars = "Year")
# print(melted_data)
# str(dane)
selected_columns <- c("Domino.s.Pizza","KFC","Burger.King","PizzaHut","Subway","McDonald.s")
number_of_fastfood_restaurants_worldwide[selected_columns] <- apply(number_of_fastfood_restaurants_worldwide[selected_columns], 2, function(x) as.numeric(gsub(" ", "", x)))
str(number_of_fastfood_restaurants_worldwide)
