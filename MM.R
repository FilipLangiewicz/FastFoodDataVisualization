library(dplyr)
library(tidyr)
library(ggplot2)

burger_king_menu <- read.csv("data/burger-king-menu.csv")
deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
obesity <- read.csv("data/obesity-percents.csv")
dane <- read.csv("data/Number_of_fastfood_restaurants_worldwide.csv", sep = ";")

library(reshape2)
melted_data <- melt(dane, id.vars = "Year")
print(melted_data)
str(dane)
selected_columns <- c("Domino.s.Pizza","KFC","Burger.King","PizzaHut","Subway","McDonald.s")
dane[selected_columns] <- apply(dane[selected_columns], 2, function(x) as.numeric(gsub(" ", "", x)))
dane %>% 
  na.omit() -> data_good
library(reshape2)
melted_data_good <- melt(data_good, id.vars = "Year")


melted_data_good %>% 
  mutate(logo = 0) -> melted_data_good_with_logo
melted_data_good_with_logo$logo <- as.character(melted_data_good_with_logo$logo)

melted_data_good_with_logo[c(19:27),4] <- paste("Loga", "logo3.png", sep = "/")
melted_data_good_with_logo[c(10:18),4] <- paste("Loga", "logo2.png", sep = "/")
melted_data_good_with_logo[c(1:9),4] <- paste("Loga", "logo1.png", sep = "/")
melted_data_good_with_logo[c(28:36),4] <- paste("Loga", "logo4.png", sep = "/")
melted_data_good_with_logo[c(37:45),4] <- paste("Loga", "logo5.png", sep = "/")
melted_data_good_with_logo[c(46:54),4] <- paste("Loga", "logo6.png", sep = "/")


library("ggimage")

melted_data_good_with_logo %>% ggplot(aes(x=Year, y=value)) +
  geom_image(aes(image=logo),size = .05) -> plot_with_logo


melted_data_good_with_logo %>% ggplot(aes(x=Year, y=value, image=logo)) +
  geom_line() + geom_point() + 
  geom_image(size = .05) +
  scale_x_continuous(breaks = seq(min(melted_data_good_with_logo$Year), max(melted_data_good_with_logo$Year), by = 1)) -> plot_with_logo_v2


geom_line() + geom_point() + scale_x_continuous(breaks = seq(min(melted_data_good_with_logo$Year), max(melted_data_good_with_logo$Year), by = 1)) + 
  scale_color_discrete(name = "Legenda") +
  theme(legend.position="right")





  