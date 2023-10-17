# library(dplyr)
# library(stringr)
# library(tidyr)
# 
# 
# burger_king_menu <- read.csv("data/burger-king-menu.csv")
# deaths_obesity <- read.csv("data/deaths-due-to-obesity.csv")
# quick_service_restaurants_us <- read.csv("data/number-of-quick-service-restaurants-in-the-us-2011_2022.csv")
# Nutrition_Value_Dataset <- read.csv("data/Nutrition_Value_Dataset.csv")
# obesity <- read.csv("data/obesity-cleaned.csv")
#   
# df <- deaths_obesity %>% 
#   filter(Entity == "United States")
# 
# plot(df$Year, df$Deaths.that.are.from.all.causes.attributed.to.high.body.mass.index..in.both.sexes.aged.all.ages)
# 
# ee <- deaths_obesity %>% 
#   mutate(Deaths = round(Deaths))
# 
# eee <- obesity %>% 
#   select(!X)
# 
# write.csv(eee, file = "data/obesity-cleaned.csv",row.names = FALSE) 
# 
# obesity %>%
#   separate_longer_delim(Obesity...., delim = " ")
# 
# # Przykładowy napis
# napis <- "0.5 [0.2-1.1]"
# 
# # Użyj funkcji strsplit do podziału napisu na części
# czesci <- strsplit(napis, " ")
# 
# # Wydziel pierwszą część
# pierwsza_czesc <- czesci[[1]][1]
# 
# # Wyświetl wynik
# print(pierwsza_czesc)
# 
# strsplit(obesity$Obesity...., " ")
# 
# obesity <- obesity %>%
#   mutate(Obesity.... = str_extract(Obesity...., "\\d+\\.\\d+"), 
#          obesity_percent = as.numeric(Obesity....))
# 
# www <- obesity %>% 
#   select(!Obesity....) %>% 
#   mutate(Obesity_percent = obesity_percent) %>% 
#   select(!obesity_percent)
# 
# write.csv(www, file = "data/obesity-cleaned.csv",row.names = FALSE) 
# 
# 
#            
# 
# 
