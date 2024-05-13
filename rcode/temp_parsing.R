#load in packages

library(tidyverse)
library(janitor)
library(stringr)
library(readr)


#load in the data

df <- read_csv(file = "") %>% 
  clean_names()

#selecting the elite category
df2 <- df %>% 
  select(program_id, program_notes, category, total_time) %>% 
  filter(category == "Elite Men" | category == "Elite Women") %>% 
  na.omit()

#Parse the temperature out from the program notes variable
water_temp <- str_extract(df2$program_notes, "(?i)water temperature\\s*:?\\s*([0-9]+(?:\\.[0-9]+)?)")
water_temp <- parse_number(water_temp)

air_temp <- str_extract(df2$program_notes, "(?i)air temperature\\s*:?\\s*([0-9]+(?:\\.[0-9]+)?)")
air_temp <- parse_number(air_temp)

air_temp <- as.data.frame(air_temp)
water_temp <- as.data.frame(water_temp)

# Combining both temperature to final data frame
df3 <- cbind(water_temp, air_temp, df2)