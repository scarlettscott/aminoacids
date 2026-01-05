## To provide SubFoodGroupCode to collect AA information from FoodData Central database
install.packages("zoo")

library(zoo)

Data <- Food_match %>%  select(SubFoodGroupCode, `NDB no`, `FDC Id`) %>% 
  filter(!is.na(`NDB no`)) %>% mutate(SubFoodGroupCode=zoo::na.locf(SubFoodGroupCode))

write.csv(Data, "Food_ID.csv", row.names = FALSE)

Data$SubFoodGroupCode <- na.locf(Data$SubFoodGroupCode)

tail(Data)

#############################################################################################
# Load AA Data 
library(readr)
library(tidyr)

# NDNS matching - below used to determine the food with highest consumption frequency for matching

#Load Matching data (excel file)
#Food_match <- read_csv("Matched_NDNS_USDA_31_03_2025.csv")
#View(Food_match)
#Food_match <- Food_match %>% 
#rename(fdc_id = `FDC Id`,
       #ndb_no =`NDB no`,
       #usda_desc=`USDA Item Description`)

#Load matching data without NPD (matched separately from paper)
Food_match <- read_csv("Matched_NDNS_USDA_18_08_2025_NPD.csv")
View(Food_match)
Food_match <- Food_match %>% 
  rename(fdc_id = `FDC Id`,
         ndb_no =`NDB no`,
         usda_desc=`USDA Item Description`)

#Select USDA Item Desc, NDB no and FDC ID
Food_match_AA <- Food_match %>%
  select(SubFoodGroupCode,
         usda_desc,
         ndb_no,
         fdc_id)

write.csv(here::here("data",
                     "ID_nos.csv"))

### Use US19 to obtain AA data for all matches 
# Select relevant AA data from US19 
usda_matches <- read_csv("US19_FCT_FAO_Tags.csv")
View(usda_matches)

usda_matches <- usda_matches %>% 
  select(fdc_id, TRPg, THRg, ILEg, LEUg, LYSg, METg, CYSg, PHEg, TYRg, VALg, ARGg, HISg, ALAg, ASPg, GLUg, GLYg, PROg, SERg)


# Join USDA AA data to NDNS-USDA food match data
#Match USDA AA data to NDNS matches
Food_match_AA <- Food_match_AA %>% 
  left_join(usda_matches, by = 'fdc_id')
View(Food_match_AA)

Food_match_AA <- Food_match_AA %>% 
  fill(SubFoodGroupCode, .direction = 'down')

SFGcode_USDA_AA <- Food_match_AA %>% 
  select(-usda_desc, -ndb_no, -fdc_id)

ndns4 %>% filter(SubFoodGroupCode=='13A') %>% View()

#Obtain average AA content per SFG (TOP 60) 
#averaging 1-many to get 1 value for each SFG
Avg_AA_60_SFG <- SFGcode_USDA_AA %>% 
  # select(SubFoodGroupCode, TRPg, THRg, ILEg, LEUg, LYSg, METg, CYSg, PHEg, TYRg, VALg, ARGg, HISg, ALAg, ASPg, GLUg, GLYg, PROg, SERg) %>% 
  group_by(SubFoodGroupCode) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

### Do we need to remove infant formula to the top 60 matches?? - yes 

