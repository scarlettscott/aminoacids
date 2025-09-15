library("dplyr")
library("ggplot2")
options(scipen = 999) #Off the scientific notation

# 0 Loading & preparing the data

# Finding the files from NDNS for dietary data
files <- list.files(path = here::here("UK-NDNS-main/data/tab"),
                    pattern = "foodleveldietarydata") %>% 
  stringr::str_subset(., "9|10|11")

# Loading the 3 years of survey together

ndns <- paste0(here::here("UK-NDNS-main/data/tab//"), files) %>% 
  purrr::map_df(~readr::read_delim(., col_types = readr::cols(.default = "c"), 
                                   delim='\t',  locale = readr::locale(encoding = "Latin1"))) 
# Food consumption 
ndns$TotalGrams <- as.numeric(ndns$TotalGrams)

#Loading individual data
ind <- readr::read_delim(here::here("UK-NDNS-main/data", "tab", "ndns_rp_yr9-11a_indiv_20211020.tab"),
                         delim = "\t")

# 4) Getting average food consumption per person
ndns2 <- ndns %>% mutate(across(c(24:81), as.numeric)) %>% 
  group_by(seriali, SurveyYear, DayNo, AgeR, Sex, Country,
           # DiaryDate, DayofWeek, DayNo, 
           FoodName, FoodNumber, SubFoodGroupCode,
           SubFoodGroupDesc, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  mutate(across(where(is.numeric), ~ .x/4))

# Checking main food groups supplying pro

ndns2 %>% 
  # ggplot(aes(forcats::fct_reorder(MainFoodGroupDesc, Proteing), Proteing)) + 
  ggplot(aes(forcats::fct_reorder(MainFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()


# 5) Getting average consumption per person per food subgroup (top 60) (for matching)

ndns2 %>% #average food consumption per person per day
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG 
  group_by(SubFoodGroupDesc, SubFoodGroupCode) %>% 
  #summarise to get median protein per group 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #arrange by protein supply
  arrange(desc(Proteing)) %>% ungroup() %>% 
  #select top 60
  slice_head(n=60)  %>%
  #view in plot SFG, Protein
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +
  scale_y_continuous(breaks = seq(0, max(ndns2$Proteing), by = 1)) + 
  labs(title = 'Average consumption per person per SFG',
       x = 'SFG',
       y = 'Proteing')

# Table format: 
ndns2 %>% #average food consumption per person per day
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG 
  group_by(SubFoodGroupDesc, SubFoodGroupCode) %>% 
  #summarise to get median protein per group 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #arrange by protein supply
  arrange(desc(Proteing)) %>% ungroup() %>% 
  #select top 60
  slice_head(n=60)  %>% View()

#################################################
# 6)
#Preparing individual data for merging with ndns2:
ind2 <- ind %>% select(seriali,Veg, VegeChk, VeganChk, vegetarn)

str(ind2$seriali) # check if seriali number or character
# Converting variables into character
ind2$seriali <- as.character(ind2$seriali)


# Merging ind2 with ndns2 to obtain vegan info for each individual
ndns3 <- ndns2 %>%    
  left_join(ind2, by = "seriali")

#Checking main vegetarian food groups supplying protein

ndns3 %>% filter(Veg==2) %>% #average consumption per vegetarian person per day 
  #select variables of interest
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc,SubFoodGroupCode, 
         SubFoodGroupDesc,TotalGrams, Proteing) %>% 
  #group by person, food groups (total grams and protein?)
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  #sum by SFG for each veg individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG
  group_by(SubFoodGroupCode, SubFoodGroupDesc) %>% 
  #calculate median protein per person 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #arrange desc(P)
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% View()


#########################################################
#Create DietChk, checking self-reported dietary groups against what was actually consumed
DietChk <- ndns3 %>% 
  select(seriali, Veg, VegeChk, VeganChk, vegetarn, FoodName, FoodNumber, SubFoodGroupCode, SubFoodGroupDesc, MainFoodGroupCode, MainFoodGroupDesc)
#Vegans and vegetarians comsuming dairy and meat products respectively. Recategorise into 'true' consumption categories

# 7) Create new dietary groups

#Identify keywords that indicate non-vegetarian (meat consumption), pescatarian or vegetarian (dairy or eggs)
meat_keywords <- c("BACON AND HAM", 
                   "BEEF VEAL AND DISHES", 
                   "BURGERS AND KEBABS", 
                   "CHICKEN AND TURKEY DISHES", 
                   "COATED CHICKEN", 
                   "LAMB AND DISHES", 
                   "LIVER AND DISHES",
                   "MEAT PIES AND PASTRIES", 
                   "OTHER MEAT AND MEAT PRODUCTS",
                   "PORK AND DISHES",
                   "SAUSAGES")

meat_keywords_foodnames <- c("CHICKEN",
                             "LAMB",
                             "BEEF",
                             "PORK",
                             "QUICHE, MEAT BASED")

pesc_keywords <- c("OILY FISH",
                   "OTHER WHITE FISH SHELLFISH & FISH DISHES", 
                   "WHITE FISH COATED OR FRIED") 

pesc_keywords_foodnames <- c("COD LIVER OIL",
                             "FISH OIL",
                             "FISH",
                             "FOREVER ARTIC SEA",
                             "OMEGA 3")

vegetarian_keywords <- c("EGGS AND EGG DISHES",
                         "YOGURT")

vegetarian_keywords_foodnames <- c("MILK SEMI-SKIMMED PASTEURISED WINTER", 
                                   "MILK SKIMMED PASTEURISED WINTER", 
                                   "CHEESE CHEDDAR ENGLISH",
                                   "CHEESE CHEDDAR IRISH",
                                   "CHEESE CHEDDAR ANY OTHER OR FOR RECIPES", 
                                   "CHEESE PARMESAN",
                                   "CHEESE LEICESTERSHIRE",
                                   "CHEESE HALLOUMI",
                                   "CHEESE GRUYERE",
                                   "CHEESE MASCARPONE",
                                   "CHEESE FETA",
                                   "RICOTTA",
                                   "PANEER",
                                   "CHEESE PROCESSED SLICES OR BLOCKS",
                                   "CHEESE SPREADS, TRIANGLES, PLAIN, DAIRYLEA ONLY",
                                   "CHEESE DOUBLE GLOUCESTER",
                                   "MACARONI CHEESE",
                                   "CHEESECAKE",
                                   "CHEESE SOFT FULL FAT SOFT CHEESE - PHILADELPHIA TYPE", 
                                   "CHEESE SOFT MEDIUM FAT", 
                                   "CHEESE SOFT LOW FAT",
                                   "CHEESE OR CHEESE AND TOMATO PIZZA WITH VEGS AND/OR FRUIT. NO MEAT, NO FISH, WITH ANY BASE, RETAIL", 
                                   "CHEESE MOZZARELLA", 
                                   "CHEESE CREAM FULLFAT", 
                                   "GOATS CHEESE", 
                                   "YOGURT LOW FAT",
                                   "YOGURT , WHOLE MILK, NATURAL, UNSWEETENED",
                                   "YOGURT, FULL FAT",
                                   "CHEESE AND TOMATO PIZZA",
                                   "ICE CREAM", 
                                   "ICECREAM",
                                   "ICE-CREAM",
                                   "CREAM DOUBLE",
                                   "CREAM SINGLE PASTEURISED",
                                   "EGG",
                                   "WHEY PROTEIN",
                                   "WHEY POWDER",
                                   "YOGURT")

# Create a new variable to flag meat/fish/dairy consumption
ndns3 <- ndns3 %>% mutate(
  Consumed_Meat =
    grepl(paste(meat_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(meat_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))

ndns3 <- ndns3 %>% mutate(
  Consumed_Fish = 
    grepl(paste(pesc_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(pesc_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))

ndns3 <- ndns3 %>% mutate(
  Consumed_DairyEgg = 
    grepl(paste(vegetarian_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(vegetarian_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))


#Info on reported dietary group and true/false intakes 
DietGrp <- ndns3 %>% select(seriali, DayNo, Consumed_Meat, Consumed_Fish, Consumed_DairyEgg)

DietGrp <- DietGrp %>% 
  left_join(ind2, by = "seriali")


#Create 'Diet' variable 
DietGrp <- DietGrp %>% mutate(Diet = case_when(
  Consumed_Meat ~ "Neither", 
  Consumed_Fish ~ "Pescatarian",
  Consumed_DairyEgg ~ "Vegetarian",
  TRUE ~ "Vegan"))

#Detertmine true diet per individual
determine_diet <- function(diets){
  if ("Neither" %in% diets) {
    return("Neither")
  } else if ("Pescatarian" %in% diets) {
    return("Pescatarian")
  }
  else if ("Vegetarian" %in% diets) {
    return("Vegetarian")
  } else {return("Vegan")}}


#View diet group changes
DietGrp_summary <- DietGrp %>% 
  group_by(seriali, Veg) %>% 
  summarize(Overall_Diet = determine_diet(Diet)) %>% ungroup()

DietGrp2 <- DietGrp_summary %>% ungroup() %>% select(seriali, Overall_Diet)

#Check
DietGrp %>% filter(Diet=='Vegan') %>% View()

DietGrp2 %>% filter(Overall_Diet=='Vegan') %>% View()
# 3 vegans 

###########################################
# 8) Individual dietary data of true dietary groups
ndns4 <- ndns3 %>% 
  left_join(DietGrp2, by = "seriali")

write.csv(here::here("data", 
                     "ndns_dietgroups.csv"))


# 9) Demographic data

#Summarise by gender, age, geo location and household income 
ind3 <- ind %>% select(seriali, Sex, AgeR, agegr1, Weight, region, GOR, eqv3)
ind3$seriali <- as.character(ind3$seriali)

#Inclusion of demographic data with individual data in ndns5
ndns5 <- ndns4 %>% left_join(ind3, by = "seriali")

demographics <- DietGrp_summary %>% 
  left_join(ind3, by = "seriali")
write.csv(here::here("data", 
                     "demographics.csv"))

#########################################################
# 10) 
#View top 60 food groups supplying protein to pop 
## NEW
ndns4 %>% #average food consumption per person per day
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG 
  group_by(SubFoodGroupDesc, SubFoodGroupCode) %>% 
  #summarise to get median protein per group 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #arrange by protein supply
  arrange(desc(Proteing)) %>% ungroup() %>% 
  #select top 60
  slice_head(n=60)  %>%
  #view in plot SFG, Protein
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() + ylim(0,10) +
  scale_y_continuous(breaks = seq(0, max(ndns2$Proteing), by = 1)) + 
  labs(title = 'Average daily consumtpion of top 60 food groups supplying protein to UK population',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename = "Plots/New Plots/Pop_SFG_pro_CHECK.jpg")



#Save top 60 for matching:
ndns4 %>% #average food consumption per person per day
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing) %>% 
  #group by person, food groups
  group_by(seriali, MainFoodGroupDesc, MainFoodGroupCode, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG 
  group_by(MainFoodGroupDesc, MainFoodGroupCode, SubFoodGroupDesc, SubFoodGroupCode) %>% 
  #summarise to get median protein per group 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #arrange by protein supply
  arrange(desc(Proteing)) %>% ungroup() %>% 
  #select top 60
  slice_head(n=60) %>% View()

write.csv("data/TotalTop60SFG.csv", row.names = FALSE)

ggsave(filename= "Plots/New Plots/Pop_SFG_av_pro.png")

###########################################
# 11)
# View top 20 food groups supplying protein to each dietary group

#NEW VERSION EXCLUDE INFANTS UNDER 5?
#ADULTS ONLY, ARRANGE BY DIET GROUP AND DESC PROTEIN?


ndns4 %>% #average food consumption per person per day
  #filter(Overall_Diet=='Vegan') %>% 
  filter(as.numeric(AgeR) > 5 ) %>%
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing, Overall_Diet) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG 
  group_by(SubFoodGroupDesc, SubFoodGroupCode, Overall_Diet) %>% 
  #summarise to get median protein per group 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(Overall_Diet) %>% 
  #arrange by protein supply
  arrange(desc(Proteing),.by_group = TRUE) %>%
  #select top 20
  slice_head(n=20) %>% 
  mutate(test=factor(SubFoodGroupDesc, SubFoodGroupDesc)) %>%
  #mutate(Overall_Diet=reorder(Overall_Diet, Proteing, FUN = median)) %>% 
  #view in plot SFG, Protein
  #ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  ggplot(aes(Proteing, test)) +
  geom_boxplot() +  
  labs(title = 'Average daily consumtpion of top 20 food groups supplying protein to dietary groups',
       x = 'Food Groups',
       y = 'Average Protein (g)') +
  facet_wrap(~Overall_Diet, scales = 'free_y') 

ggsave(filename = "Plots/New Plots/SFG_protein.jpg")


data$facet_var <- reorder(data$facet_var, -ave(data$value, data$facet_var, FUN = sum))



###########################################################################################
#Is this necessary anymore??? no??

devtools::install_github("jmcphers/bookmarkr")

#Check nutrition powders and drinks 

NPD <- ndns5 %>% filter(SubFoodGroupCode=='50E') %>%
  select(seriali, Overall_Diet, FoodName, SubFoodGroupCode, SubFoodGroupDesc, Energykcal, EnergykJ, Proteing, Fatg, Carbohydrateg, TotalGrams, Waterg)

NPD <- NPD %>% 
  mutate(Proteing100g = (Proteing / TotalGrams) * 100)

NPD %>% 
  write.csv(here::here("data",
                       "NPD.csv"))

Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% View()


# Nutrition keywords
#What about fybogel? 

powder_keywords <- c("dry",
                     "PROTEIN POWDER",
                     "MEAL REPLACEMENT MILKSHAKE POWDER",
                     "MEAL REPLACEMENT POWDER",
                     "POWDER ONLY",
                     "COMPLAN",
                     "COMPLAN",
                     "whey",
                     "METAGENICS ULTRAINFLAMX",
                     "SHAKE POWDER",
                     "FYBOGEL",
                     "CAMBRIDGE WEIGHT PLAN CHOCOLATE SHAKE FORTIFIED",
                     "LIGHTER LIFE TOTAL BALANCE SOUP POWDER FORTIFIED")

drinks_keywords <- c("liquid",
                     "PROTEIN SHAKE",
                     "shakes",
                     "DUNN'S RIVER NURISHMENT",
                     "ENSURE",
                     "MEAL REPLACEMENT DRINK",
                     "ENERGY DRINK",
                     "INFATRINI")

bars_keywords <- c("BARS")

NPD <- NPD %>% mutate(
  nutrition_powders = 
    grepl(paste(powder_keywords, collapse = "|"), FoodName, ignore.case = TRUE))

NPD <- NPD %>% mutate(
  nutrition_drinks = 
    grepl(paste(drinks_keywords, collapse = "|"), FoodName, ignore.case = TRUE))

NPD <- NPD %>% mutate(
  nutrition_bars = 
    grepl(paste(bars_keywords, collapse = "|"), FoodName, ignore.case = TRUE))

NPDGrp <- NPD %>% select(seriali, DayNo, nutrition_powders, nutrition_drinks, nutrition_bars)

NPDGrp <- NPDGrp %>% mutate(NPD = case_when(
  nutrition_powders ~ "Powders", 
  nutrition_drinks ~ "Drinks",
  TRUE ~ "Bars"))

labels <- NPDGrp %>% ungroup() %>% 
  select(FoodNumber, NPD)



#Average consumption of NPDs

Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% 
  filter(Overall_Diet=='Vegetarian') %>% ungroup() %>% 
  summarise(mean_proteing=mean(Proteing, na.rm = TRUE))

summarise(mean_LYS=mean(LYSg, na.rm = TRUE)) %>% print()
#== 9.93 protein 
#== 2.29 lysine 


Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% 
  filter(Overall_Diet=='Pescatarian') %>% ungroup() %>% 
  summarise(mean_Protein_g100g=mean(Protein_g100g, na.rm = TRUE)) %>% print()
#== 15.7 p CONSUMPTION 
#== 32.8 p composition
#== 2.29 L

Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% 
  filter(Overall_Diet=='Neither') %>% ungroup() %>% 
  summarise(mean_LYS=mean(LYSg, na.rm = TRUE)) %>% print()
#== 10.9 P
#== 2.29 L

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Protein_g100g) %>% 
  arrange(desc(Protein_g100g)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Protein_g100g), Protein_g100g)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Vegetarians',
       x = 'Food Groups',
       y = 'Average Protein (g)')

### Use g/100g rather than total protein g???

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Vegan') %>%
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 17.1

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Vegetarian') %>% 
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 4.55

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Pescatarian') %>%
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 15.7

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Neither') %>% 
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 10.9



###########################################################################################
## To provide SubFoodGroupCode to collect AA information from FoodData Central database
install.packages("zoo")

library(zoo)

Data <- Food_match %>%  select(SubFoodGroupCode, `NDB no`, `FDC Id`) %>% 
  filter(!is.na(`NDB no`)) %>% mutate(SubFoodGroupCode=zoo::na.locf(SubFoodGroupCode))

write.csv(Data, "Food_ID.csv", row.names = FALSE)

Data$SubFoodGroupCode <- na.locf(Data$SubFoodGroupCode)

tail(Data)

#############################################################################################
# 12) Load AA Data 01/08
library(readr)
library(tidyr)

# NDNS matching - below used to determine the food with highest consumption frequency for matching

#Load Matching data (excel file)
#Food_match <- read_csv("Matched_NDNS_USDA_31_03_2025.csv")
#View(Food_match)
#Food_match <- Food_match %>% 
  rename(fdc_id = `FDC Id`,
         ndb_no =`NDB no`,
         usda_desc=`USDA Item Description`)

#NEW without NPD
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


# Join to food match data?
#Match USDA AA data to NDNS matches
Food_match_AA <- Food_match_AA %>% 
  left_join(usda_matches, by = 'fdc_id')
View(Food_match_AA)

Food_match_AA <- Food_match_AA %>% 
  fill(SubFoodGroupCode, .direction = 'down')

Food_match_AA <- Food_match_AA %>% 
  select(-usda_desc, -ndb_no, -fdc_id)



#Obtain average AA content per SFG (TOP 60)
Avg_AA_SFG <- Food_match_AA %>% 
 # select(SubFoodGroupCode, TRPg, THRg, ILEg, LEUg, LYSg, METg, CYSg, PHEg, TYRg, VALg, ARGg, HISg, ALAg, ASPg, GLUg, GLYg, PROg, SERg) %>% 
  group_by(SubFoodGroupCode) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


###################################

#Match NPD AA from paper to Food_match_AA

#View NPDs by frequency of consumption
ndns4 %>% filter(SubFoodGroupCode=='50E') %>% 
  group_by(FoodName) %>% 
  summarise(Frequency = n ()) %>% 
  arrange(desc(Frequency)) %>% View() 


## NPD MATCHING


#Load matched data 
NPD_AA_match <- read.csv("NPD_matching.csv")
View(NPD_AA_match)
#Check consumption of foods without matches (low rankig items)
ndns2 %>% ungroup() %>% filter(FoodName=='PECTIN (DRY MIX)') %>%
  select(seriali, TotalGrams, Proteing) %>% View()
#never more than 0.5g total

ndns2 %>% ungroup() %>% filter(FoodName=='BELEAN HIGH PROTEIN SHAKE') %>%
  select(seriali, TotalGrams, Proteing) %>% View()
# one person, once. 50g total, 3.6g protein


library(tidyr)
#Screen out low ranking items
#Reasoning: 
# Pectin - never more that 0.5g total. 
# BELEAN = one person had once. 50g total, 3.6g protein. No protein type info. 
# Fybogel = not protein
NPD_AA_match <- NPD_AA_match %>% 
  select(-X) %>%
  drop_na()

#Match NPD AA info to NDNS consumption
# Converting variables into character
NPD_AA_match$FoodNumber <- as.character(NPD_AA_match$FoodNumber)

View(NPD_AA_match)

NPD_AA_match <- NPD_AA_match %>% select(-FoodName)

#Match this NPD AA data to ndns consumption data along with the USDA data then multiply AA (g1g) by dry weight Protein_g1g for each intake (see below)

#####################
#Calculate dry weight and protein per g for just NPDs
AA_NDNS <- ndns4 %>%
  mutate(
    NPD = SubFoodGroupCode == "50E",
    dry_weight = if_else(NPD, TotalGrams - Waterg, NA_real_),
    Protein_g1g = if_else(NPD, Proteing/dry_weight, NA_real_))
 #Match AA composition data 
AA_NDNS_match <- AA_NDNS %>% left_join(NPD_AA_match, by = 'FoodNumber')

#Calculate true AA consumption
AA_NDNS_match <- AA_NDNS_match %>% mutate(
  LEUg = (Protein_g1g * Leucine_g1g),
  ILEg = (Protein_g1g * Isoleucine_g1g),
  VALg = (Protein_g1g * Valine_g1g),
  LYSg = (Protein_g1g * Lysine_g1g),
  METg = (Protein_g1g * Methionine_g1g),
  HISg = (Protein_g1g * Histidine_g1g),
  PHEg = (Protein_g1g * Phenylalanine_g1g),
  THRg = (Protein_g1g * Threonine_g1g))

#Check AA conetnts calculated for NODs
AA_NDNS_match %>% filter(SubFoodGroupCode=='50E') %>% View()

#Merge AA datasets with NDNS

#Merge AVG AA for SFGs (SFGCode)
NDNS_USDA <- AA_NDNS_match %>% 
  left_join(Avg_AA_SFG, by = 'SubFoodGroupCode', suffix = c("", "_new"))

#Fill missing values using coalesce()
NDNS_USDA <- NDNS_USDA %>% 
  mutate(
    LEUg = coalesce(LEUg, LEUg_new),
    ILEg = coalesce(ILEg, ILEg_new),
    VALg = coalesce(VALg, VALg_new),
    LYSg = coalesce(LYSg, LYSg_new),
    METg = coalesce(METg, METg_new),
    HISg = coalesce(HISg, HISg_new),
    PHEg = coalesce(PHEg, PHEg_new),
    THRg = coalesce(THRg, THRg_new),
  ) %>% select(-ends_with("_new")) #remove temporary columns

#Remove other temp columns? e.g. g1g and g100g?
############################################################################################
#Merge digestibility data

#Load digestibility data (matched with NDNS top 60 and respective USDA matches)
Digest_matches <- read_csv("Matched_NDNS_USDA_DIAAS_22_04_2025.csv")

#Remove unnecessary columns and drop empty
Digest_matches <- Digest_matches %>% select(
  -`USDA Item Description`, -`NDB no`, 
  -`FDC Id`, -`Avg_protein, g/100g`, -`Water, g`) %>% 
  drop_na()

#Select only SFG Code and digestibility coefficients
Digest_coef <- Digest_matches %>% 
  select(-MainFoodGroupCode, -MainFoodGroupDesc, -SubFoodGroupDesc, -TotalGrams, -Proteing, -`Protein_g/100g_NDNS`, -Food_item_name)

#Merge with NDNS_USDA
NDNS_USDA_DIG <- NDNS_USDA %>% 
  left_join(Digest_coef, by = 'SubFoodGroupCode')

#Multiply digestibility coefficients (mg/g protein) with composition data to obtain digestible AA composition 
NDNS_USDA_DIG <- NDNS_USDA_DIG %>% 
  mutate(
    Digest_TRPmg = `TRP_Dig_mg/g` * Proteing,
    Digest_THRmg = `THR_Dig_mg/g` * Proteing, 
    Digest_ILEmg = `ILE_Dig_mg/g` * Proteing,
    Digest_LEUmg = `LEU_Dig_mg/g` * Proteing,
    Digest_LYSmg = `LYS_Dig_mg/g` * Proteing,
    Digest_SAAmg = `SAA_Dig_mg/g` * Proteing,
    Digest_AAAmg = `AAA_Dig_mg/g` * Proteing,
    Digest_VALmg = `VAL_Dig_mg/g` * Proteing,
    Digest_HISmg = `HIS_Dig_mg/g` * Proteing
  )

### Dataset has columns that may not be necessary anymore. Should they be removed or is it fine to leave?


##############################################################################################

#01/08
#Determine average weights for adult men and women, elderly men and women, infants, children and adolescents
# Filter to remove -1.0? N/A scores 
#male adults
demographics %>% filter(AgeR > 18 & AgeR < 65 & Sex==1) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 81.3 = ~80

#female adults
demographics %>% filter(AgeR > 18 & AgeR < 65 & Sex==2) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 68.9 = ~70

#elderly male 
demographics %>% filter(AgeR > 64 & Sex==1) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 75.7 = ~75

#elderly female 
demographics %>% filter(AgeR > 64 & Sex==2) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 59.4 = ~60

#infants == 12.7
demographics %>% filter(AgeR < 4) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()

#children == 25.3
demographics %>% filter(AgeR > 3 & AgeR < 11) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()

#adolescent male = 56
demographics %>% filter(Sex==1 & AgeR > 10 & AgeR < 19) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()

#adolescent female = 54.6
demographics %>% filter(Sex==2 & AgeR > 10 & AgeR < 19) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()



#############################################################################################################
# 17)
### Mean daily intakes of protein and IAAs for adults 

## Whole population: 
Dailyintakes <- NDNS_USDA_DIG %>% 
  group_by(seriali, DayNo, AgeR, Sex, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            #sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRPmg, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRPmg, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THRmg, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILEmg, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEUmg, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYSmg, na.rm = TRUE),
            sum_SAAg = sum(METg + CYSg, na.rm = TRUE),
            sum_dig_SAA = sum(Digest_SAAmg, na.rm = TRUE),
            sum_AAAg = sum(PHEg + TYRg, na.rm = TRUE),
            sum_dig_AAAg = sum(Digest_AAAmg, na.rm = TRUE),
            #sum_METg = sum(METg, na.rm = TRUE),
            #sum_dig_MET = sum(Digest_METmg, na.rm = TRUE),
            #sum_CYSg = sum(CYSg, na.rm = TRUE),
            #sum_dig_CYS = sum(Digest_CYSmg, na.rm = TRUE),
            #sum_PHEg = sum(PHEg, na.rm = TRUE),
            #sum_dig_PHE = sum(Digest_PHEmg, na.rm = TRUE),
            #sum_TYRg = sum(TYRg, na.rm = TRUE),
            #sum_dig_TYR = sum(Digest_TYRmg, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VALmg, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            #sum_dig_ARG = sum(Digest_ARGmg, na.rm = TRUE),
            sum_HISg = sum(HISg, na.rm = TRUE),
            sum_dig_HIS = sum(Digest_HISmg, na.rm = TRUE)) %>% ungroup()

#No digestiblity data for arginine but as semi essential not a concern?
##############################
#13/09
#Mean daily intakes by sub groups

# Split into age groups (1.5-3, 4-10, 11-18, 19-64, 65+)
Dailyintakes$AgeR=as.numeric(as.character(Dailyintakes$AgeR))
  
Dailyintakes <- Dailyintakes %>% 
  mutate(AgeGrp = case_when(
    AgeR < 4 ~ "Infant",
    AgeR >= 4 & AgeR <= 10 ~ "Child",
    AgeR >= 11 & AgeR <= 18 ~ "Adolescent",
    AgeR >= 19  & AgeR <= 64 ~ "Adult",
    AgeR >= 65 ~ "Senior",
    TRUE ~ NA_character_ #catch-all for missing or unexpected values
  ))


Dailyintakes <- Dailyintakes %>% 
  mutate(sum_TRPmg = sum_TRPg * 1000, 
         sum_THRmg = sum_THRg * 1000, 
         sum_ILEmg = sum_ILEg * 1000,
         sum_LEUmg = sum_LEUg * 1000, 
         sum_LYSmg = sum_LYSg * 1000,
         sum_SAAmg = sum_SAAg * 1000,
         sum_AAAmg = sum_AAAg * 1000, 
         sum_VALmg = sum_VALg * 1000, 
         sum_HISmg = sum_HISg * 1000)

Dailyintakes %>% filter(Sex==1 & AgeGrp == 'Adult') %>%
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()

##Exclude low energy intakes?? 

#Maleintakes <- Dailyintakes %>% filter(Sex==1) %>% 
  group_by(AgeGrp, Overall_Diet) %>% 
  summarise(mean_proteing = mean(sum_Proteing, na.rm = TRUE),
            mean_energy = mean(sum_energy, na.rm = TRUE),
            mean_TRPg = mean(sum_TRPg, na.rm = TRUE),
            mean_DIGTRP = mean(sum_dig_TRP, na.rm = TRUE),
            mean_THRg = mean(sum_THRg, na.rm = TRUE),
            mean_DIGTHR = mean(sum_dig_THR, na.rm = TRUE),
            mean_ILEg = mean(sum_ILEg, na.rm = TRUE),
            mean_DIGILE = mean(sum_dig_ILE, na.rm = TRUE),
            mean_LEUg = mean(sum_LEUg, na.rm = TRUE),
            mean_DIGLEU = mean(sum_dig_LEU, na.rm = TRUE),
            mean_LYSg = mean(sum_LYSg, na.rm = TRUE),
            mean_DIGLYS = mean(sum_dig_LYS, na.rm = TRUE),
            mean_SAAg = mean(sum_SAAg, na.rm = TRUE),
            mean_DIGSAA = mean(sum_dig_SAA, na.rm = TRUE),
            mean_AAAg = mean(sum_AAAg, na.rm = TRUE),
            mean_DIGAAA = mean(sum_dig_AAAg, na.rm = TRUE),
            mean_VALg = mean(sum_VALg, na.rm = TRUE),
            mean_DIGVAL = mean(sum_dig_VAL, na.rm = TRUE),
            mean_ARGg = mean(sum_ARGg, na.rm = TRUE),
           # mean_DIGARG = mean(sum_dig_ARG, na.rm = TRUE),
            mean_HISg = mean(sum_HISg, na.rm = TRUE),
            mean_DIGHIS = mean(sum_dig_HIS, na.rm = TRUE))
  write.csv(here::here("data",
                      "Maleintakes.csv"))


#Femaleintakes <- Dailyintakes %>% filter(Sex==2) %>% 
  group_by(AgeGrp, Overall_Diet) %>% 
  summarise(mean_proteing = mean(sum_Proteing, na.rm = TRUE),
            mean_energy = mean(sum_energy, na.rm = TRUE),
            mean_TRPg = mean(sum_TRPg, na.rm = TRUE),
            mean_DIGTRP = mean(sum_dig_TRP, na.rm = TRUE),
            mean_THRg = mean(sum_THRg, na.rm = TRUE),
            mean_DIGTHR = mean(sum_dig_THR, na.rm = TRUE),
            mean_ILEg = mean(sum_ILEg, na.rm = TRUE),
            mean_DIGILE = mean(sum_dig_ILE, na.rm = TRUE),
            mean_LEUg = mean(sum_LEUg, na.rm = TRUE),
            mean_DIGLEU = mean(sum_dig_LEU, na.rm = TRUE),
            mean_LYSg = mean(sum_LYSg, na.rm = TRUE),
            mean_DIGLYS = mean(sum_dig_LYS, na.rm = TRUE),
            mean_SAAg = mean(sum_SAAg, na.rm = TRUE),
            mean_DIGSAA = mean(sum_dig_SAA, na.rm = TRUE),
            mean_AAAg = mean(sum_AAAg, na.rm = TRUE),
            mean_DIGAAA = mean(sum_dig_AAAg, na.rm = TRUE),
            mean_VALg = mean(sum_VALg, na.rm = TRUE),
            mean_DIGVAL = mean(sum_dig_VAL, na.rm = TRUE),
            mean_ARGg = mean(sum_ARGg, na.rm = TRUE),
            # mean_DIGARG = mean(sum_dig_ARG, na.rm = TRUE),
            mean_HISg = mean(sum_HISg, na.rm = TRUE),
            mean_DIGHIS = mean(sum_dig_HIS, na.rm = TRUE))

#  write.csv(here::here("data",
                       "Femaleintakes.csv"))


######################
# Average dietary supply of protein by top 20 SFG to adult population
NDNS_USDA_DIG %>% 
  #  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  
  select(FoodName, MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Overall_Diet) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()


###
#EXCLUDE INFANTS UNDER 5?
#EXCLUDE LOW ENERGY INTAKES?

NDNS_USDA_DIG$AgeR=as.character(as.numeric(NDNS_USDA_DIG$AgeR))

NDNS_USDA_DIG %>% #food consumption per person per day
  filter(AgeR > 5 ) %>%
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing, Overall_Diet) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG 
  group_by(SubFoodGroupDesc, SubFoodGroupCode, Overall_Diet) %>% 
  #summarise to get median protein per group 
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(Overall_Diet) %>% 
  #arrange by protein supply
  arrange(desc(Proteing),.by_group = TRUE) %>% 
  #select top 20
  slice_head(n=20) %>% 
  #mutate(test=factor(SubFoodGroupDesc, SubFoodGroupDesc)) %>%
  #mutate(Overall_Diet=reorder(Overall_Diet, Proteing, FUN = median)) %>% View()
  #view in plot SFG, Protein
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  #ggplot(aes(Proteing, test)) +
  geom_boxplot() +  
  labs(title = 'Average daily consumtpion of top 20 food groups supplying protein to dietary groups',
       x = 'Food Groups',
       y = 'Average Protein (g)') +
  facet_wrap(~Overall_Diet, scales = 'free_y') 



data$facet_var <- reorder(data$facet_var, -ave(data$value, data$facet_var, FUN = sum))

#####################################################
#FREQUENCY
# Should this be a table or graph? Or what other potential formats are there? 

library(forcats)

NDNS_USDA_DIG %>% #average food consumption per person per day
  #filter(Overall_Diet=='Neither') %>% 
  filter(as.numeric(AgeR) > 5 ) %>%
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing, Overall_Diet) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>% 
  #group by SFG
  group_by(SubFoodGroupDesc, SubFoodGroupCode, Overall_Diet) %>% 
  #sum no. of times each SFG consumed
  summarise(frequency = n(), .groups = 'drop') %>% 
  #group by diet group
  group_by(Overall_Diet) %>% 
  #arrange by descendig frequcny of consumption, by diet group
  arrange(desc(frequency), by_group = TRUE) %>% 
  #mutate 
  mutate(SubFoodGroupDesc = fct_reorder(SubFoodGroupDesc, frequency)) %>% 
  #view in plot SFG, Protein
  #select top 40
  slice_head(n=20)  %>%
  #ggplot
  ggplot(aes(SubFoodGroupDesc, frequency)) + 
  geom_boxplot() + coord_flip() + 
  labs(title = 'Frequency of SFG consumption by diet group',
       x = 'Food Groups',
       y = 'Frequency') + 
  facet_wrap(~Overall_Diet, scales = 'free') 

#### Issue with reordering in descending frequency


ndns4 %>% #average food consumption per person per day
  filter(as.numeric(AgeR) > 5 ) %>%
  #select variables of interest
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
         SubFoodGroupDesc, TotalGrams, Proteing, Overall_Diet) %>%
  #group by person, SFG 
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum by seriali and SFG 
  summarise(TotalProteing = sum(Proteing, na.rm = TRUE), .groups = 'drop') %>% 
  #summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
  #group by SFG 
  group_by(SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum total no. of people who have consumed
  summarise(total_frequency = n()) %>% View()




#####################

# Protein intakes by age/sex/dietary groups
str(Dailyintakes$sum_Proteing)

#MALE

Dailyintakes %>% filter(Sex == 1, AgeGrp == 'Infant') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Infant Boys by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()

Dailyintakes %>% filter(Sex == 1, AgeGrp == 'Child') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Boys by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()

Dailyintakes %>% filter(Sex == 1, AgeGrp == 'Adolescent') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Adolescent Boys by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()


Dailyintakes %>% filter(Sex == 1, AgeGrp == 'Adult') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Adult Men by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()

Dailyintakes %>% filter(Sex == 1, AgeGrp == 'Senior') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Senior Men by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()  




#FEMALE 

Dailyintakes %>% filter(Sex == 2, AgeGrp == 'Infant') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Infant Girls by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()

Dailyintakes %>% filter(Sex == 2, AgeGrp == 'Child') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Girls by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()

Dailyintakes %>% filter(Sex == 2, AgeGrp == 'Adolescent') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Adolescent Girls by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()

adultwomenEAR <- data.frame(yintercept = 46.2, label = "EAR")
adultwomenRNI <- data.frame(yintercept = 52.5, label = "RNI")


Dailyintakes %>% filter(Sex == 2, AgeGrp == 'Adult') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "black")+
  ylim(0, NA)+
  geom_hline(data = adultwomenEAR, aes(yintercept = yintercept, linetype = label), colour = "darkred", size = 1)+
  geom_hline(data = adultwomenRNI, aes(yintercept = yintercept, linetype = label), colour = "darkblue", size = 1)+
  scale_linetype_manual(name = "Reference", values = c("dashed", "dashed"))+
  labs(title = "Mean Daily Protein Intakes of Adult Women by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake (g)") + 
  theme_minimal()

Dailyintakes %>% filter(Sex == 2, AgeGrp == 'Senior') %>%
  ggplot(aes(x = Overall_Diet, y = sum_Proteing)) + 
  geom_boxplot(fill = "lightblue", colour = "darkblue")+
  ylim(0, NA)+
  labs(title = "Mean Daily Protein Intakes of Older Women by Dietary Group",
       x = "Dietary Group",
       y = "Mean Daily Protein Intake") + 
  theme_minimal()  

###############################################################
#Amino Acid Intakes (tables?) 

#Males
#Adult
Dailyintakes %>% filter(Sex==1 & AgeGrp == 'Senior') %>%
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()

#Adult females
Dailyintakes %>% filter(Sex==2 & AgeGrp == 'Senior') %>%
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()
#### Repeat for other age grps 

#Adolescent girls
Dailyintakes %>% filter(Sex==2 & AgeGrp == 'Adolescent') %>%
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()


