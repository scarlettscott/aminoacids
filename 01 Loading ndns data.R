#Loading ndns data
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

# Checking main food groups supplying pro

ndns %>% 
  group_by(seriali, SurveyYear, DayNo, AgeR, Sex, Country,
           # DiaryDate, DayofWeek, DayNo, 
           FoodName, FoodNumber, SubFoodGroupCode,
           SubFoodGroupDesc, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  # ggplot(aes(forcats::fct_reorder(MainFoodGroupDesc, Proteing), Proteing)) + 
  ggplot(aes(forcats::fct_reorder(MainFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()


# Getting average consumption per person per food subgroup (top 60) (for matching)

ndns %>% #average food consumption per person per day
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
  arrange(desc(Proteing)) %>% ungroup() %>% View()
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
ndns %>% #average food consumption per person per day
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
#
#Preparing individual data for merging with ndns2:
ind2 <- ind %>% select(seriali,Veg, VegeChk, VeganChk, vegetarn)

str(ind2$seriali) # check if seriali number or character
# Converting variables into character
ind2$seriali <- as.character(ind2$seriali)


# Merging ind2 with ndns to obtain vegan info for each individual
ndns2 <- ndns %>%    
  left_join(ind2, by = "seriali")

#Checking main vegetarian food groups supplying protein
##Error 
ndns2 %>% filter(Veg==2) %>% #average consumption per vegetarian person per day 
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
DietChk <- ndns2 %>% 
  select(seriali, Veg, VegeChk, VeganChk, vegetarn, FoodName, FoodNumber, SubFoodGroupCode, SubFoodGroupDesc, MainFoodGroupCode, MainFoodGroupDesc)
#Vegans and vegetarians consuming dairy and meat products respectively. Recategorise into 'true' consumption categories

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
ndns2 <- ndns2 %>% mutate(
  Consumed_Meat =
    grepl(paste(meat_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(meat_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))

ndns2 <- ndns2 %>% mutate(
  Consumed_Fish = 
    grepl(paste(pesc_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(pesc_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))

ndns2 <- ndns2 %>% mutate(
  Consumed_DairyEgg = 
    grepl(paste(vegetarian_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(vegetarian_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))


#Info on reported dietary group and true/false intakes 
DietGrp <- ndns2 %>% select(seriali, DayNo, Consumed_Meat, Consumed_Fish, Consumed_DairyEgg)

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
# ) Individual dietary data of true dietary groups
ndns3 <- ndns2 %>% 
  left_join(DietGrp2, by = "seriali")

write.csv(here::here("data", 
                     "ndns_dietgroups.csv"))


# ) Demographic data

#Summarise by gender, age, geo location and household income 
ind3 <- ind %>% select(seriali, agegr1, Weight, region, GOR, eqv3)
ind3$seriali <- as.character(ind3$seriali)

#Inclusion of demographic data with individual data in ndns4
ndns4 <- ndns3 %>% left_join(ind3, by = "seriali")

demographics <- DietGrp_summary %>% 
  left_join(ind3, by = "seriali")
write.csv(here::here("data", 
                     "demographics.csv"))


# NDNS data with corrected dietary groups
