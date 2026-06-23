#Match NPD AA from paper to Food_match_AA
library(tidyr)
library(readr)

# Loading the data (or you could load the script for generating the data)

ndns3 <- read.csv(here::here("data", "inter-outputs",
                            "ndns_dietgroups.csv"))
#Loading individual data
ind <- readr::read_delim(here::here("data", "tab", "ndns_rp_yr9-11a_indiv_20211020.tab"),
                         delim = "\t")


#Summarise by gender, age, geo location and household income 
ind3 <- ind %>% select(seriali, agegr1, Weight, region, GOR, eqv3)
#ind3$seriali <- as.character(ind3$seriali) # They are both integer

#Inclusion of demographic data with individual data in ndns4
ndns4 <- ndns3 %>% left_join(ind3, by = "seriali")

#View NPDs by frequency of consumption 
ndns4 %>% filter(SubFoodGroupCode=='50E') %>% 
  group_by(FoodName) %>% 
  summarise(Frequency = n ()) %>% 
  arrange(desc(Frequency)) %>% View() 


#Load matched data
NPD_AA_match <- read.csv(here::here("data", "NPD_Gorissen_matching.csv"))
#View(NPD_AA_match)

#Check consumption of foods without matches (low ranking items)
ndns %>% ungroup() %>% filter(FoodName=='PECTIN (DRY MIX)') %>% # Is this the "original" ndns dataset?
  select(seriali, TotalGrams, Proteing) %>% View()
#never more than 1.6g total


ndns %>% ungroup() %>% filter(FoodName=='BELEAN HIGH PROTEIN SHAKE') %>%
  select(seriali, TotalGrams, Proteing) %>% View()
# one person, once. 202.5g total, 14.38g protein


#Screen out low ranking items
#Reasoning: 
  # Pectin - never more that 1.6g total. 
  # BELEAN = one person had once. 50g total, 3.6g protein. No protein type info. 
  # Fybogel = not protein
NPD_AA_match <- NPD_AA_match %>% 
  drop_na()

#Match NPD AA info to NDNS consumption
# Converting variables into character
NPD_AA_match$FoodNumber <- as.character(NPD_AA_match$FoodNumber)

#View(NPD_AA_match)

#Remove food name to match with ndns data using food number 
NPD_AA_match <- NPD_AA_match %>% select(-FoodName)


#####################
#Preparing to merge with ndns

ndns4$TotalGrams <- as.numeric(ndns4$TotalGrams)
ndns4$Waterg <- as.numeric(ndns4$Waterg)
ndns4$Proteing <- as.numeric(ndns4$Proteing)

#Calculate dry weight and protein per g for just NPDs
NPDs_dryweight <- ndns4 %>% filter(SubFoodGroupCode=='50E') %>% 
  mutate(
    NPD = SubFoodGroupCode == "50E",
    dry_weight = if_else(NPD, TotalGrams - Waterg, NA_real_),
    Protein_g1g = if_else(NPD, Proteing/dry_weight, NA_real_))

#Select variables for merging with NPD compositions
NPDs_dryweight_only <- NPDs_dryweight %>% select(
  FoodNumber, dry_weight, Protein_g1g)

NPDs_dryweight_only$FoodNumber <- as.character(NPDs_dryweight_only$FoodNumber)

#Match AA composition data 
NPD_merged <- NPDs_dryweight_only %>% left_join(NPD_AA_match, by = 'FoodNumber')






