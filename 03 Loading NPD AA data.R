#Match NPD AA from paper to Food_match_AA
library(tidyr)

#View NPDs by frequency of consumption
ndns4 %>% filter(SubFoodGroupCode=='50E') %>% 
  group_by(FoodName) %>% 
  summarise(Frequency = n ()) %>% 
  arrange(desc(Frequency)) %>% View() 


#Load matched data
NPD_AA_match <- read.csv("NPD_Gorissen_matching.csv")
View(NPD_AA_match)

#Check consumption of foods without matches (low ranking items)
ndns %>% ungroup() %>% filter(FoodName=='PECTIN (DRY MIX)') %>%
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

View(NPD_AA_match)

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

#Match AA composition data 
NPD_merged <- NPDs_dryweight_only %>% left_join(NPD_AA_match, by = 'FoodNumber')






