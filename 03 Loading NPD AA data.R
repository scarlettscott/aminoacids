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

#Remove food name to match with ndns datat using food number 
NPD_AA_match <- NPD_AA_match %>% select(-FoodName)

#Match this NPD AA data to ndns consumption data along with the USDA data then multiply AA (g1g protein) by dry weight Protein_g1g for each intake (see below)


#####################


ndns4$TotalGrams <- as.numeric(ndns4$TotalGrams)
ndns4$Waterg <- as.numeric(ndns4$Waterg)
ndns4$Proteing <- as.numeric(ndns4$Proteing)

#Calculate dry weight and protein per g for just NPDs
AA_NDNS <- ndns4 %>%
  mutate(
    NPD = SubFoodGroupCode == "50E",
    dry_weight = if_else(NPD, TotalGrams - Waterg, NA_real_),
    Protein_g1g = if_else(NPD, Proteing/dry_weight, NA_real_))

#Match AA composition data 
AA_NPD_NDNS_match <- AA_NDNS %>% left_join(NPD_AA_match, by = 'FoodNumber')

#Calculate true AA consumption
AA_NPD_NDNS_match <- AA_NPD_NDNS_match %>% mutate(
  LEUg = (Protein_g1g * Leucine_g1g.protein),
  ILEg = (Protein_g1g * Isoleucine_g1g.protein),
  VALg = (Protein_g1g * Valine_g1g.protein),
  LYSg = (Protein_g1g * Lysine_g1g.protein),
  METg = (Protein_g1g * Methionine_g1g.protein),
  HISg = (Protein_g1g * Histidine_g1g.protein),
  PHEg = (Protein_g1g * Phenylalanine_g1g.protein),
  THRg = (Protein_g1g * Threonine_g1g.protein))

#Check AA conetnts calculated for NPDs
AA_NPD_NDNS_match %>% filter(SubFoodGroupCode=='50E') %>% View()

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