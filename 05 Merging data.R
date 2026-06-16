#Merging data

# Data: 
# NDNS consumption data = ndns4 
# USDA AA composition data = Avg_AA_60_SFG
# NPD AA composition data = NPD_merged
# Digestibility coefficients = Digest_coef


# 1) Merge USDA AA data with NDNS 
ndns4$TotalGrams <- as.numeric(ndns4$TotalGrams)
ndns4$Waterg <- as.numeric(ndns4$Waterg)
ndns4$Proteing <- as.numeric(ndns4$Proteing)

NDNS_USDA <- ndns4 %>% 
  left_join(Avg_AA_60_SFG, by = 'SubFoodGroupCode')


# 2) Merge NPD AA data 
#Merge NPD AA data with NDNS_USDA by FoodNumber

#Calculate dry weight and protein per g for just NPDs
NPDs_dryweight <- NDNS_USDA %>%
  mutate(
    dry_weight = if_else(SubFoodGroupCode == "50E",
                         TotalGrams - Waterg, NA_real_),
    Protein_g1g = if_else(SubFoodGroupCode == "50E",
                          Proteing/dry_weight, NA_real_))
#Check
NPDs_dryweight %>% filter(SubFoodGroupCode=='50E') %>% View()

#Match AA composition data 
NPD_merged <- NPDs_dryweight %>% left_join(NPD_AA_match, by = 'FoodNumber')

#Check
NPD_merged %>% filter(SubFoodGroupCode=='50E') %>% View()

#Calculate true AA consumption 
NDNS_USDA_NPD <- NPD_merged %>% 
  mutate(
  LEUg = (Protein_g1g * Leucine_g1g.protein),
  ILEg = (Protein_g1g * Isoleucine_g1g.protein),
  VALg = (Protein_g1g * Valine_g1g.protein),
  LYSg = (Protein_g1g * Lysine_g1g.protein),
  METg = (Protein_g1g * Methionine_g1g.protein),
  HISg = (Protein_g1g * Histidine_g1g.protein),
  PHEg = (Protein_g1g * Phenylalanine_g1g.protein),
  THRg = (Protein_g1g * Threonine_g1g.protein))

#Check 
NDNS_USDA_NPD %>% filter(SubFoodGroupCode=='50E') %>% View()


# 3) Merge digestibility coefficients with NDNS_USDA_NPD data

NDNS_USDA_NPD_DIG <- NDNS_USDA_NPD %>% 
  left_join(Digest_coef, by = 'SubFoodGroupCode')

#Multiply digestibility coefficients with composition data to obtain digestible AA composition 
completedata <- NDNS_USDA_NPD_DIG %>% 
  mutate(
    Digest_Proteing = Protein_coef * Proteing,
    Digest_TRPg = TRP_coef * TRPg,
    Digest_THRg = THR_coef * THRg, 
    Digest_ILEg = ILE_coef * ILEg,
    Digest_LEUg = LEU_coef * LEUg,
    Digest_LYSg = LYS_coef * LYSg,
    Digest_METg = MET_coef * METg,
    Digest_CYSg = CYS_coef * CYSg,   
    Digest_PHEg = PHE_coef * PHEg,
    Digest_TYRg = TYR_coef * TYRg,
    Digest_VALg = VAL_coef * VALg,
    Digest_HISg = HIS_coef * HISg
  )
