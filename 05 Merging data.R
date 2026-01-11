#Merging data

# Data: 
# NDNS consumption data = ndns4 
# USDA AA composition data = Avg_AA_60_SFG
# NPD AA composition data = NPD_merged
# Digestibility coefficients = Digest_coef


# 1) Merge USDA AA data with NDNS 

NDNS_USDA <- ndns4 %>% 
  left_join(Avg_AA_60_SFG, by = 'SubFoodGroupCode')


# 2) Merge NPD AA data 
#Merge NPD_merged with NDNS_USDA by foodnumber
NDNS_USDA_NPD <- NDNS_USDA %>% 
  left_join(NPD_merged, by = 'FoodNumber')

#Calculate true AA consumption
NDNS_USDA_NPD <- NDNS_USDA_NPD %>% 
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
NDNS_USDA_NPD_DIG <- NDNS_USDA_NPD_DIG %>% 
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
