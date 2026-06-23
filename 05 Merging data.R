#Merging data

# Data: # Add the script where they are "generated" for ref. and/or load the data
# NDNS consumption data = ndns4 (script 01)
# USDA AA composition data = Avg_AA_60_SFG (script 02)
# NPD AA composition data = NPD_merged (script 03)
# Digestibility coefficients = Digest_coef (script 04)


# 1) Merge USDA AA data with NDNS 

ndns4 |> count(MainFoodGroupDesc, SubFoodGroupCode)

NDNS_USDA <- ndns4 %>% 
  left_join(Avg_AA_60_SFG, by = 'SubFoodGroupCode')

length(unique(NDNS_USDA$MainFoodGroupCode))

NDNS_USDA |> filter(Proteing>1) |> 
  group_by(MainFoodGroupCode,MainFoodGroupDesc) |> 
  summarise(Leu_T = mean(LEUg, na.rm = TRUE), 
            cons = mean(TotalGrams, na.rm = TRUE),
            prot = mean(Proteing, na.rm = TRUE)) |> 
  filter(is.na(Leu_T)) |> arrange(desc(prot))

sum(!is.na(NDNS_USDA$LEUg))
sum(is.na(NDNS_USDA$Proteing))

NDNS_USDA |> filter(SubFoodGroupCode == "1R")
NDNS_USDA |> filter(MainFoodGroupCode == "51") |> 
  distinct(SubFoodGroupDesc)

NDNS_USDA |> filter(MainFoodGroupCode == "51" & Proteing >1) 

NDNS_USDA |> filter(MainFoodGroupCode == "51" & 
                      grepl("MILK", FoodName))  |>
  filter(!grepl("NO MILK", FoodName)) |> 
  distinct(FoodName)

class(NDNS_USDA$Proteing)

NDNS_USDA |> filter(is.na(LEUg) & Proteing>1) |> 
  count(SubFoodGroupCode) |> arrange(desc(n))


NDNS_USDA$FoodNumber <- as.character(NDNS_USDA$FoodNumber)
# 2) Merge NPD AA data 
#Merge NPD_merged with NDNS_USDA by foodnumber
NDNS_USDA_NPD <- NDNS_USDA %>% 
  left_join(NPD_merged, by = 'FoodNumber')

#Calculate true AA consumption - This is wrong, you are overwriting LEUg
NDNS_USDA_NPD <- NDNS_USDA_NPD %>% 
  mutate(
  LEUg = ifelse(is.na(LEUg), (Protein_g1g * Leucine_g1g.protein), LEUg),
  ILEg = ifelse(is.na(ILEg)(Protein_g1g * Isoleucine_g1g.protein),ILEg),
  VALg = ifelse(is.na(VALg)(Protein_g1g * Valine_g1g.protein),VALg),
  LYSg = ifelse(is.na(LYSg)(Protein_g1g * Lysine_g1g.protein),LYSg),
  METg = ifelse(is.na(METg)(Protein_g1g * Methionine_g1g.protein),METg),
  HISg = ifelse(is.na(HISg)(Protein_g1g * Histidine_g1g.protein),HISg),
  PHEg = ifelse(is.na(PHEg)(Protein_g1g * Phenylalanine_g1g.protein),PHEg),
  THRg = ifelse(is.na(THRg)(Protein_g1g * Threonine_g1g.protein), THRg))



#Check 
#NDNS_USDA_NPD %>% filter(SubFoodGroupCode=='50E') %>% View()
sum(!is.na(NDNS_USDA$LEUg))
sum(!is.na(NDNS_USDA_NPD$LEUg))


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

