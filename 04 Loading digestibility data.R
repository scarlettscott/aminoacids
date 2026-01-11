
#Load digestibility data (matched with NDNS top 60 and respective USDA matches)
Digest_matches <- read_csv("Matched_NDNS_USDA_DIAAS_22_04_2025.csv")

#Remove unnecessary columns and drop empty
Digest_matches <- Digest_matches %>% select(
  -`USDA Item Description`, -`NDB no`, 
  -`FDC Id`, -`Avg_protein, g/100g`, -`Water, g`) %>% 
  drop_na()

#Select only SFG Code and digestibility coefficients
Digest_coef <- Digest_matches %>% 
  select(SubFoodGroupCode, Protein_coef, TRP_coef, THR_coef, ILE_coef, LEU_coef, 
         LYS_coef, MET_coef, CYS_coef, PHE_coef, TYR_coef, VAL_coef, ARG_coef, HIS_coef)




