


# 1) View top 20 food groups supplying protein to each dietary group

# EXCLUDE INFANTS UNDER 5
#ADULTS ONLY, ARRANGE BY DIET GROUP AND DESC PROTEIN?
## Not in order of descending protein

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
       x = 'Average Protein (g)',
       y = 'Food Groups') +
  facet_wrap(~Overall_Diet, scales = 'free_y') 

ggsave(filename = "Plots/New Plots/SFG_protein.jpg")


data$facet_var <- reorder(data$facet_var, -ave(data$value, data$facet_var, FUN = sum))

#######################################################################
# 2) Average protein consumption per person per day
## Need to work out descending order. Not currently in descending order. 

NDNS_USDA_NPD %>%
  #filter(Overall_Diet=='Vegan') %>% 
  filter(as.numeric(AgeR) > 5 ) %>%
  select(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, 
         SubFoodGroupDesc, TotalGrams, Proteing, Overall_Diet) %>% 
  #group by person, food groups
  group_by(seriali, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum per SFG for each individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))%>% 
  #group by SFG 
  group_by(Overall_Diet, SubFoodGroupDesc, SubFoodGroupCode) %>% 
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
  labs(title = 'Average daily protein consumption from top 20 food groups supplying protein to dietary groups',
       x = 'Average Protein (g)',
       y = 'Food Groups') +
  facet_wrap(~Overall_Diet, scales = 'free_y') 

###############################################################################

#Daily consumption of protein per dietary group (population) 
completedata %>% filter(Overall_Diet=='Vegan') %>% View()
completedata$AgeR <- as.numeric(completedata$AgeR)

completedata %>% 
  filter(AgeR >= 5) %>% 
  #group by individuals, diet groups, food groups
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  #sum by SFG per individual
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  #group by SFG and diet group 
  group_by(SubFoodGroupDesc, SubFoodGroupCode, Overall_Diet) %>% 
  #summarise for median SFG per diet group
  summarise(across(where(is.numeric), ~median (.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  #group by diet group 
  group_by(Overall_Diet) %>% 
  #arrange by protein supply
  arrange(desc(Proteing), .by_group = TRUE) %>% slice_head(n=20) %>%
  #view in plot SFG, Protein
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  #ggplot(aes(Proteing, test)) +
  geom_boxplot() + coord_flip() +
  labs(title = 'Average daily consumtpion of top 20 food groups supplying protein to dietary groups',
       x = 'Food Groups',
       y = 'Average Protein (g)') +
  facet_wrap(~Overall_Diet, scales = 'free_y') 

Dailyintakes %>% filter(Overall_Diet == 'Vegetarian') %>% count()


#############################################################################################################
# 3) Mean daily intakes of protein and IAAs for adults 

NDNS_USDA_NPD_DIG %>% filter(seriali=='100101022') %>% View()


### Whole population 

completedata$Energykcal <- as.numeric(completedata$Energykcal)

#Calculate BMR ( for Goldberg cut offs, using Schofield equations)
# BMR = 𝑎⋅weight (kg)+ 𝑏, where a and b are relating to age / sex groups =

completedata <- completedata %>%
  mutate(
    BMR = case_when(
      Sex = 1 & AgeR >= 4 & AgeR <= 10  ~ 22.7 * Weight + 495,
      Sex = 2 & AgeR >= 4 & AgeR <= 10 ~ 22.5 * Weight + 499,
      
      Sex = 1 & AgeR > 10 & AgeR <= 18 ~ 17.5 * Weight + 651,
      Sex = 2 & AgeR > 10 & AgeR <= 18 ~ 12.2 * Weight + 746,
      
      Sex = 1 & AgeR > 18 & AgeR <= 64 ~ 11.6 * Weight + 879,
      Sex = 2 & AgeR > 18 & AgeR <= 64 ~ 8.7 * Weight + 829,
      
      Sex = 1 & AgeR > 64 ~ 13.5 * Weight + 487,
      Sex = 2 & AgeR > 64 ~ 10.5 * Weight + 596,
      
      TRUE ~ NA_real_
    )
  )


Dailyintakes <- completedata %>% #Daily intakes of protein and AA for population
  group_by(seriali, DayNo, AgeR, Sex, Overall_Diet, BMR) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_Proteing, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRPg, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRPg, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THRg, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILEg, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEUg, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYSg, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_METg, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYSg, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHEg, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYRg, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VALg, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            #sum_dig_ARG = sum(Digest_ARGmg, na.rm = TRUE),
            sum_HISg = sum(HISg, na.rm = TRUE),
            sum_dig_HIS = sum(Digest_HISg, na.rm = TRUE)) %>% ungroup()

# Will values with no AA composition data come up with a value of 0? This will change the mean intakes

#################################################################
# 4) Determine age groups

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

## Exclude infants

################################################################
# 5) Mean daily intakes by age/sex groups per dietary group

#Mean daily intakes of adult males per dietary group 
Dailyintakes %>% filter(Sex==1 & AgeGrp == 'Adult') %>%
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()

#Should this be median/mean?
# Repeat for other age/sex groups

#Notes: 0 values for daily intakes? 101103281 (water only), 900209122 (water only), 900801131 (water only). Could the survey have aligned with Ramadan? 
completedata %>% filter(seriali=='900803131') %>% View()

#####################################################################
# 6) Calculate Goldberg cut-offs to exclude low energy intakes

# Goldberg cut offs: Energy intake:Basal metabolic rate
#EI/BMR < 1.1 (underreporting) 
#EI/BMR > 2.4 (overreporting)

Dailyintakes <- Dailyintakes %>%
  mutate(EI_BMR = sum_energy / BMR)

# Can only look at distribution of population consumption rather than by SFG. 
#Need to calculate the average daily intake of each SFG per individual?

Dailyintakes %>% filter(EI_BMR > 1.1 & EI_BMR < 2.4) %>% View()

###########################################################################################
# 7) Digestible and total protein and amino acid intakes per sex and age groups excluding low intakes

#Excluding low energy intakes (1018 vs 2273 entries (male) and 1022 vs 3268 (female) - IS THIS A PROBLEM?)
#Male 
Dailyintakes %>% filter(Sex==1 & AgeGrp == 'Adult'& EI_BMR > 1.1 & EI_BMR < 2.4 ) %>% 
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()
#Female 
Dailyintakes %>% filter(Sex==2 & AgeGrp == 'Adult'& EI_BMR > 1.1 & EI_BMR < 2.4 ) %>% 
  group_by(Overall_Diet) %>% 
  summarise(across(starts_with("sum"),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}")) %>% View()


########################################################################
# 8)
#Determine average weights for adult men and women, elderly men and women, children, and adolescents to compare intakes to recommendations 

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


####################################################
# Average protein and amino acid intakes by increasing/decreasing consumption of ASFs (continuous)




#####################################################
#Below is additional information that we may/may not want to include

#FREQUENCY
# Should this be a table or graph? Or other?

library(forcats)

completedata %>% #average food consumption per person per day
  #filter(Overall_Diet=='Neither') %>% 
  filter(AgeR >= 5 ) %>%
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

#### Issue with reordering in descending frequency, same as consumption above


################################################
# Protein intakes by age/sex/dietary groups
str(Dailyintakes$sum_Proteing)

#MALE


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
  #geom_hline(data = adultwomenEAR, aes(yintercept = yintercept, linetype = label), colour = "darkred", size = 1)+
  #geom_hline(data = adultwomenRNI, aes(yintercept = yintercept, linetype = label), colour = "darkblue", size = 1)+
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

# Equivalised Household Income Tertiles by Geographic Region

demographics$eqv3 <- factor(demographics$eqv3)

demographics$GOR <- factor(demographics$GOR,
                           levels = 1:12,
                           labels = c("North East", "North West", "Yorkshire and The Humber",
                                      "East Midlands", "West Midlands", "East of England",
                                      "London", "South East", "South West",
                                      "Wales", "Scotland", "Northern Ireland"))


demographics %>%
  count(GOR, eqv3) %>% 
  #summarise(n_people = n()) %>% 
  ggplot(aes(x = GOR, y = n, fill = eqv3)) + 
  geom_col(position = "dodge") + 
  labs(title = "Distribution of Equivalised Household Income Tertiles by Location",
       x = "Location",
       y = "Number of People") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))
  
# Protein intakes (digestible) by location 


# Protein intakes (digestible) by income tertile
