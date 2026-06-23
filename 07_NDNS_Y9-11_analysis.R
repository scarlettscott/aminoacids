

# Loading the libraries
library(reshape)
library(survey)
library(car)

# Read in the Y1-4, Y5-6, Y7-8 and Y9-11 'person level dietary data' files and combine into Y1-11 person level dietary dataset
# Read in the Y1-4, Y5-6, Y7-8 and Y9-11 'indiv' files and combine into Y1-11 indiv dataset


# Merge these together to form single dataset called 'FoodNutrientY1_11'

names(FoodNutrientY1_11)

# This combined Y1-11 dataset will contain the following design variables:

# point - Primary Sampling Unit (also named area in archived dataset)
# strata - stratification level (also named cluster in archived datasets)
# wti_Y1234567891011 - survey weights (combination of wti_UKY1234, wti_Y56, wti_Y78, wti_Y911)
#summary(FoodNutrientY1_11$point)
#summary(FoodNutrientY1_11$strata)
#summary((FoodNutrientY1_11$wti_Y1234567891011))
summary(FoodNutrientY1_11$Area) # PSU
summary((FoodNutrientY1_11$wti_Y911)) # Survey weight
summary((FoodNutrientY1_11$astrata1))  # Strata 1-5
summary((FoodNutrientY1_11$astrata2))  # Strata 1-5

hist(FoodNutrientY1_11$Age)


##############################################################
#    Create age/sex group variable for descriptive tables    #
##############################################################

# Boys 4-10
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 4 & FoodNutrientY1_11$Age <= 10 & FoodNutrientY1_11$Sex == 1] = 1
# Girls 4-10
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 4 & FoodNutrientY1_11$Age <= 10 & FoodNutrientY1_11$Sex == 2] = 2
# Boys 11-18
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 11 & FoodNutrientY1_11$Age <= 18 & FoodNutrientY1_11$Sex == 1] = 3
# Girls 11-18
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 11 & FoodNutrientY1_11$Age <= 18 & FoodNutrientY1_11$Sex == 2] = 4
# Men 19-64
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 19 & FoodNutrientY1_11$Age <= 64 & FoodNutrientY1_11$Sex == 1] = 5
# Women 19-64
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 19 & FoodNutrientY1_11$Age <= 64 & FoodNutrientY1_11$Sex == 2] = 6
# Men 65+
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 65 & FoodNutrientY1_11$Sex == 1] = 7
# Women 65+
FoodNutrientY1_11$repgr[FoodNutrientY1_11$Age >= 65 & FoodNutrientY1_11$Sex == 2] = 8

table(FoodNutrientY1_11$repgr)

# Women childbearing age 16-49
FoodNutrientY1_11$wcb[FoodNutrientY1_11$Age >= 16 & FoodNutrientY1_11$Age <= 49 & FoodNutrientY1_11$Sex == 2] = 1

FoodNutrientY1_11$sexcombagegrps[FoodNutrientY1_11$Age >= 1 & FoodNutrientY1_11$Age <= 3] = 1  #PSC
FoodNutrientY1_11$sexcombagegrps[FoodNutrientY1_11$Age >= 4 & FoodNutrientY1_11$Age <= 10] = 2 #SAC
FoodNutrientY1_11$sexcombagegrps[FoodNutrientY1_11$Age >= 11 & FoodNutrientY1_11$Age <= 18] = 3 #Adolescents
FoodNutrientY1_11$sexcombagegrps[FoodNutrientY1_11$Age >= 19 & FoodNutrientY1_11$Age <= 64] = 4 #Adults
FoodNutrientY1_11$sexcombagegrps[FoodNutrientY1_11$Age >= 65] = 5 # Older adults 

table(FoodNutrientY1_11$sexcombagegrps)

FoodNutrientY1_11$sexcombagegrps <- as.factor(FoodNutrientY1_11$sexcombagegrps)

###########################################################
# Create variable for combined survey years for reporting #
###########################################################

#FoodNutrientY1_11$yeargr[FoodNutrientY1_11$SurveyYear==1 |  FoodNutrientY1_11$SurveyYear==2] = 1
#FoodNutrientY1_11$yeargr[FoodNutrientY1_11$SurveyYear==3 |  FoodNutrientY1_11$SurveyYear==4] = 2
#FoodNutrientY1_11$yeargr[FoodNutrientY1_11$SurveyYear==5 |  FoodNutrientY1_11$SurveyYear==6] = 3
#FoodNutrientY1_11$yeargr[FoodNutrientY1_11$SurveyYear==7 |  FoodNutrientY1_11$SurveyYear==8] = 4
#FoodNutrientY1_11$yeargr[FoodNutrientY1_11$SurveyYear==9 |  FoodNutrientY1_11$SurveyYear==10 |  FoodNutrientY1_11$SurveyYear==11] = 5

###############################################################
# Create variables for age/sex group by combined survey years #
###############################################################

#FoodNutrientY1_11$grouprep = ifelse( is.na(FoodNutrientY1_11$repgr)==T , NA ,paste0(FoodNutrientY1_11$repgr, FoodNutrientY1_11$yeargr ) )
#FoodNutrientY1_11$grouptotrep = paste0( FoodNutrientY1_11$sexcombagegrps,FoodNutrientY1_11$yeargr )
#FoodNutrientY1_11$grouprepnum = as.numeric(FoodNutrientY1_11$grouprep)
#FoodNutrientY1_11$grouptotrepnum = as.numeric(FoodNutrientY1_11$grouptotrep)
#
#FoodNutrientY1_11$group.wcb = ifelse( is.na(FoodNutrientY1_11$wcb)==T , NA , paste0(FoodNutrientY1_11$wcb, FoodNutrientY1_11$yeargr ) )
#FoodNutrientY1_11$group.wcbnum = as.numeric(FoodNutrientY1_11$group.wcb)


#######################################
###       Collapse stratas          ###
#######################################

FoodNutrientY1_11$strata = car::recode(FoodNutrientY1_11$astrata1, "2036=2037")
FoodNutrientY1_11$strata = car::recode(FoodNutrientY1_11$astrata1, "3028=3027")
FoodNutrientY1_11$strata = car::recode(FoodNutrientY1_11$astrata1, "3082=3083")
FoodNutrientY1_11$strata = car::recode(FoodNutrientY1_11$astrata1, "2053=2052")


#######################################
###       Recode variables          ###
#######################################

#vFoodNutrientY1_11$Achiee5 = car::recode(FoodNutrientY1_11$Achieve5, "2=0")

#  need to account for -5 and na's
#FoodNutrientY1_11$Achieve5 = ifelse( is.na(FoodNutrientY1_11$Achieve5)==T | FoodNutrientY1_11$Achieve5 == -5,0,FoodNutrientY1_11$Achieve5 )
#FoodNutrientY1_11$FruitJuiceg100percent = ifelse( is.na(FoodNutrientY1_11$FruitJuiceg100percent)==T,0,FoodNutrientY1_11$FruitJuiceg100percent )
#FoodNutrientY1_11$Totfruitvegportions = ifelse( is.na(FoodNutrientY1_11$Totfruitvegportions)==T | FoodNutrientY1_11$Totfruitvegportions == -5,0,FoodNutrientY1_11$Totfruitvegportions )


#########################################################
###       Free sugars and AOAC blo variables          ###
#########################################################

#FoodNutrientY1_11$FreesugarspctotE.blo = ifelse(FoodNutrientY1_11$FreesugarspctotE<=5,1,0)

FoodNutrientY1_11$AOACfibreg.blo = ifelse(FoodNutrientY1_11$AOACFibreg>=15 & FoodNutrientY1_11$Age<=4,1,
  ifelse(FoodNutrientY1_11$AOACFibreg<15 & FoodNutrientY1_11$Age<=4,0,
  ifelse(FoodNutrientY1_11$AOACFibreg>=20 & FoodNutrientY1_11$Age>=5 & FoodNutrientY1_11$Age<=10,1,
  ifelse(FoodNutrientY1_11$AOACFibreg<20 & FoodNutrientY1_11$Age>=5 & FoodNutrientY1_11$Age<=10,0,
  ifelse(FoodNutrientY1_11$AOACFibreg>=25 & FoodNutrientY1_11$Age>=11 & FoodNutrientY1_11$Age<=15,1,
  ifelse(FoodNutrientY1_11$AOACFibreg<25 & FoodNutrientY1_11$Age>=11 & FoodNutrientY1_11$Age<=15,0,
  ifelse(FoodNutrientY1_11$AOACFibreg>=30 & FoodNutrientY1_11$Age>=16,1,
  ifelse(FoodNutrientY1_11$AOACFibreg<30 & FoodNutrientY1_11$Age>=16,0,NA))))))))

# Fibre scenario3 below threshold

FoodNutrientY1_11$scena3fibreg.blo = ifelse(FoodNutrientY1_11$fibre_scenario3>=15 & FoodNutrientY1_11$Age<=4,1,
ifelse(FoodNutrientY1_11$fibre_scenario3<15 & FoodNutrientY1_11$Age<=4,0,
ifelse(FoodNutrientY1_11$fibre_scenario3>=20 & FoodNutrientY1_11$Age>=5 & FoodNutrientY1_11$Age<=10,1,
ifelse(FoodNutrientY1_11$fibre_scenario3<20 & FoodNutrientY1_11$Age>=5 & FoodNutrientY1_11$Age<=10,0,
ifelse(FoodNutrientY1_11$fibre_scenario3>=25 & FoodNutrientY1_11$Age>=11 & FoodNutrientY1_11$Age<=15,1,
ifelse(FoodNutrientY1_11$fibre_scenario3<25 & FoodNutrientY1_11$Age>=11 & FoodNutrientY1_11$Age<=15,0,
ifelse(FoodNutrientY1_11$fibre_scenario3>=30 & FoodNutrientY1_11$Age>=16,1,
ifelse(FoodNutrientY1_11$fibre_scenario3<30 & FoodNutrientY1_11$Age>=16,0,NA))))))))

########################################################################
###     Replace missing with zero for the blo RNI variables          ###
########################################################################

#class(FoodNutrientY1_11$bloVitAlrni)
#
#FoodNutrientY1_11$bloVitAlrni = ifelse( is.na(FoodNutrientY1_11$bloVitAlrni)==T , 0  ,FoodNutrientY1_11$bloVitAlrni  )
#FoodNutrientY1_11$bloRibolrni = ifelse( is.na(FoodNutrientY1_11$bloRibolrni)==T , 0  ,FoodNutrientY1_11$bloRibolrni  )
#FoodNutrientY1_11$bloFollrni = ifelse( is.na(FoodNutrientY1_11$bloFollrni)==T , 0  ,FoodNutrientY1_11$bloFollrni  )
#FoodNutrientY1_11$bloFollrniplussupps = ifelse( is.na(FoodNutrientY1_11$bloFollrniplussupps)==T , 0  ,FoodNutrientY1_11$bloFollrniplussupps  )
#FoodNutrientY1_11$PCRNIVitD = ifelse(is.na(FoodNutrientY1_11$PCRNIVitD)==T , 0  ,FoodNutrientY1_11$PCRNIVitD)
#FoodNutrientY1_11$bloCalciumlrni = ifelse( is.na(FoodNutrientY1_11$bloCalciumlrni)==T , 0  ,FoodNutrientY1_11$bloCalciumlrni  )
#FoodNutrientY1_11$bloMglrni = ifelse( is.na(FoodNutrientY1_11$bloMglrni)==T , 0  ,FoodNutrientY1_11$bloMglrni  )
#FoodNutrientY1_11$bloPotassiumlrni = ifelse( is.na(FoodNutrientY1_11$bloPotassiumlrni)==T , 0  ,FoodNutrientY1_11$bloPotassiumlrni  )
#FoodNutrientY1_11$bloIodinelrni = ifelse( is.na(FoodNutrientY1_11$bloIodinelrni)==T , 0  ,FoodNutrientY1_11$bloIodinelrni  )
#FoodNutrientY1_11$bloSeleniumlrni = ifelse( is.na(FoodNutrientY1_11$bloSeleniumlrni)==T , 0  ,FoodNutrientY1_11$bloSeleniumlrni  )
#FoodNutrientY1_11$bloZinclrni = ifelse( is.na(FoodNutrientY1_11$bloZinclrni)==T , 0  ,FoodNutrientY1_11$bloZinclrni  )
#FoodNutrientY1_11$bloIronlrni = ifelse( is.na(FoodNutrientY1_11$bloIronlrni)==T, 0, FoodNutrientY1_11$bloIronlrni   )

##############################################
###       Alcohol binary variable          ###
##############################################
#FoodNutrientY1_11$DAlcohol = ifelse( FoodNutrientY1_11$Alcoholg > 0, 1 ,0 )

#Saving dataset

#saveRDS(FoodNutrientY1_11, here::here("inter-output", "FoodNutrientY1_11.rds"))

#FoodNutrientY1_11 <- readRDS(here::here("inter-output", "FoodNutrientY1_11.rds"))
#########################################
###       Specify survey design       ###
#########################################

FoodNutrientY111design = svydesign(id=~Area,
                        strata =~strata, weights=~wti_Y911,data=FoodNutrientY1_11,nest=TRUE)


# histogram

#Fibre intake
title <- "Fibre weighted \n (refine/unrefined)"
title <- "Fibre weighted"

svyhist(~AOACFibreg, FoodNutrientY111design, main=title,  col="#00880020", xlab = "Fibre (g/day)")
svyhist(~fibre_scenario1, FoodNutrientY111design, main="Fibre weighted", col="#88000020",add=TRUE)
legend("topright", c("Fibre", "Fibre scenario"),  fill=c("#00880020", "#88000020"))


#Fibre intake from wheat perc
#title <- "Fibre weighted \n (refine/unrefined)"
title <- "Fibre from wheat weighted"

svyhist(~wheat_perc0, FoodNutrientY111design, main=title,  col="#00880020", xlab = "(%) of fibre from wheat")
svyhist(~wheat_perc3, FoodNutrientY111design, main="", col="#88000020",add=TRUE)
legend("topright", c("Fibre", "Fibre scenario"),  fill=c("#00880020", "#88000020"))


# Age
svyhist(~Age, FoodNutrientY111design, main="Age weighted")


svyboxplot(AOACFibreg~sexcombagegrps,FoodNutrientY111design, main=title,  col="#00880020", xlab = "Fibre (g/day)")
svyboxplot(fibre_scenario1~sexcombagegrps,FoodNutrientY111design,  col="#88000020",add=TRUE)

########################################################
# Create descriptive statistics required for table 3.4 #
########################################################

##  get the mean and  s.e. of mean by groups  ##
table_1 <- svyby(~AOACFibreg,~Sex,FoodNutrientY111design,svymean,na.rm=T)
table_1[, c(4:5)] <- svyby(~fibre_scenario3,~Sex,FoodNutrientY111design,svymean,na.rm=T)[, c(2:3)]
table_1$Sex[table_1$Sex == "1"] <- "Male"
table_1$Sex[table_1$Sex == "2"] <- "Female"
names(table_1) <- c("Variable", "Mean_fibre", "SE_fibre", "Mean_scenario", "SE_scenario" )

table_2 <- svyby(~AOACFibreg,~sexcombagegrps,FoodNutrientY111design,svymean,na.rm=T)
table_2[, c(4:5)] <- svyby(~fibre_scenario3,~sexcombagegrps,FoodNutrientY111design,svymean,na.rm=T)[, c(2:3)]
table_2$sexcombagegrps <- as.character(table_2$sexcombagegrps)
table_2$sexcombagegrps[table_2$sexcombagegrps == "1"] <- "PSC"
table_2$sexcombagegrps[table_2$sexcombagegrps == "2"] <- "SAC"
table_2$sexcombagegrps[table_2$sexcombagegrps == "3"] <- "Adolescents"
table_2$sexcombagegrps[table_2$sexcombagegrps == "4"] <- "Adults"
table_2$sexcombagegrps[table_2$sexcombagegrps == "5"] <- "Older Adults"
names(table_2) <- c("Variable", "Mean_fibre", "SE_fibre", "Mean_scenario", "SE_scenario" )

table_3 <- svyby(~AOACFibreg,~nssec8,FoodNutrientY111design,svymean,na.rm=T)
table_3[, c(4:5)] <- svyby(~fibre_scenario3,~nssec8,FoodNutrientY111design,svymean,na.rm=T)[, c(2:3)]
names(table_3) <- c("Variable", "Mean_fibre", "SE_fibre", "Mean_scenario", "SE_scenario" )

table_summary1 <- data.table::rbindlist(list( table_1, table_2, table_3), use.names = T, fill = T)

xlsx::write.xlsx(table_summary1, here::here("output", "summary-table_v.1.0.0.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

##   get the median, upper2.5 and lower2.5 percentile by groups  ##
svyby(~AOACFibreg,~Sex,FoodNutrientY111design,svyquantile, quantiles=c(0.5,0.025,0.975),na.rm=T,keep.var=FALSE)
svyby(~fibre_scenario1,~sexcombagegrps,FoodNutrientY111design,svyquantile, quantiles=c(0.5,0.025,0.975),na.rm=T,keep.var=FALSE)

##  get the st.dev by groups  ##
sqrt(svyby(~Saturatedfattyacidsg,~grouprepnum,FoodNutrientY111design,svyvar,na.rm=T))
sqrt(svyby(~Saturatedfattyacidsg,~grouptotrepnum,FoodNutrientY111design,svyvar,na.rm=T))


## get the mean of binary variables as percentage (meeting recommendations)
table_4 <- svyby(~AOACfibreg.blo,~Sex,FoodNutrientY111design,svymean, na.rm=T)
table_4[, c(4:5)] <- svyby(~scena3fibreg.blo,~Sex,FoodNutrientY111design,svymean, na.rm=T)[, c(2:3)]
table_4$Sex[table_4$Sex == "1"] <- "Male"
table_4$Sex[table_4$Sex == "2"] <- "Female"
names(table_4) <- c("Variable", "Perc_recomfibre", "SE_fibre", "Perc_recomscena3", "SE_scenario" )

table_5 <- svyby(~AOACfibreg.blo,~sexcombagegrps,FoodNutrientY111design,svymean, na.rm=T)
table_5[, c(4:5)] <- svyby(~scena3fibreg.blo,~sexcombagegrps,FoodNutrientY111design,svymean, na.rm=T)[, c(2:3)]
table_5$sexcombagegrps <- as.character(table_5$sexcombagegrps)
table_5$sexcombagegrps[table_5$sexcombagegrps == "1"] <- "PSC"
table_5$sexcombagegrps[table_5$sexcombagegrps == "2"] <- "SAC"
table_5$sexcombagegrps[table_5$sexcombagegrps == "3"] <- "Adolescents"
table_5$sexcombagegrps[table_5$sexcombagegrps == "4"] <- "Adults"
table_5$sexcombagegrps[table_5$sexcombagegrps == "5"] <- "Older Adults"
names(table_5) <- c("Variable", "Perc_recomfibre", "SE_fibre", "Perc_recomscena3", "SE_scenario" )

table_6 <- svyby(~AOACfibreg.blo,~nssec8,FoodNutrientY111design,svymean, na.rm=T)
table_6[, c(4:5)] <- svyby(~scena3fibreg.blo,~nssec8,FoodNutrientY111design,svymean, na.rm=T)[, c(2:3)]
names(table_6) <- c("Variable", "Perc_recomfibre", "SE_fibre", "Perc_recomscena3", "SE_scenario" )

table_summary2 <- data.table::rbindlist(list( table_4, table_5, table_6), use.names = T, fill = T)

table_summary2[, c(2:5)] <- 100*table_summary2[, c(2:5)]

xlsx::write.xlsx(table_summary2, here::here("output", "summary-table_v.1.0.0.xlsx"), 
                 sheetName="Sheet2", 
                 col.names=TRUE, row.names=FALSE, append=TRUE)


## get the mean of binary variables as percentage (fibre from wheat products)
table_7 <- svyby(~wheat_perc0,~Sex,FoodNutrientY111design,svymean, na.rm=T)
table_7[, c(4:5)] <- svyby(~wheat_perc3,~Sex,FoodNutrientY111design,svymean, na.rm=T)[, c(2:3)]
table_7$Sex[table_7$Sex == "1"] <- "Male"
table_7$Sex[table_7$Sex == "2"] <- "Female"
names(table_7) <- c("Variable", "Perc_wheatfibre", "SE_fibre", "Perc_wheatfibrescena3", "SE_scenario" )

table_8 <- svyby(~wheat_perc0,~sexcombagegrps,FoodNutrientY111design,svymean, na.rm=T)
table_8[, c(4:5)] <- svyby(~wheat_perc3,~sexcombagegrps,FoodNutrientY111design,svymean, na.rm=T)[, c(2:3)]
table_8$sexcombagegrps <- as.character(table_8$sexcombagegrps)
table_8$sexcombagegrps[table_8$sexcombagegrps == "1"] <- "PSC"
table_8$sexcombagegrps[table_8$sexcombagegrps == "2"] <- "SAC"
table_8$sexcombagegrps[table_8$sexcombagegrps == "3"] <- "Adolescents"
table_8$sexcombagegrps[table_8$sexcombagegrps == "4"] <- "Adults"
table_8$sexcombagegrps[table_8$sexcombagegrps == "5"] <- "Older Adults"
names(table_8) <- c("Variable", "Perc_wheatfibre", "SE_fibre", "Perc_wheatfibrescena3", "SE_scenario" )

table_9 <- svyby(~wheat_perc0,~nssec8,FoodNutrientY111design,svymean, na.rm=T)
table_9[, c(4:5)] <- svyby(~wheat_perc3,~nssec8,FoodNutrientY111design,svymean, na.rm=T)[, c(2:3)]
names(table_9) <- c("Variable", "Perc_wheatfibre", "SE_fibre", "Perc_wheatfibrescena3", "SE_scenario" )

#Binding the tables together
table_summary3 <- data.table::rbindlist(list( table_7, table_8, table_9), use.names = T, fill = T)

#Changing to perc (%)
table_summary3[, c(2:5)] <- 100*table_summary3[, c(2:5)]

xlsx::write.xlsx(table_summary3, here::here("output", "summary-table_v.1.0.0.xlsx"), 
                 sheetName="Sheet3", 
                 col.names=TRUE, row.names=FALSE, append=TRUE)

# This code can also be used to get the descriptive statistics for SFApctotE and SFApcfoodE

# The mean percentage below or equal to 10% SFApctotE is also provided in the tables
#FoodNutrientY111design$variables$SFApctotE.blo = ifelse(FoodNutrientY111design$variables$SFApctotE<=10,1,0)

#100*svyby(~SFApctotE.blo,~grouptotrepnum,FoodNutrientY111design,svymean, na.rm=T)

# get the unweighted bases
svyby(~Saturatedfattyacidsg,~grouprepnum,FoodNutrientY111design,unwtd.count)
svyby(~Saturatedfattyacidsg,~grouptotrepnum,FoodNutrientY111design,unwtd.count)


# The output from each of these calculations can be rearranged to produce
# columns B, C, D, E and F of Table 3.4 for the respective age/sex group and combined survey year



########################################################
# Statistical comparison for Years 9-11 with Years 7&8 #
########################################################

# Need repgr variable to be complete including Children 1.5-3
FoodNutrientY111design$variables$repgr[FoodNutrientY111design$variables$Age <= 3] = 9

FoodNutrientY111design$variables$Frepgr = as.factor(FoodNutrientY111design$variables$repgr)
#FoodNutrientY111design$variables$Fyeargr = as.factor(FoodNutrientY111design$variables$yeargr)

# make children 1.5-3yrs the first level
FoodNutrientY111design$variables$Frepgr = relevel(FoodNutrientY111design$variables$Frepgr,ref=9)
levels(FoodNutrientY111design$variables$Frepgr)

# make Y78 and Y911 the first two levels
#FoodNutrientY111design$variables$Fyeargr = relevel(FoodNutrientY111design$variables$Fyeargr,ref=5)
#levels(FoodNutrientY111design$variables$Fyeargr)
#FoodNutrientY111design$variables$Fyeargr = relevel(FoodNutrientY111design$variables$Fyeargr,ref=5)
#levels(FoodNutrientY111design$variables$Fyeargr)

# Total
FoodNutrientY111design$variables$Fsexcombagegrps = as.factor(FoodNutrientY111design$variables$sexcombagegrps)
levels(FoodNutrientY111design$variables$Fsexcombagegrps)

# WCB
#FoodNutrientY111design$variables$Fwcb = as.factor(FoodNutrientY111design$variables$wcb)


### Specify covariates
#vor1 = c("Frepgr","Fyeargr")
vor1 = c("Frepgr","Sex")
#vor2 = c("Fsexcombagegrps","Fyeargr")
vor2 = c("Fsexcombagegrps","Frepgr")


make.formula.model<-function(y,x)
{
  formula(paste(y,"~", paste(x, collapse = "*")))
}

############################################################################
# Statistical model for difference of means for sex split reporting groups #
############################################################################

model_split = svyglm(make.formula.model("AOACFibreg",vor1),design=FoodNutrientY111design)
summary(model_split)
R_sq = 1-model_split$deviance/model_split$null.deviance

Residuals = resid(model_split)
Fitted = fitted.values(model_split)
plot(Fitted,Residuals,main="AOACFibreg")
abline(0,0)
StandResiduals = rstandard(model_split)
qqnorm(StandResiduals, main="AOACFibreg")
qqline(StandResiduals)


# For each age/sex group the following code is used to calculate the mean difference and 95% CI for Years 9-11 with Years 7&8
# The coefficients [10,14] are changed for each subsequent age/sex group (i.e. [10,15], [10,16])

# Boys 4-10yrs

##  Get the standard error of the mean difference
b = round(sqrt(summary(model_split)$cov.unscaled[10,10] + summary(model_split)$cov.unscaled[14,14] + 
                 2 * summary(model_split)$cov.unscaled[10,14]),5)

##  Get the estimated mean difference
a = round(summary(model_split)$coefficients[10] + summary(model_split)$coefficients[14],5) 

#  Get the p-value of the mean difference
pval = round(2*(1-pnorm(abs(a/b))),4)

#  Get the lower and upper confidence limits
low = a-1.96*b
upp = a+1.96*b

###############################################################################
# Statistical model for difference of means for sex combined reporting groups #
###############################################################################

model_comb = svyglm(make.formula.model("AOACFibreg",vor2),design=FoodNutrientY111design)
summary(model_comb)
R_sq = 1-model_comb$deviance/model_comb$null.deviance

Residuals = resid(model_comb)
Fitted = fitted.values(model_comb)
plot(Fitted,Residuals,main="Saturatedfattyacidsg")
abline(0,0)
StandResiduals = rstandard(model_comb)
qqnorm(StandResiduals, main="Saturatedfattyacidsg")
qqline(StandResiduals)


# Children 1.5-3yrs

##  Get the standard error of the mean difference
b = round(sqrt(summary(model_comb)$cov.unscaled[6,6] ),5)

##  Get the estimated mean difference
a = round(summary(model_comb)$coefficients[6],5)   

#  Get the p-value of the mean difference
pval = round(2*(1-pnorm(abs(a/b))),4)

#  Get the lower and upper confidence limits
low = a-1.96*b
upp = a+1.96*b


# For each subsequent age group the following code is used to calculate the mean difference and 95% CI for Years 9-11 with Years 7&8
# The coefficients [6,10] are changed for each subsequent age group (i.e. [6,11], [6,12])

# Children 4-10yrs

##  Get the standard error of the mean difference
b = round(sqrt(summary(model_comb)$cov.unscaled[6,6] + summary(model_comb)$cov.unscaled[10,10] + 
                 2 * summary(model_comb)$cov.unscaled[6,10]),5)

##  Get the estimated mean difference
a = round(summary(model_comb)$coefficients[6] + summary(model_comb)$coefficients[10],5) 

#  Get the p-value of the mean difference
pval = round(2*(1-pnorm(abs(a/b))),4)

#  Get the lower and upper confidence limits
low = a-1.96*b
upp = a+1.96*b



##################################################################################
# Statistical model for difference of proportions for sex split reporting groups #
##################################################################################

model_split = svyglm(make.formula.model("SFApctotE.blo",vor1),family=quasibinomial(link="identity"),design=FoodNutrientY111design)
summary(model_split)
R_sq = 1-model_split$deviance/model_split$null.deviance

Residuals = resid(model_split)
Fitted = fitted.values(model_split)
plot(Fitted,Residuals,main="SFApctotE.blo")
abline(0,0)
StandResiduals = rstandard(model_split)
qqnorm(StandResiduals, main="SFApctotE.blo")
qqline(StandResiduals)


# For each age/sex group the following code is used to calculate the mean difference and 95% CI for Years 9-11 with Years 7&8
# The coefficients [10,14] are changed for each subsequent age/sex group (i.e. [10,15], [10,16])

# Boys 4-10yrs

##  Get the standard error of the mean difference
b = round(sqrt(summary(model_split)$cov.unscaled[10,10] + summary(model_split)$cov.unscaled[14,14] + 
                 2 * summary(model_split)$cov.unscaled[10,14]),5)

##  Get the estimated mean difference
a = round(summary(model_split)$coefficients[10] + summary(model_split)$coefficients[14],5) 

#  Get the p-value of the mean difference
pval = round(2*(1-pnorm(abs(a/b))),4)

#  Get the lower and upper confidence limits
low = a-1.96*b
upp = a+1.96*b

# Multiply by 100 to get percentage meeting recomendation
a = 100*a
low = 100*low
upp = 100*upp

#####################################################################################
# Statistical model for difference of proportions for sex combined reporting groups #
#####################################################################################

model_comb = svyglm(make.formula.model("SFApctotE.blo",vor2),family=quasibinomial(link="identity"),design=FoodNutrientY111design)
summary(model_comb)
R_sq = 1-model_comb$deviance/model_comb$null.deviance

Residuals = resid(model_comb)
Fitted = fitted.values(model_comb)
plot(Fitted,Residuals,main="SFApctotE.blo")
abline(0,0)
StandResiduals = rstandard(model_comb)
qqnorm(StandResiduals, main="SFApctotE.blo")
qqline(StandResiduals)


# Children 1.5-3yrs

# The Sat Fat threshold is only applicable to those aged 5yrs and over

# For each age group the following code is used to calculate the mean difference and 95% CI for Years 9-11 with Years 7&8
# The coefficients [6,10] are changed for each subsequent age group (i.e. [6,11], [6,12])

# Children 4-10yrs

##  Get the standard error of the mean difference
b = round(sqrt(summary(model_comb)$cov.unscaled[6,6] + summary(model_comb)$cov.unscaled[10,10] + 
                 2 * summary(model_comb)$cov.unscaled[6,10]),5)

##  Get the estimated mean difference
a = round(summary(model_comb)$coefficients[6] + summary(model_comb)$coefficients[10],5) 

#  Get the p-value of the mean difference
pval = round(2*(1-pnorm(abs(a/b))),4)

#  Get the lower and upper confidence limits
low = a-1.96*b
upp = a+1.96*b

# Multiply by 100 to get percentage meeting recomendation
a = 100*a
low = 100*low
upp = 100*upp


# The output from each of these calculations can be rearranged to produce
# columns G, H and I of Table 3.4 for the respective age/sex group

