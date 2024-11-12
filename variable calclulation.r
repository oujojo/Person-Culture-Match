# set work path
setwd(".../person-culture_fit/")
getwd()
# load libraries and original data
library(haven)
library(dplyr)
library(lme4)
library(lmerTest)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(nnet)
library(flextable)
library(gridExtra)
library(officer)
library(stringr)
library(tidyr)
library(RSA)
library(interactions)
library(effects)


##########################################################
###########   data cleaning and calculate   ##############
#  EMPHASIZE:                                            #
# XXcen -- individual after ipsatized                    #
# XXreg -- region                                        #
# gromXX -- XXcen - XXreg                                #                      
# gramXX -- XXcen - XX_mean,                             #
# where XX_mean is XX's overall mean of the whole sample # 
# gramXXreg --  XXreg - XX_mean                          #
##########################################################

# this R file comes from:
# source the R file with the ML-RSA functions (downloaded from https://osf.io/jhyu9)
source("MultilevelRSA_24052021.r")

# LOAD DATA
fit_data <- read.csv('./Times_data0927.csv')
uszips <- read_xlsx('uszips.xlsx')

# Items of original data
dim(fit_data)  #BEFORE DIMENSION：122580  44

# Filter data
### remove region missing in USA
fit_data <- fit_data %>% 
  filter(country == "United States") %>%
  filter(county_name != "NA") %>%
  filter(state != "NA")
dim(fit_data) #AFTER DIMENSION：46470  44
### rename columns
colnames(fit_data)
colnames(fit_data) <- c("zipcode",'location','uuid',
                        'con1','trad1','ben1','univ1','selfdir1','stim1','hed1',
                        'achiev1','pow1','sec1',
                        'con2','trad2','ben2','univ2','selfdir2','stim2','hed2',
                        'achiev2','pow2','sec2',
                        'ag_narc1','ag_narc2','ag_narc3','com_narc1','com_narc2',
                        'self_esteem','subjective_misfit',
                        'age','gender','state_country','race_eth','income',
                        'charac1','charac2','charac3','timestamp','timestamp_readable',
                        'city','state','country','county')
                
### check missing value in other columns
fit_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) 
fit_data <- na.omit(fit_data)
dim(fit_data)  #46458    44 

# Set Config
### Region Level Selection
region_level = 'county'
n_thresh = 50  # the region sample size >= n_thresh will be saved
range_region_df <- fit_data %>% 
  count(!!sym(region_level)) %>%
  filter(n >= n_thresh)
fit_data <- fit_data %>% 
  filter(!!sym(region_level) %in% range_region_df$county)  %>% 
  mutate(regionID = as.numeric(factor(!!sym(region_level))))
dim(fit_data) #36123    45

# CALCULATE NEW VARIABLE
# THE RULE :step1 --  20 values into 10 big values, use mean
#           step2 --  ipsatized ：individual self-centered   
#           step3 --  group mean COreg
#           step4 -- 
fit_data <- fit_data %>% 
  mutate(CO = (con1+con2)/ 2,TR = (trad1+trad2)/ 2,BE = (ben1+ben2)/ 2,
         UN = (univ1+univ2)/ 2,SD = (selfdir1+selfdir2)/ 2,ST = (stim1+stim2)/ 2,
         HE = (hed1+hed2)/ 2,AC = (achiev1+achiev2)/ 2,PO = (pow1+pow2)/ 2,
         SE = (sec1+sec2)/ 2  # step1
         ) %>% 
  mutate(person_style = (CO+TR+BE+UN+SD+ST+HE+AC+PO+SE)/10) %>% # remove individual response style
  mutate(COcen = CO - person_style, #
         TRcen = TR - person_style, #
         BEcen = BE - person_style, #
         UNcen = UN - person_style, #
         SDcen = SD - person_style, #
         STcen = ST - person_style, #
         HEcen = HE - person_style, #
         ACcen = AC - person_style, #
         POcen = PO - person_style, #
         SEcen = SE - person_style  #
         ) 

# GROUP MEAN -- county / state
### region average level -- as SUFFIX
value_average_df <- fit_data %>%
  group_by(!!sym(region_level)) %>%
  summarise(  #suffix
    COreg = mean(CO),
    TRreg = mean(TR),
    BEreg = mean(BE),
    UNreg = mean(UN),
    SDreg = mean(SD),
    STreg = mean(ST),
    HEreg = mean(HE),
    ACreg = mean(AC),
    POreg = mean(PO),
    SEreg = mean(SE)
  ) %>% 
  mutate(region_style = (COreg+TRreg+BEreg+UNreg+SDreg+STreg+HEreg+ACreg+POreg+SEreg)/10)

### concat two dataframes
fit_data <- fit_data  %>%
  left_join(value_average_df, by = region_level) %>%
  mutate(COregcen = COreg - region_style,
         TRregcen = TRreg - region_style,
         BEregcen = BEreg - region_style,
         UNregcen = UNreg - region_style,
         SDregcen = SDreg - region_style,
         STregcen = STreg - region_style,
         HEregcen = HEreg - region_style,
         ACregcen = ACreg - region_style,
         POregcen = POreg - region_style,
         SEregcen = SEreg - region_style)



### group mean by region -- use prefix "grom", and notice that we have ipsatized the individual style
fit_data <- fit_data  %>%
  mutate(gromCO = COcen- COregcen, gromTR = TRcen- TRregcen,
         gromBE = BEcen- BEregcen, gromUN = UNcen- UNregcen,
         gromSD = SDcen- SDregcen, gromST = STcen- STregcen,
         gromHE = HEcen- HEregcen, gromAC = ACcen- ACregcen,
         gromPO = POcen- POregcen, gromSE = SEcen- SEregcen
         )

# grand mean: 1.XXcen  2.
# mean(fit_data$COcen) is equal to mean(fit_data$COreg)
fit_data <- fit_data %>% 
  mutate( # overall means
         CO_mean = mean(COcen), TR_mean = mean(TRcen),
         BE_mean = mean(BEcen), UN_mean = mean(UNcen),
         SD_mean = mean(SDcen), ST_mean = mean(STcen),
         HE_mean = mean(HEcen), AC_mean = mean(ACcen),
         PO_mean = mean(POcen), SE_mean = mean(SEcen),
         
         # individual after grand meaning
         gramCO = COcen- CO_mean, gramTR = TRcen- TR_mean,
         gramBE = BEcen- BE_mean, gramUN = UNcen- UN_mean,
         gramSD = SDcen- SD_mean, gramST = STcen- ST_mean,
         gramHE = HEcen- HE_mean, gramAC = ACcen- AC_mean,
         gramPO = POcen- PO_mean, gramSE = SEcen- SE_mean,
         
         # region after grand meaning
         gramCOreg = COregcen- CO_mean, gramTRreg = TRregcen- TR_mean,
         gramBEreg = BEregcen- BE_mean, gramUNreg = UNregcen- UN_mean,
         gramSDreg = SDregcen- SD_mean, gramSTreg = STregcen- ST_mean,
         gramHEreg = HEregcen- HE_mean, gramACreg = ACregcen- AC_mean,
         gramPOreg = POregcen- PO_mean, gramSEreg = SEregcen- SE_mean
)

### Z-score standarlization:



######################################################
###########     overall match calculate     ##########
#      method 1: profile correlation                 #
#      method 2: random slope                        #
######################################################
# reverse variables with negative direction
### define reverse function
reverse_mapping <- function(x) {
  ifelse(x == 0, 1,
         ifelse(x == 0.2, 0.8,
                ifelse(x == 0.4, 0.6,
                       ifelse(x == 0.6, 0.4,
                              ifelse(x == 0.8, 0.2,
                                     ifelse(x == 1, 0, NA))))))
}
fit_data$subject_fit <- reverse_mapping(fit_data$subjective_misfit)




