# Subject matter expert variable identification
FeaturesofInterest <- c('ZIP',
                        'BUYER_STATUS', # Shouldn't be included in the analysis
  #-------------------------- ----------------------------------
  #                 BUYING POWER
  #______________________________________________________________
  #     ================ CREDIT CARDS ======================
                        'AMEX_PREM',
                        'AMEX_REG',
                        'DEBIT_CC',
                        'DISC_PREM',
                        'DISC_REG',
                        'OTHER_PREM_CC',
                        'OTHER_REG_CC',
                        'STORE_REG_CC',
                        'VISA_PREM',
                        'VISA_REG',
                     
  #     ================ INCOME ===========================
                       # 'CUR_EST_MED_INC',
                        'INC_WIOUTSCS_V4',
                        'M_HH_LEVEL', # Too many categories
                        'ZINVESTR',
                       'PENNY_SAVED_EARNED',
                       'OCCUPATION_GROUP',
                        'FEMALE_LABOR_FOR',
                          #'MED_INC',
  #-------------------------- ----------------------------------
  #                STAGE OF LIFE
  #______________________________________________________________      
                        #'MEDIANAGE',
                        'EXAGE',
                        'ESTAGE',
                        #'P_MARRY',
                        'MARRIED',
                       
                        'NUM_CHILD',
                        #'PRESCHLD',
                        'ADULT1_G',
                        'ADULT2_G',
                        #'P_RENTER', # Dwelling unit renter occupied
                        'PHOMOWNR',
                        'HOMEOWNR',
                        #'RENTER', # potentially redundant with HOMEOWNR
                        #'PRENTER', # potentially redundant with HOMEOWNR
# -------------------------------------------------------------------
#                  HOME VALUE 
#____________________________________________________________________

                        # 'MED_HOME',
                        # 'MED_RENT',
                        # 'LOAN_AMT',
                        'YEAR_BLT',
# -------------------------------------------------------------------
#                  LIFE STYLE 
#____________________________________________________________________
                        
                        #'MED_TRAV_TOWRK',
                        'AVG_COMMUTETIM',                       
                        'DOITSELF',
                        'ZHITECH',
                        'ZONLINE',
                        
                        'ZTRAVANY',

                        #'ZAUTOOWN',
                          'ZDONENVR',
                          'ZVOLUNTR',
                          'ZRELIGON',
                        colnames(complete.customer.data.frame)[451:554]
                        )

# Get reduced data
reducedData <- complete.customer.data.frame %>% 
  select(FeaturesofInterest)

reducedData %<>% 
  mutate(ZIP = as.numeric(ZIP),
         YEAR_BLT = as.numeric(YEAR_BLT),
         NUM_CHILD = as.numeric(NUM_CHILD),
         PENNY_SAVED_EARNED = as.numeric(PENNY_SAVED_EARNED),
         DOTSELF = as.numeric(DOITSELF),
         CREDITCARD = ifelse((AMEX_PREM == "Y"| AMEX_REG == "Y"|
                               DEBIT_CC == "Y"| DISC_PREM == "Y"|
                               DISC_PREM == "Y"|DISC_REG == "Y"|
                               OTHER_PREM_CC == "Y"| OTHER_REG_CC == "Y"|
                               STORE_REG_CC == "Y" | VISA_PREM == "Y"|
                              VISA_REG == "Y"),1,0),
         NUM_CHILD = as.numeric(NUM_CHILD)
         
         ) %>% 
  select(-ends_with("_PREM")) %>% 
  select(-ends_with("_REG")) %>% 
  select(-ends_with("_CC")) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate_at(vars(matches("RESPONSE")), as.factor) %>% 
  mutate(MARRIED = ifelse(str_detect(MARRIED,"M"),"M",
                          ifelse(str_detect(MARRIED, "S"),"S","U"))) %>% 
  mutate(NUM_CHILD = ifelse(is.na(NUM_CHILD),0,NUM_CHILD)) %>% 
  mutate(HOMEOWNR = ifelse(HOMEOWNR == "","N",ifelse(HOMEOWNR == "U" & !PHOMOWNR == "","Y","N"))) %>% 
  select(-PHOMOWNR) %>% 
  mutate(EXAGE = as.numeric(ifelse(EXAGE == "U",ESTAGE,EXAGE))) %>% 
  select(-ESTAGE) %>% 
  mutate(FEMALEHEAD = as.factor(ifelse(ADULT1_G == "F" | ADULT2_G == "F",1,0)))
  

# Feature selection 

# EDA
## Are there correlations between predictors?

###  Starting with Purchasing power

library(ggmosaic)
ggplot(reducedData) + geom_mosaic(aes(x= product(M_HH_LEVEL,INC_WIOUTSCS_V4)))

### relationship between the 2 income variables

reducedData %>% select(M_HH_LEVEL,INC_WIOUTSCS_V4) %>% gather(.) %>% ggplot(aes(x=value)) + geom_bar() + facet_wrap(~key,scales = "free")
ggplot(reducedData) + geom_mosaic(aes(x= product(RESPONSE16,INC_WIOUTSCS_V4),fill = RESPONSE16))

chisq.test(reducedData$M_HH_LEVEL,reducedData$INC_WIOUTSCS_V4)


ggplot(reducedData) + geom_mosaic(aes(x= product(OCCUPATION_GROUP,INC_WIOUTSCS_V4))) + facet_wrap(product(RESPONSE16))
chisq.test(reducedData$INC_WIOUTSCS_V4,reducedData$OCCUPATION_GROUP)



# STAGE OF LIFE data manipulation


### We will use INC_WIOUTSCS_V4
# 
# ggplot(data = fly) +
#   geom_mosaic(aes(x = product(DoYouRecline, RudeToRecline), fill=DoYouRecline, conds=product(Gender)), na.rm=TRUE, divider=mosaic("v")) +  labs(x = "Is it rude recline? ", title='f(DoYouRecline, RudeToRecline| Gender)')
# Select features

# Model
# interpret

