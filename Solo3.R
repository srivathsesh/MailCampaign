# Subject matter expert variable identification
FeaturesofInterest <- c(
                          'ACCTNO',
                          'ZIP',
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
                        'GO_WITH_FLOW',
                        'ZTRAVANY',

                        #'ZAUTOOWN',
                          'ZDONENVR',
                          'ZVOLUNTR',
                          'ZRELIGON',
                        colnames(complete.customer.data.frame)[451:554]
                        )

Locations <- read.csv('M_HH_Level_Location.csv',as.is = T) [,c(1,3)]%>% 
  mutate(Location = as.factor(ifelse(Location == "unknown","Unknown",Location)),
         M_HH_Level = as.factor(M_HH_Level))

# Get reduced data
reducedData <- complete.customer.data.frame %>% 
  select(FeaturesofInterest)

reducedData %<>% 
  mutate(ZIP = as.numeric(ZIP),
         YEAR_BLT = as.numeric(YEAR_BLT),
         NUM_CHILD = as.numeric(NUM_CHILD),
         PENNY_SAVED_EARNED = as.numeric(PENNY_SAVED_EARNED),
         DOITSELF = as.numeric(DOITSELF),
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
  mutate(FEMALEHEAD = as.factor(ifelse(ADULT1_G == "F" | ADULT2_G == "F",1,0))) %>% 
  left_join(.,Locations, by = c("M_HH_LEVEL" = "M_HH_Level")) %>% 
  mutate(Contacted = select(.,starts_with("ANY_")) %>% rowSums(.) > 0,
         Responded = select(.,starts_with("RESPONSE")) %>% 
           select(-RESPONSE0) %>% 
           mutate_if(is.factor,as.character) %>%
           mutate_if(is.character, as.numeric) %>% 
           rowSums(.) > 0) %>% 
  select(-BUYER_STATUS,
         -M_HH_LEVEL,
         -ADULT1_G,
         -ADULT2_G
         )

 # Reshape reducedData
# Objective is to have a data frame that looks something like this 
#----------------------------------------------------------
# ACCTNO | Response | Campaign | Contacted | Qty | Amount | Other Attributes
#----------------------------------------------------------
#        |          |          |           |    |         
#________|__________|__________|___________|____|__________

 
reshaped <- reducedData %>% 
  select(ACCTNO
         ,INC_WIOUTSCS_V4
         ,ZINVESTR
         ,PENNY_SAVED_EARNED
         ,OCCUPATION_GROUP
         ,FEMALE_LABOR_FOR
         ,EXAGE
         ,MARRIED
         ,NUM_CHILD
         ,HOMEOWNR
         ,YEAR_BLT
         ,AVG_COMMUTETIM
         ,DOITSELF
         ,ZHITECH
         ,ZONLINE
         ,GO_WITH_FLOW
         ,ZTRAVANY
         ,ZDONENVR
         ,ZVOLUNTR
         ,ZRELIGON
         ,CREDITCARD
         ,FEMALEHEAD
         ,Location
         ,starts_with("RESPONSE")
         ) 
  groupingCols <- colnames(reshaped)[1:23]
  
  reshapedResp <- reshape2::melt(reshaped,id = groupingCols,value.name = "Response") %>% 
    mutate(Campaign = as.numeric(substr(as.character(variable),9,nchar(as.character(variable)))))
  reshapedSumMail <- reshape2::melt(reducedData %>% select(starts_with("SUM_MAIL"),!!groupingCols),id= groupingCols, value.name = "SUM_MAIL")
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


reducedData %>% select(starts_with('ANY'),starts_with('RESPONSE')) -> testing
testing %>% mutate_if(is.factor,as.character) %>% mutate_if(is.character, as.numeric) %>%  colSums(.) -> testagg
integer(16) -> rst
for(i in 1:16){ rst[i] <- testagg[i+16]/testagg[1]}


names(rst) <- as.character(1:16)
plot(testagg[1:16], rst)

plot(rst,type= "b")
# STAGE OF LIFE data manipulation


### We will use INC_WIOUTSCS_V4


# Model
# interpret

