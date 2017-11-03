#CHANGE NOTES:
# 2017-10-08: added 'Not Selected' options for:
#             NSB_CHECK(), NSB_CHECK_RC(), BA_USE_CHECK(), BA_USE_CHECK_RC(), DYS_EXER_CHECK(), DYS_EXER_CHECK_RC()
#             updated BINARY_CODE_FROM_INPUTS() - changed sex, ba_use, dys_exer, noc_s from is.null(...) to if(...)


#List of function used in FEV program:
#AGE_CHECK(age) - 
#BA_USE_CHECK(ba_use) - 
#BA_USE_CHECK_RC(ba_use) - 
#NSB_CHECK(noc_s) - 
#NSB_CHECK_RC(noc_s) - 
#DYS_EXER_CHECK(dys_exer) - 
#DYS_EXER_CHECK_RC(dys_exer) - 
#SEX_CHECK(sex, dys_exer) - 
#AS(sex, alb) - 
#HS(sex, height_square) - 
#SEX_FM(sex) - 
#SEX_FM_RC(sex) - 
#SFM(dys_exer) - 
#ACE(age) - 
#ACE_RC(age) - 
#BUE(ba_use, ba_use_bool) - 
#BUE_RC(ba_use, ba_use_bool_rc) - 
#NSB(noc_s, noc_s_bool) - 
#NSB_RC(noc_s, noc_s_bool_rc) - 
#DEE(dys_exer, dys_exer_effect) - 
#DEE_RC(dys_exer, dys_exer_effect_rc) - 
#DSE(sex, dys_exer, dys_sex_effect) - 
#FEV <- function (trig,...,sex) - 
#FEV_RC <- function (follow_up_baseline,...,sex) - 
#BINARY_CODE_FROM_INPUTS(...) - 
#FEV_calculate_coefficients() - 

#functions that determine boolean values
AGE_CHECK <- function(age){
       if (is.null(age) || is.na(age))              {age_bool = 3}
  else if ((35 <= age) & (age <= 49)) {age_bool = 0}
  else if ((50 <= age) & (age <= 64)) {age_bool = 1}
  else if ((age <= 34) | (age >= 65)) {age_bool = 2}
  return(age_bool)  
}

BA_USE_CHECK <- function(ba_use){
       if (ba_use == 'Current use')   {ba_use_bool = 1}
  else if (ba_use == 'Former use')    {ba_use_bool = 0}
  else if (ba_use == 'No use')        {ba_use_bool = 0}
  else if (ba_use == 'Not Selected')  {ba_use_bool = 0}
  return(ba_use_bool)
}

#######For rate of change coefficient
BA_USE_CHECK_RC <- function(ba_use){
       if (ba_use == 'Current use') {ba_use_bool_rc = 1}
  else if (ba_use == 'Former use')  {ba_use_bool_rc = 0}
  else if (ba_use == 'No use')      {ba_use_bool_rc = 0}
  else if (ba_use == 'Not Selected'){ba_use_bool_rc = 0}
  return(ba_use_bool_rc)
}

NSB_CHECK <- function(noc_s){
       if (noc_s == 'Yes')          {noc_s_bool = 0}
  else if (noc_s == 'Maybe')        {noc_s_bool = 1}
  else if (noc_s == 'No')           {noc_s_bool = 0}
  else if (noc_s == 'Not Selected') {noc_s_bool = 0}
  return(noc_s_bool)
}

#######For rate of change coefficient
NSB_CHECK_RC <- function(noc_s){
       if (noc_s == 'Yes')          {noc_s_bool_rc = 1}
  else if (noc_s == 'Maybe')        {noc_s_bool_rc = 0}
  else if (noc_s == 'No')           {noc_s_bool_rc = 0}
  else if (noc_s == 'Not Selected') {noc_s_bool_rc = 0}
  return(noc_s_bool_rc)
}

DYS_EXER_CHECK <- function(dys_exer){
       if (dys_exer == "On slight exertion")    {dys_exer_effect = -226.09}
  else if (dys_exer == "On moderate exercise")  {dys_exer_effect = -560.37}
  else if (dys_exer == "On rigorous exercise")  {dys_exer_effect = -224.83}
  else if (dys_exer == 'No dyspnea on ex.')     {dys_exer_effect = 0}
  else if (dys_exer == 'Not Selected')          {dys_exer_effect = 0}
  return(dys_exer_effect)
}

#######For rate of change coefficient
DYS_EXER_CHECK_RC <- function(dys_exer){
       if (dys_exer == "On slight exertion")    {dys_exer_effect_rc = 15.79}
  else if (dys_exer == "On moderate exercise")  {dys_exer_effect_rc = -2.43}
  else if (dys_exer == "On rigorous exercise")  {dys_exer_effect_rc = -1.81}
  else if (dys_exer == 'No dyspnea on ex.')     {dys_exer_effect_rc = 0}
  else if (dys_exer == 'Not Selected')          {dys_exer_effect_rc = 0}
  return(dys_exer_effect_rc)
}

SEX_CHECK <- function(sex, dys_exer){
       if ((sex == 'female') & (dys_exer == 'On rigorous exercise'))   {dys_sex_effect = 149.38}
  else if ((sex == 'female') & (dys_exer == 'On moderate exercise'))   {dys_sex_effect = 575.01}
  else if ((sex == 'female') & (dys_exer == 'On slight exertion'))     {dys_sex_effect = -368.46}
  else if ((sex == 'female') & (dys_exer == 'No dyspnea on ex.'))      {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'On rigorous exercise'))   {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'On moderate exercise'))   {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'On slight exertion'))     {dys_sex_effect = 0}
  else if ((sex == 'male')   & (dys_exer == 'No dyspnea on ex.'))      {dys_sex_effect = 0}
  return(dys_sex_effect)
}

#AS - Albumin*Sex (female vs. male)
AS <- function(sex, alb) {
  if (sex == 'female') {
    alb_sex = -9.50*alb
  } else if (sex == 'male') {
    alb_sex = 0
  }
  return (alb_sex)
}

#HS - Height square, cm^2
HS <- function(sex, height_square) {
  if (sex == 'female') {
    height_square_sex = -0.02*(height_square^2)
  } else if (sex == 'male') {
    height_square_sex = 0
  }
  return (height_square_sex)
}

#SEX_FM - Sex (female vs. male)
SEX_FM <- function(sex) {
  if (sex == 'female') {
    female_male_effect = -660.44
  } else if (sex == 'male') {
    female_male_effect = 0
  }
  return (female_male_effect)
}

#######For rate of change coefficient
SEX_FM_RC <- function(sex) {
  if (sex == 'female') {
    female_male_effect_rc = 5.25
  } else if (sex == 'male') {
    female_male_effect_rc = 0
  }
  return (female_male_effect_rc)
}

SFM <- function(dys_exer) { #Right now all this function does is call the DYS_EXER_CHECK_RC function, not sure if this is correct, might need to update
  sex_fm_rc = DYS_EXER_CHECK_RC(dys_exer)
  return(sex_fm_rc)
}

# ACE <- function(age) {
#   age_bool=AGE_CHECK(age)
#   if (age_bool == 1 | age_bool == 0) {
#     ace = (-15.76*age) + ((-139.62+29.47)*age_bool)-29.47
#   } else if ((age_bool == 2) | (age_bool == 3)) {
#     ace = 0
#   } 
#   return(ace)
# }
ACE <- function(age) {
  age_bool=AGE_CHECK(age)
  if (age_bool == 1 | age_bool == 0) {
    ace = (-15.76*age) + ((-139.62+29.47)*age_bool)-29.47
  } else if ((age_bool == 2) | (age_bool == 3)) {
    ace = 0
  } else if (is.na(age_bool)){
    ace = na
  }
  
  return(ace)
}

#######For rate of change coefficient
# ACE_RC <- function(age) {
#   age_bool=AGE_CHECK(age)
#   if (age_bool == 1 | age_bool == 0) {
#     ace_rc = (-0.81*age) + ((6.68-2.69)*age_bool)+2.69
#   }  else if ((age_bool == 2) | (age_bool == 3)) {
#     ace_rc = 0
#   }
#   return(ace_rc)
# }
ACE_RC <- function(age) {
  age_bool=AGE_CHECK(age)
  if (age_bool == 1 | age_bool == 0) {
    ace_rc = (-0.81*age) + ((6.68-2.69)*age_bool)+2.69
  }  else if ((age_bool == 2) | (age_bool == 3)) {
    ace_rc = 0
  } else if (is.na(age_bool)){
    ace_rc = na
  }
  return(ace_rc)
}

#BUE - Bronchodilator Use Effect
BUE <- function(ba_use, ba_use_bool) {
  if(ba_use == 'Former use' | ba_use == "Current use") {
    bue = (-213.06*ba_use_bool)-50.95
  } else if (ba_use == 'No use') {
    bue = 0
  }
  return(bue)
}

#######For rate of change coefficient
BUE_RC <- function(ba_use, ba_use_bool_rc) {
  if(ba_use == 'Former use' | ba_use == "Current use") {
    bue_rc = (3.16*ba_use_bool_rc)-1.86
  } else if (ba_use == 'No use') {
    bue_rc = 0
  }
  return(bue_rc)
}

#NSB - Nocturnal Symptoms Effect
NSB <- function(noc_s, noc_s_bool) {
  if(noc_s == 'Yes' | noc_s == 'Maybe') {
    nsb = (-219.16*noc_s_bool)-342.92
  } else if (noc_s == 'No') {
    nsb = 0
  }
  return(nsb)
}

#######For rate of change coefficient
NSB_RC <- function(noc_s, noc_s_bool_rc) {
  if(noc_s == 'Yes' | noc_s == 'Maybe') {
    nsb_rc = (1.91*noc_s_bool_rc)+2.04
  } else if (noc_s == 'No') {
    nsb_rc = 0
  }
  return(nsb_rc)
}

#DEE - Dyspnea on Exertion Effect
DEE <- function (dys_exer, dys_exer_effect) {
  dee = dys_exer_effect
  return(dee)
}

#######For rate of change coefficient
DEE_RC <- function (dys_exer, dys_exer_effect_rc) {
  # dee_rc = dys_exer_effect_rc
  dee_rc = DYS_EXER_CHECK_RC(dys_exer) 
  return(dee_rc)
}

#DSE - Dyspnea Sex Effect
DSE <- function(sex, dys_exer, dys_sex_effect) {
  dse = dys_sex_effect
  return(dse)
}

# FEV <- function (trig,
#                  hema,
#                  alb,
#                  glob,
#                  alk_phos,
#                  white_bc,
#                  qrs,
#                  alcohol,
#                  wine,
#                  cocktail,
#                  height_square,
#                  cum_smoke,
#                  age,
#                  #age_bool,                                        ###JK
#                  ba_use,
#                  ba_use_bool,
#                  dys_exer,
#                  noc_s,
#                  noc_s_bool,
#                  sex) {
#   b_fev =
#     intercept +                                   
#     AS(sex, alb) +                             
#     HS (sex, height_square) +                               
#     (cum_smoke*trig*smoke_pack_years_trig_effect) +                          
#     SEX_FM(sex) +                         
#     ACE(age) +                                                      ###JK
#     BUE(ba_use, ba_use_bool) +             
#     NSB(noc_s, noc_s_bool) +               
#     DEE(dys_exer, DYS_EXER_CHECK(dys_exer)) +       #UPDATED
#     DSE(sex, dys_exer, SEX_CHECK(sex,dys_exer)) +   #UPDATED
#     (trig*trig_effect) +                  
#     (hema*hema_effect) +                  
#     (alb*alb_effect) +                    
#     (glob*glob_effect) +                  
#     (alk_phos*alk_phos_effect) +          
#     (white_bc*white_bc_effect) +          
#     (qrs*qrs_effect) +                    
#     (alcohol*alcohol_effect) +            
#     (wine*wine_effect) +                  
#     (cocktail*cocktail_effect) +          
#     ((height_square^2)*height_square_effect) +              
#     (cum_smoke*cum_smoke_effect)          
#   
#   return(b_fev)
# }
FEV_pass_coefficients <- function (coefficient_array, trig,
                 hema,
                 alb,
                 glob,
                 alk_phos,
                 white_bc,
                 qrs,
                 alcohol,
                 wine,
                 cocktail,
                 height_square,
                 cum_smoke,
                 age,
                 #age_bool,                                        ###JK
                 ba_use,
                 ba_use_bool,
                 dys_exer,
                 noc_s,
                 noc_s_bool,
                 sex) {
  FEV_intercept                     =coefficient_array[1]
  FEV_trig_effect                   =coefficient_array[2]
  FEV_hema_effect                   =coefficient_array[3]
  FEV_alb_effect                    =coefficient_array[4]
  FEV_glob_effect                   =coefficient_array[5]
  FEV_alk_phos_effect               =coefficient_array[6]
  FEV_white_bc_effect               =coefficient_array[7]
  FEV_qrs_effect                    =coefficient_array[8]
  FEV_alcohol_effect                =coefficient_array[9]
  FEV_wine_effect                   =coefficient_array[10]
  FEV_cocktail_effect               =coefficient_array[11]
  FEV_height_square_effect          =coefficient_array[12]
  FEV_cum_smoke_effect              =coefficient_array[13]
  FEV_smoke_pack_years_trig_effect  =coefficient_array[14]
  FEV_intercept_rc                 =coefficient_array[15]
  FEV_follow_up_baseline_effect     =coefficient_array[16]
  FEV_trig_effect_rc                =coefficient_array[17]
  FEV_hema_effect_rc                =coefficient_array[18]
  FEV_alb_effect_rc                 =coefficient_array[19]
  FEV_glob_effect_rc                =coefficient_array[20]
  FEV_alk_phos_effect_rc            =coefficient_array[21]
  FEV_white_bc_effect_rc            =coefficient_array[22]
  FEV_qrs_effect_rc                 =coefficient_array[23]
  FEV_alcohol_effect_rc             =coefficient_array[24]
  FEV_wine_effect_rc                =coefficient_array[25]
  FEV_cocktail_effect_rc            =coefficient_array[26]
  FEV_height_square_effect_rc       =coefficient_array[27]
  FEV_cum_smoke_effect_rc           =coefficient_array[28]
  b_fev =
    FEV_intercept +                                   
    AS(sex, alb) +                             
    HS (sex, height_square) +                               
    (cum_smoke*trig*FEV_smoke_pack_years_trig_effect) +                          
    SEX_FM(sex) +                         
    ACE(age) +                                                      ###JK
    BUE(ba_use, ba_use_bool) +             
    NSB(noc_s, noc_s_bool) +               
    DEE(dys_exer, DYS_EXER_CHECK(dys_exer)) +       #UPDATED
    DSE(sex, dys_exer, SEX_CHECK(sex,dys_exer)) +   #UPDATED
    (trig*FEV_trig_effect) +                  
    (hema*FEV_hema_effect) +                  
    (alb*FEV_alb_effect) +                    
    (glob*FEV_glob_effect) +                  
    (alk_phos*FEV_alk_phos_effect) +          
    (white_bc*FEV_white_bc_effect) +          
    (qrs*FEV_qrs_effect) +                    
    (alcohol*FEV_alcohol_effect) +            
    (wine*FEV_wine_effect) +                  
    (cocktail*FEV_cocktail_effect) +          
    ((height_square^2)*FEV_height_square_effect) +              
    (cum_smoke*FEV_cum_smoke_effect)          
  
  return(b_fev)
}

# #####For rate of change of FEV
# FEV_RC <- function (follow_up_baseline, 
#                     trig, 
#                     hema, 
#                     alb, 
#                     glob, 
#                     alk_phos, 
#                     white_bc,
#                     qrs, 
#                     alcohol, 
#                     wine, 
#                     cocktail, 
#                     height_square, 
#                     cum_smoke,
#                     age, 
#                     #age_bool,                                       ###JK
#                     ba_use, 
#                     ba_use_bool_rc, 
#                     dys_exer,
#                     noc_s, 
#                     noc_s_bool_rc, 
#                     sex) {
#   b_fev_rc =
#     intercept_rc +  
#     (follow_up_baseline*follow_up_baseline_effect) +
#     SEX_FM_RC(sex) +
#     ACE_RC(age) +                                                           ###JK
#     BUE_RC(ba_use, ba_use_bool_rc) +             
#     NSB_RC(noc_s, noc_s_bool_rc) +               
#     DEE_RC(dys_exer, DYS_EXER_CHECK_RC(dys_exer)) +      
#     (trig*trig_effect_rc) +                  
#     (hema*hema_effect_rc) +                  
#     (alb*alb_effect_rc) +                    
#     (glob*glob_effect_rc) +                  
#     (alk_phos*alk_phos_effect_rc) +          
#     (white_bc*white_bc_effect_rc) +          
#     (qrs*qrs_effect_rc) +                    
#     (alcohol*alcohol_effect_rc) +            
#     (wine*wine_effect_rc) +                  
#     (cocktail*cocktail_effect_rc) +          
#     ((height_square^2)*height_square_effect_rc) +              
#     (cum_smoke*cum_smoke_effect_rc)          
#   
#   return(b_fev_rc)
# }

#####For rate of change of FEV, now with coefficients being passed to the function
FEV_RC_pass_coefficients <- function (coefficient_array,
                    follow_up_baseline, 
                    trig, 
                    hema, 
                    alb, 
                    glob, 
                    alk_phos, 
                    white_bc,
                    qrs, 
                    alcohol, 
                    wine, 
                    cocktail, 
                    height_square, 
                    cum_smoke,
                    age, 
                    #age_bool,                                       ###JK
                    ba_use, 
                    ba_use_bool_rc, 
                    dys_exer,
                    noc_s, 
                    noc_s_bool_rc, 
                    sex) {
  FEV_intercept                     =coefficient_array[1]
  FEV_trig_effect                   =coefficient_array[2]
  FEV_hema_effect                   =coefficient_array[3]
  FEV_alb_effect                    =coefficient_array[4]
  FEV_glob_effect                   =coefficient_array[5]
  FEV_alk_phos_effect               =coefficient_array[6]
  FEV_white_bc_effect               =coefficient_array[7]
  FEV_qrs_effect                    =coefficient_array[8]
  FEV_alcohol_effect                =coefficient_array[9]
  FEV_wine_effect                   =coefficient_array[10]
  FEV_cocktail_effect               =coefficient_array[11]
  FEV_height_square_effect          =coefficient_array[12]
  FEV_cum_smoke_effect              =coefficient_array[13]
  FEV_smoke_pack_years_trig_effect  =coefficient_array[14]
  FEV_intercept_rc                 =coefficient_array[15]
  FEV_follow_up_baseline_effect     =coefficient_array[16]
  FEV_trig_effect_rc                =coefficient_array[17]
  FEV_hema_effect_rc                =coefficient_array[18]
  FEV_alb_effect_rc                 =coefficient_array[19]
  FEV_glob_effect_rc                =coefficient_array[20]
  FEV_alk_phos_effect_rc            =coefficient_array[21]
  FEV_white_bc_effect_rc            =coefficient_array[22]
  FEV_qrs_effect_rc                 =coefficient_array[23]
  FEV_alcohol_effect_rc             =coefficient_array[24]
  FEV_wine_effect_rc                =coefficient_array[25]
  FEV_cocktail_effect_rc            =coefficient_array[26]
  FEV_height_square_effect_rc       =coefficient_array[27]
  FEV_cum_smoke_effect_rc           =coefficient_array[28]
  
  b_fev_rc =
    FEV_intercept_rc +  
    (follow_up_baseline*FEV_follow_up_baseline_effect) +
    SEX_FM_RC(sex) +
    ACE_RC(age) +                                                           ###JK
    BUE_RC(ba_use, ba_use_bool_rc) +             
    NSB_RC(noc_s, noc_s_bool_rc) +               
    DEE_RC(dys_exer, DYS_EXER_CHECK_RC(dys_exer)) +      
    (trig*FEV_trig_effect_rc) +                  
    (hema*FEV_hema_effect_rc) +                  
    (alb*FEV_alb_effect_rc) +                    
    (glob*FEV_glob_effect_rc) +                  
    (alk_phos*FEV_alk_phos_effect_rc) +          
    (white_bc*FEV_white_bc_effect_rc) +          
    (qrs*FEV_qrs_effect_rc) +                    
    (alcohol*FEV_alcohol_effect_rc) +            
    (wine*FEV_wine_effect_rc) +                  
    (cocktail*FEV_cocktail_effect_rc) +          
    ((height_square^2)*FEV_height_square_effect_rc) +              
    (cum_smoke*FEV_cum_smoke_effect_rc)          
  
  return(b_fev_rc)
}

#function for generating binary code
BINARY_CODE_FROM_INPUTS <- function(
  age,
  follow_up_baseline, 
  trig,
  hema,
  alb,
  glob,
  alk_phos,
  white_bc,
  qrs,
  alcohol,
  wine,
  cocktail,
  height_square,
  cum_smoke,
  sex, #selectInput
  ba_use,#selectInput
  dys_exer,#selectInput
  noc_s#selectInput
) {
  if(is.null(age) || is.na(age)) {F1 = 0} else {F1 = 1}
  if(is.null(follow_up_baseline) || is.na(follow_up_baseline)) {F2 = 0} else {F2 = 1}
  if(is.null(trig) || is.na(trig)) {F3 = 0} else {F3 = 1}
  if(is.null(hema) || is.na(hema)) {F4 = 0} else {F4 = 1}
  if(is.null(alb) || is.na(alb)) {F5 = 0} else {F5 = 1}
  if(is.null(glob) || is.na(glob)) {F6 = 0} else {F6 = 1}
  if(is.null(alk_phos) || is.na(alk_phos)) {F7 = 0} else {F7 = 1}
  if(is.null(white_bc) || is.na(white_bc)) {F8 = 0} else {F8 = 1}
  if(is.null(qrs) || is.na(qrs)) {F9 = 0} else {F9 = 1}
  if(is.null(alcohol) || is.na(alcohol)) {F10 = 0} else {F10 = 1}
  if(is.null(wine) || is.na(wine)) {F11 = 0} else {F11 = 1}
  if(is.null(cocktail) || is.na(cocktail)) {F12 = 0} else {F12 = 1}
  if(is.null(height_square) || is.na(height_square)) {F13 = 0} else {F13 = 1}
  if(is.null(cum_smoke) || is.na(cum_smoke)) {F14 = 0} else {F14 = 1}
  if(sex == 'Not Selected') {F15 = 0} else {F15 = 1}
  if(ba_use == 'Not Selected') {F16 = 0} else {F16 = 1}
  if(dys_exer == 'Not Selected') {F17 = 0} else {F17 = 1}
  if(noc_s == 'Not Selected') {F18 = 0} else {F18 = 1}
  # bc <- paste(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18) #generates single chars
  bc <- c(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18) #updated 2017-10-29 generates array of chars
  return(bc)
}

FEV_input_labels <- function() {
  c('trig',
    'hema',
    'alb',
    'glob',
    'alk_phos',
    'white_bc',
    'qrs',
    'alcohol',
    'wine',
    'cocktail',
    'height_square',
    'cum_smoke',
    'age',
    'follow_up_baseline',
    'ba_use',
    'dys_exer',
    'noc_s',
    'sex'
  )
}

#####################DEFINE COEFFICIENT NAMES####################################
#define coefficient_names
c1N <- "intercept"
c2N <- "trig_effect"
c3N <- "hema_effect"
c4N <- "alb_effect"
c5N <- "glob_effect"
c6N <- "alk_phos_effect"
c7N <- "white_bc_effect"
c8N <- "qrs_effect"
c9N <- "alcohol_effect"
c10N <- "wine_effect"
c11N <- "cocktail_effect"
c12N <- "height_square_effect"
c13N <- "cum_smoke_effect"
c14N <- "smoke_pack_years_trig_effect"
c15N <- "intercept_rc"
c16N <- "follow_up_baseline_effect"
c17N <- "trig_effect_rc"
c18N <- "hema_effect_rc"
c19N <- "alb_effect_rc"
c20N <- "glob_effect_rc"
c21N <- "alk_phos_effect_rc"
c22N <- "white_bc_effect_rc"
c23N <- "qrs_effect_rc"
c24N <- "alcohol_effect_rc"
c25N <- "wine_effect_rc"
c26N <- "cocktail_effect_rc"
c27N <- "height_square_effect_rc"
c28N <- "cum_smoke_effect_rc"

FEV_coeff_name_vector <- c(c1N,c2N,c3N,c4N,c5N,c6N,c7N,c8N,c9N,c10N, 
                           c11N,c12N,c13N,c14N,c15N,c16N,c17N,c18N,c19N,c20N, 
                           c21N,c22N,c23N,c24N,c25N,c26N,c27N,c28N
                           )

# #Chen's algorithm will go inside this function
# FEV_calculate_coefficients <- function(){
#   
#   #1. Define the pre-computed values for all of the coefficients - this will be replaced with algorithm
#   FEV_coeff_intercept = 1127.26       # Intercept defined for baseline FEV, mL (Table 2. Wenjia's manuscript)
#   FEV_coeff_trig_effect = -0.30       # Parameter: Triglycerides
#   FEV_coeff_hema_effect = -12.28      # Hematocrit
#   FEV_coeff_alb_effect = 11.33        # Albumin
#   FEV_coeff_glob_effect = -3.49       # Globulin
#   FEV_coeff_alk_phos_effect = -1.48   # Alkaline phosphotase
#   FEV_coeff_white_bc_effect = -0.20   # White blood cell count
#   FEV_coeff_qrs_effect = 27.37        # QRS interval
#   FEV_coeff_alcohol_effect = -5.99    # Alcohol index
#   FEV_coeff_wine_effect = 10.76       # Wine intake
#   FEV_coeff_cocktail_effect = -0.60   # Cocktail intake
#   FEV_coeff_height_square_effect = 0.11           # Height square
#   FEV_coeff_cum_smoke_effect = -3.46              # Cumulative smoke pack-year
#   FEV_coeff_smoke_pack_years_trig_effect = 0.003  # Smoke pack-years * Triglycerides
#   FEV_coeff_intercept_rc = 21.86                  # Effect for rate of FEV change defined ['rc' stands for 'rate of change'] (Table 2. Wenjia's manuscript)
#   FEV_coeff_follow_up_baseline_effect = -0.46     # Follow-up since baseline, y
#   FEV_coeff_trig_effect_rc = 0.004                # Parameter: Triglycerides
#   FEV_coeff_hema_effect_rc = -0.29                # Hematocrit
#   FEV_coeff_alb_effect_rc = 0.07                  # Albumin
#   FEV_coeff_glob_effect_rc = 0.10                 # Globulin
#   FEV_coeff_alk_phos_effect_rc = 0.03             # Alkaline phosphotase
#   FEV_coeff_white_bc_effect_rc = -0.03            # White blood cell count
#   FEV_coeff_qrs_effect_rc = -0.64                 # QRS interval
#   FEV_coeff_alcohol_effect_rc = 0.14              # Alcohol index
#   FEV_coeff_wine_effect_rc = -0.23                # Wine intake
#   FEV_coeff_cocktail_effect_rc = -0.13            # Cocktail intake
#   FEV_coeff_height_square_effect_rc = 0
#   FEV_coeff_cum_smoke_effect_rc = 0
# 
#   #2. Concatenate the values of all of the coefficients  
#   FEV_coefficient_vector = c(
#     FEV_coeff_intercept,
#     FEV_coeff_trig_effect,
#     FEV_coeff_hema_effect,
#     FEV_coeff_alb_effect,
#     FEV_coeff_glob_effect,
#     FEV_coeff_alk_phos_effect,
#     FEV_coeff_white_bc_effect,
#     FEV_coeff_qrs_effect,
#     FEV_coeff_alcohol_effect,
#     FEV_coeff_wine_effect,
#     FEV_coeff_cocktail_effect,
#     FEV_coeff_height_square_effect,
#     FEV_coeff_cum_smoke_effect,
#     FEV_coeff_smoke_pack_years_trig_effect,
#     FEV_coeff_intercept_rc,
#     FEV_coeff_follow_up_baseline_effect,
#     FEV_coeff_trig_effect_rc,
#     FEV_coeff_hema_effect_rc,
#     FEV_coeff_alb_effect_rc,
#     FEV_coeff_glob_effect_rc,
#     FEV_coeff_alk_phos_effect_rc,
#     FEV_coeff_white_bc_effect_rc,
#     FEV_coeff_qrs_effect_rc,
#     FEV_coeff_alcohol_effect_rc,
#     FEV_coeff_wine_effect_rc,
#     FEV_coeff_cocktail_effect_rc,
#     FEV_coeff_height_square_effect_rc,
#     FEV_coeff_cum_smoke_effect_rc
#   )
#   #3. And return the coefficient vector
#   return(FEV_coefficient_vector)
# }

listoffactors = NULL #initialize listoffactors

buildformula_factors <- function(BINARY_CODE_DATAFRAME,FACTOR_NAMES_DATAFRAME){
  if(!is.null(listoffactors)){listoffactors <- NULL}
  
  for(i in 1:nrow(BINARY_CODE_DATAFRAME)){
    if (BINARY_CODE_DATAFRAME$file_name[i] == 1){     #if INPUT value is not null
      listoffactors <- c(listoffactors,unlist((apply(FACTOR_NAMES_DATAFRAME[,(2:ncol(FACTOR_NAMES_DATAFRAME))], 1, function(x) unname(x[!is.na(x)])))[i]))
    }
  }
  return(listoffactors)
}

FEV_calculate_coefficients<- function(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME){
  #####################################
  #STEP0: Prepare the data(Chen's code)
  #####################################
  load("~/Documents/analysis4/analysis4.rdata")	#this command loads the workspace, can change to other directly if analysis4.rdata is saved somewhere else
  
  A13.new<-0.295*data_mi2[,"A13"]
  data_rf<-cbind.data.frame(data_mi2,A13.new)	#this is the original dataset with 126 variables
  #From the original dataset, we will only select predictors for our final model and the two outcomes
  data_rf2<-subset(data_rf, select=c(RANDOMID,visit,fev1,fev1_fvc,age,sex,A13.new,A28,A35,A36,A38,A112,A113,
                                     A138,A147,A182,cpackyr,height2,year, year2,smoke,A86,A126,A131))	
  data_rf2$sex<-as.factor(data_rf2$sex) #Sex needs to be converted into a factor variable instead of continuous
  #change the variable names for all the "Axx" variables
  colnames(data_rf2)[7:16]<-c("triglycerides","hematocrit","albumin","globulin","ALP","wine","cocktail",
                              "WBC","QRS_intv","alcohol_indx")
  colnames(data_rf2)[22:24]<-c("broncho","dyspnea_exc","night_sym")
  
  data.num<-subset(data_rf2, select=c(3:5,7:16,18))	#create a dataset with only continuous variables, including outcomes (except for cpackyr, year, year2)
  data.num2<-scale(data.num, center = TRUE, scale = TRUE)	#center and scale these variables and create a new dataset
  
  data.cha<-subset(data_rf2, select=-c(3:5,7:16,18))  #create a dataset with the rest of uncentered variables
  data_rf4<-cbind(data.cha,data.num2)		#combine the centered/scaled variables with the rest variables to create the regression dataset
  
  max<-data.table(data_rf4)[ , list(visit = max(visit)), by =RANDOMID]  #Label the last visit of each participant (note: they should attent visit 1, 2, 5 and 6)
  colnames(max)[2]<-'max'		# Name this variable as "max" - the last visit
  
  data_rf4<-join(data_rf4,max,by='RANDOMID',type='right', match='all')	#Add the "max" variable to our regression dataset;
  data_rf4$status<-as.numeric(data_rf4$max<6 & data_rf4$max==data_rf4$visit)   
  data_rf4$max<-NULL   #we then drop variable "max", because it is no longer needed 
  data_rf4$agecat[age>=65]<-4
  data_rf4$agecat[age<65 & age>=50]<-3
  data_rf4$agecat[age<50 & age>=35]<-2
  data_rf4$agecat[age<35 & age>=20]<-1
  data_rf4$agecat<-as.factor(data_rf4$agecat)	# Add age category to our data
  
  
  #-------------------------------------------#
  #   Running the Random effects model	    #
  #-------------------------------------------#
  # Note: the model is based on framingham data data_rf4 (centered/scaled, with a censoring variable)
  #Step 1: calculate stablized inverse probability weights of dropping out to the regression model;  
  tstarting_time<-tstartfun(RANDOMID, visit, data_rf4)		#Preparing the data for calculation of inverse probability weight of being censored 
  # Calculate inverse probability weight of being censored, which is a stablized inverse probability weight
  ipw<- ipwtm(exposure = status, family = "binomial",link="logit",numerator=~1,
              denominator=~age+agecat+sex+triglycerides+hematocrit+albumin+globulin+ALP+wine+cocktail+WBC
              +QRS_intv+alcohol_indx+height2+broncho+dyspnea_exc+night_sym,
              id = RANDOMID, tstart = tstarting_time, timevar = visit, type = "first",
              data = data_rf4)
  
  data_rf4<-cbind.data.frame(data_rf4,ipw$ipw.weights) #add censoring variable to data
  colnames(data_rf4)[27]<-'sw'		#change the name of the weight to "sw" - stablized weights
  
  ########################################################
  #STEP1: Generate BINARY_CODE_DATAFRAME from the filename - NO, just pass BINARY_CODE_DATAFRAME to the function
  ########################################################
  
  
  #STEP2: Create inside this func. or outside this function, the FACTOR_NAMES_DATAFRAME - NO, just pass FACTOR_NAMES_DATAFRAME to the function
  
  #STEP3: Use buildformula_factors(BINARY_CODE_DATAFRAME,FACTOR_NAMES_DATAFRAME) to build the equation
  formula_factors <- buildformula_factors(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
  #STEP4: use reformulate to build the full equation(can combine steps 3 and 4)
  full_formula <- reformulate(formula_factors,response="fev1")
  #STEP5: Use lmfin to compute the coefficients
  lmfin <- lmer(full_formula,data_rf4,weights=sw, REML=FALSE)
  #STEP6: Use extract_lmer_coefficients(lmfin) to extract coefficients
  # FEV_coeff_DATA_FRAME <- extract_lmer_coefficients(lmfin)
  coeff_lmfin <- coeffs(lmfin)
  coefficients_DATA_FRAME <- as.data.frame(coeff_lmfin)
  
  final_FEV_coeff_data_frame<- data.frame(FEV_coeff_names=row.names(coefficients_DATA_FRAME),
                                          FEV_coeff_vals=coefficients_DATA_FRAME$coeff_lmfin)
  
  return(final_FEV_coeff_data_frame) #return names and values of the coefficients
}

# #extract_lmer_coefficients()
# #inputs: lmer_output - output object generated by lmer() function
# #outputs:FEV_coeff_DATA_FRAME - data frame containing the FEV coefficients and names
# extract_lmer_coefficients <- function(lmer_output){
#   coeff_lmfin <- coeffs(lmer_output)
#   coefficients_DATA_FRAME <- as.data.frame(coeff_lmfin)
#   return(coefficients_DATA_FRAME) #return names and values of the coefficients
# }

FEV_calculate_lmer_fn<- function(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME){
  #####################################
  #STEP0: Prepare the data(Chen's code)
  #####################################
  load("~/Documents/analysis4/analysis4.rdata")	#this command loads the workspace, can change to other directly if analysis4.rdata is saved somewhere else

  A13.new<-0.295*data_mi2[,"A13"]
  data_rf<-cbind.data.frame(data_mi2,A13.new)	#this is the original dataset with 126 variables
  #From the original dataset, we will only select predictors for our final model and the two outcomes
  data_rf2<-subset(data_rf, select=c(RANDOMID,visit,fev1,fev1_fvc,age,sex,A13.new,A28,A35,A36,A38,A112,A113,
                                     A138,A147,A182,cpackyr,height2,year, year2,smoke,A86,A126,A131))
  data_rf2$sex<-as.factor(data_rf2$sex) #Sex needs to be converted into a factor variable instead of continuous
  #change the variable names for all the "Axx" variables
  colnames(data_rf2)[7:16]<-c("triglycerides","hematocrit","albumin","globulin","ALP","wine","cocktail",
                              "WBC","QRS_intv","alcohol_indx")
  colnames(data_rf2)[22:24]<-c("broncho","dyspnea_exc","night_sym")

  data.num<-subset(data_rf2, select=c(3:5,7:16,18))	#create a dataset with only continuous variables, including outcomes (except for cpackyr, year, year2)
  data.num2<-scale(data.num, center = TRUE, scale = TRUE)	#center and scale these variables and create a new dataset

  data.cha<-subset(data_rf2, select=-c(3:5,7:16,18))  #create a dataset with the rest of uncentered variables
  data_rf4<-cbind(data.cha,data.num2)		#combine the centered/scaled variables with the rest variables to create the regression dataset

  max<-data.table(data_rf4)[ , list(visit = max(visit)), by =RANDOMID]  #Label the last visit of each participant (note: they should attent visit 1, 2, 5 and 6)
  colnames(max)[2]<-'max'		# Name this variable as "max" - the last visit

  data_rf4<-join(data_rf4,max,by='RANDOMID',type='right', match='all')	#Add the "max" variable to our regression dataset;
  data_rf4$status<-as.numeric(data_rf4$max<6 & data_rf4$max==data_rf4$visit)
  data_rf4$max<-NULL   #we then drop variable "max", because it is no longer needed
  data_rf4$agecat[age>=65]<-4
  data_rf4$agecat[age<65 & age>=50]<-3
  data_rf4$agecat[age<50 & age>=35]<-2
  data_rf4$agecat[age<35 & age>=20]<-1
  data_rf4$agecat<-as.factor(data_rf4$agecat)	# Add age category to our data


  #-------------------------------------------#
  #   Running the Random effects model	    #
  #-------------------------------------------#
  # Note: the model is based on framingham data data_rf4 (centered/scaled, with a censoring variable)
  #Step 1: calculate stablized inverse probability weights of dropping out to the regression model;
  tstarting_time<-tstartfun(RANDOMID, visit, data_rf4)		#Preparing the data for calculation of inverse probability weight of being censored
  # Calculate inverse probability weight of being censored, which is a stablized inverse probability weight
  ipw<- ipwtm(exposure = status, family = "binomial",link="logit",numerator=~1,
              denominator=~age+agecat+sex+triglycerides+hematocrit+albumin+globulin+ALP+wine+cocktail+WBC
              +QRS_intv+alcohol_indx+height2+broncho+dyspnea_exc+night_sym,
              id = RANDOMID, tstart = tstarting_time, timevar = visit, type = "first",
              data = data_rf4)

  data_rf4<-cbind.data.frame(data_rf4,ipw$ipw.weights) #add censoring variable to data
  colnames(data_rf4)[27]<-'sw'		#change the name of the weight to "sw" - stablized weights

  ########################################################
  #STEP1: Generate BINARY_CODE_DATAFRAME from the filename - NO, just pass BINARY_CODE_DATAFRAME to the function
  ########################################################


  #STEP2: Create inside this func. or outside this function, the FACTOR_NAMES_DATAFRAME - NO, just pass FACTOR_NAMES_DATAFRAME to the function

  #STEP3: Use buildformula_factors(BINARY_CODE_DATAFRAME,FACTOR_NAMES_DATAFRAME) to build the equation
  formula_factors <- buildformula_factors(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
  #STEP4: use reformulate to build the full equation(can combine steps 3 and 4)
  full_formula <- reformulate(formula_factors,response="fev1")
  #STEP5: Use lmfin to compute the coefficients
  lmfin <- lmer(full_formula,data_rf4,weights=sw, REML=FALSE)
  return(lmfin)
}