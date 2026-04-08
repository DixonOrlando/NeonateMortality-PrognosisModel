########################Neonates external validation########################

library(tidyverse)
library(tidymodels)
library(meta)
library(dcurves)

##################Dataset cleaning##################
###Reading the dataset###
neo_kenya = read_csv("cin_nn_data_18nov_2025(in).csv")

summary(neo_kenya)

###Converting character variables into factor variables###
neo_kenya = neo_kenya %>%
  mutate_if(is.character, as.factor)

###Converting the date of admisssion and date of discharge into the right format###
neo_kenya = neo_kenya %>%
  mutate(DODnew = mdy(date_discharge)) %>% #The data has month/day/year format
  mutate(DOAnew = mdy(date_adm)) %>%  
  mutate(AgeD = as.numeric(DODnew-DOAnew, na.rm=TRUE)) %>%
  drop_na(AgeD)

neo_kenya = neo_kenya %>%
  filter(AgeD >= 0) #Filter those with only positive AgeD. 242 dropped.

###Deriving severe respiratory distress (combine grunting, indrawing, and difficulty breathing)###
neo_kenya %>%
  select(indrawing, difficulty_breathing, grunting) %>%
  summary()

neo_kenya = neo_kenya %>% #Converting all the "empty" values inside the variables to be NA.
  mutate(indrawing = if_else(indrawing == "Empty", NA, indrawing),
         difficulty_breathing = if_else(difficulty_breathing == "Empty", NA, difficulty_breathing),
         grunting = if_else(grunting == "Empty", NA, grunting))

neo_kenya = neo_kenya %>%
  mutate(Q37RESPI2 = case_when(
    indrawing == "severe" | indrawing == "sternum" | grunting == "Yes" ~ "Yes",
    indrawing != "severe" & indrawing != "sternum" & grunting != "Yes" ~ "No",
    is.na(indrawing) & is.na(grunting) ~ NA
  ))

###Deriving very poor feeding or unable to suck###

#Definition in the development dataset: Very poor feeding, or unable to sucks
neo_kenya %>%
  select(difficulty_feeding, suck_breastfeed) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(difficulty_feeding = if_else(difficulty_feeding == "Empty", NA, difficulty_feeding),
         suck_breastfeed = if_else(suck_breastfeed == "Empty", NA, suck_breastfeed))

neo_kenya = neo_kenya %>%
  mutate(Q42UNBRE2 = case_when(
    difficulty_feeding == "Yes" | suck_breastfeed == "No" ~ "Yes",
    difficulty_feeding == "No" & suck_breastfeed == "Yes" ~ "No",
    is.na(difficulty_feeding) & is.na(suck_breastfeed) ~ NA
  ))

###Deriving Lethargy, drowsy or unconscious (difficult to wake, moving only when stimulated)###
neo_kenya %>%
  select(level_of_activity) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(level_of_activity = if_else(level_of_activity == "Empty", NA, level_of_activity))

neo_kenya = neo_kenya %>%
  mutate(Q42LETHA2 = level_of_activity)

###Deriving birth asphyxia###
neo_kenya %>%
  filter(is.na(pry_adm_diag) & is.na(adm_diag_1) & is.na(adm_diag_2) & is.na(adm_diag_3)) %>%
  select(pry_adm_diag, adm_diag_1, adm_diag_2, adm_diag_3) %>%
  nrow()

neo_kenya %>% filter(str_detect(pry_adm_diag, "asphyxia")) %>% count(pry_adm_diag)
neo_kenya %>% filter(str_detect(adm_diag_1, "asphyxia")) %>% count(adm_diag_1)
neo_kenya %>% filter(str_detect(adm_diag_2, "asphyxia")) %>% count(adm_diag_2)
neo_kenya %>% filter(str_detect(adm_diag_3, "asphyxia")) %>% count(adm_diag_3)

neo_kenya = neo_kenya %>%
  mutate(AdDxASPH = case_when(
    # 1. Any field contains "asphyxia"
    str_detect(pry_adm_diag, "asphyxia") | str_detect(adm_diag_1, "asphyxia") | str_detect(adm_diag_2, "asphyxia") | str_detect(adm_diag_3, "asphyxia") ~ "Yes",
    
    # 2. All fields are NA
    is.na(pry_adm_diag) & is.na(adm_diag_1) & is.na(adm_diag_2) & is.na(adm_diag_3) ~ NA,
    
    # 3. Otherwise none contain "asphyxia" (but not all NA)
    TRUE ~ "No"
  ))

###Deriving SpO2###
neo_kenya %>%
  select(oxygen_saturation) %>%
  summary()

#Converting those to be lower than 0 to be NA.
neo_kenya = neo_kenya %>%
  mutate(Q52SP02P = oxygen_saturation,
         Q52SP02P = if_else(Q52SP02P < 0, NA, Q52SP02P))

#Set the upper and lower limit for SpO2
neo_kenya = neo_kenya %>%
  mutate(Q52SP02P = case_when(
    Q52SP02P > 100 ~ NA, #Upper limit of 100
    Q52SP02P <= 30 ~ NA, #Lower limit of 31
    TRUE ~ Q52SP02P
  ))


###Deriving weight###
neo_kenya %>% filter(wt_now >0) %>% select(wt_now) %>% view() #Some weight are in grams and the other in kilograms.

#Convert the weight that is in kilograms into grams
#Those that weigh less than 100 (considering the lowest possible value for kg which is around 0.1 kg), must be multiplied by 1,000. This is because they should be in kilogram.
neo_kenya = neo_kenya %>%
  mutate(Q48WEIGH = if_else(wt_now < 0, NA, wt_now)) %>% #There are some values that fall below 0, this should be converted into NA.
  mutate(Q48WEIGH = if_else(Q48WEIGH >= 100 & !is.na(Q48WEIGH), Q48WEIGH / 1000, Q48WEIGH)) #Converting those with KG to grams.


#Setting the lower and upper limit for the weight.
neo_kenya = neo_kenya %>%
  mutate(Q48WEIGH = case_when(
    Q48WEIGH < 0.550 ~ NA, #Lower limit of
    Q48WEIGH > 5.5 ~ NA, #Upper limit of
    TRUE ~ Q48WEIGH
  ))


###Deriving heart rate###
neo_kenya %>%
  select(heart_rate_hr_min) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Q50HEART = heart_rate_hr_min,
         Q50HEART = if_else(Q50HEART <0, NA, Q50HEART))

#Setting the upper and lower limit
neo_kenya = neo_kenya %>%
  mutate(Q50HEART = case_when(
    Q50HEART < 50 ~ NA,
    Q50HEART > 230 ~ NA,
    TRUE ~ Q50HEART
  ))


###Deriving respiratory rate###
neo_kenya %>%
  select(respiratory_rate_rr_per_mi) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Q51RESPI = respiratory_rate_rr_per_mi,
         Q51RESPI = if_else(Q51RESPI <0, NA, Q51RESPI))

#Setting the upper and lower limit
neo_kenya = neo_kenya %>%
  mutate(Q51RESPI = case_when(
    Q51RESPI >= 120 ~ NA,
    TRUE ~ Q51RESPI
  ))



###Deriving temperature###
neo_kenya %>%
  select(temperature_degrees_celciu) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Q53TEMPE = temperature_degrees_celciu,
         Q53TEMPE = if_else(Q53TEMPE <0, NA, Q53TEMPE))

#Setting the upper and lower limit
neo_kenya = neo_kenya %>%
  mutate(Q53TEMPE = case_when(
    Q53TEMPE < 32 ~ NA, 
    Q53TEMPE > 42.4 ~ NA,
    TRUE ~ Q53TEMPE
  ))


###Deriving full oxygen system###
neo_kenya %>%
  select(oxygen_ordered) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(FullOxygen = oxygen_ordered)

###Deriving convulsions###
neo_kenya %>%
  select(convulsions) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Q43CONVU2 = convulsions,
         Q43CONVU2 = if_else(Q43CONVU2 == "Empty", NA, Q43CONVU2)) #Converthing "empty" values into NA.

###Deriving sex###
neo_kenya %>%
  select(child_sex) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Q12SEX = child_sex,
         Q12SEX = case_when(
           Q12SEX == "Empty" ~ NA, #Converthing those with empty or indeterminate value to be NA.
           Q12SEX == "Indeterminate" ~ NA,
           TRUE ~ Q12SEX
         ))


###Deriving jaundice###
neo_kenya %>%
  select(jaundice) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Q40JAUND2 = jaundice)

neo_kenya = neo_kenya %>%
  mutate(Q40JAUND2 = case_when(
    Q40JAUND2 == "#NAME?" ~ "Yes",
    Q40JAUND2 == "none(0)" ~ "No",
    Q40JAUND2 == "Empty" ~ NA,
    Q40JAUND2 == "Not classified" ~ NA,
    TRUE ~ Q40JAUND2
  ))


###Converting outcome into an appropriate format and then filter observations with missing outcome###
neo_kenya %>%
  select(outcome) %>%
  summary()

neo_kenya = neo_kenya %>%
  mutate(Died = outcome,
         Died = if_else((AgeD >7 & outcome == "Dead"), "Alive", Died))

neo_kenya = neo_kenya %>% #Excluding those with missing outcome or "Empty" value inside the outcome variable.
  filter(!(Died == "Empty" | is.na(Died)))


neo_kenya = neo_kenya %>%
  mutate(Died = if_else(Died == "Dead", "1", "0"))


###Checking the hospitals and/or period with high missingness for SpO2###
exclude_hospitals = neo_kenya %>% #Two hospitals will be excluded
  count(Q52SP02P, hosp_id) %>% #Kitale County Referral Hospital: 71.3%
  filter(is.na(Q52SP02P)) %>% #Malindi Sub County Hospital: 78.3%
  left_join(neo_kenya %>%
              count(hosp_id) %>%
              rename('total' = 'n')) %>%
  mutate(prop = n/total * 100) %>%
  filter(prop >= 50) 

neo_kenya = neo_kenya %>% #Filter out hospitals with very high SpO2 missingness.
  filter(!(hosp_id %in% exclude_hospitals$hosp_id))

###Filtering out observations with missing hospital ID###
neo_kenya = neo_kenya %>%
  filter(!is.na(hosp_id))

###Exclude observations in the year with high missingness for SpO2###
neo_kenya %>% #Proportion of missingness in SpO2 across the years.
  mutate(year = year(DOAnew)) %>%
  count(year, Q52SP02P) %>%
  filter(is.na(Q52SP02P)) %>%
  left_join(neo_kenya %>%
              mutate(year = year(DOAnew)) %>%
              count(year) %>%
              rename('total' = 'n')) %>%
  mutate(prop = n/total*100)

neo_kenya  %>% #Proportion of missingness in SpO2 for hospitals across the years.
  mutate(year = year(DOAnew),
         month = month(DOAnew)) %>%
  count(year, month,  Q52SP02P) %>%
  filter(is.na(Q52SP02P)) %>%
  left_join(neo_kenya  %>%
              mutate(year = year(DOAnew),
                     month = month(DOAnew)) %>%
              count(year, month) %>%
              rename('total' = 'n'),
            by = c("year" = "year", "month" = "month")) %>%
  mutate(prop = (n/total)*100)

neo_kenya  %>% #Proportion of missingness in SpO2 for hospitals across the years.
  mutate(year = year(DOAnew),
         month = month(DOAnew)) %>%
  count(year,  Q52SP02P, hosp_id) %>%
  filter(is.na(Q52SP02P)) %>%
  left_join(neo_kenya  %>%
              mutate(year = year(DOAnew),
                     month = month(DOAnew)) %>%
              count(year, hosp_id) %>%
              rename('total' = 'n'),
            by = c("year" = "year", "hosp_id" = "hosp_id")) %>%
  mutate(prop = (n/total)*100) %>%
  filter(hosp_id %in% exclude_hospitals$hosp_id) %>% view()

neo_kenya = neo_kenya %>% #Filter out observations for the year with high missingness in SpO2.
  mutate(year = year(DOAnew)) %>%
  filter(year != 2020)


###Finding observations with missingness across many variables###
neo_kenya <- neo_kenya[rowSums(is.na(neo_kenya %>% #Those with missingness across more than 6 variables will be excluded, 6,418 observations dropped.
                                        dplyr::select(Q37RESPI2, Q42UNBRE2, Q42LETHA2, AdDxASPH, Q52SP02P, Q48WEIGH, Q50HEART, Q51RESPI, Q53TEMPE, FullOxygen, Q43CONVU2, Q40JAUND2))) <= 6, ] 



###Preparing the dataset into a final form###
neo_kenya %>% 
  select(Q37RESPI2, Q42UNBRE2, Q42LETHA2, AdDxASPH, Q52SP02P, Q48WEIGH, Q50HEART, Q51RESPI, Q53TEMPE, FullOxygen, Q43CONVU2, Q40JAUND2) %>%
  summary()

final_neo_kenya = neo_kenya %>%
  mutate(Q37RESPI2 = if_else(Q37RESPI2 == "Yes", "YES", "no"), #Converting the value so it matches with the format in the Nigerian dataset.
         Q42UNBRE2 = if_else(Q42UNBRE2 == "Yes", "YES", "no"),
         Q42LETHA2 = if_else(Q42LETHA2 == "Yes", "YES", "no"),
         AdDxASPH = if_else(AdDxASPH == "Yes", "Birth Asphyxia", "none"),
         FullOxygen = if_else(FullOxygen == "Yes", "YES", "no"),
         Q43CONVU2 = if_else(Q43CONVU2 == "Yes", "YES", "no"),
         Q40JAUND2 = if_else(Q40JAUND2 == "Yes", "YES", "no")) %>%
  mutate(Q37RESPI2 = factor(Q37RESPI2),
         Q42UNBRE2 = factor(Q42UNBRE2),
         Q42LETHA2 = factor(Q42LETHA2),
         AdDxASPH = factor(AdDxASPH),
         FullOxygen = factor(FullOxygen),
         Q43CONVU2 = factor(Q43CONVU2),
         Q40JAUND2 = factor(Q40JAUND2),
         Died = factor(Died)) #For Died, it's not necessary to convert them into an appropriate format because it has been done in the previous derivation process.


###Checking the dataset to ensure that everything is correct and there is no outlier###
final_neo_kenya %>%
  summary()

###De-identify facilities###
final_neo_kenya = final_neo_kenya %>%
  mutate(hosp_id = factor(as.numeric(hosp_id)))

##################Performing case-mix check##################
#Building membership model
comb_data = final_neo_kenya %>%
  dplyr::select(Q37RESPI2, Q42UNBRE2, Q42LETHA2, AdDxASPH, Q52SP02P, Q48WEIGH, Q50HEART, Q51RESPI, Q53TEMPE, FullOxygen, Q43CONVU2, Q12SEX, Q40JAUND2, Died) %>%
  mutate(membership = "0") %>%
  rbind(final_neo %>%
          dplyr::select(Q37RESPI2, Q42UNBRE2, Q42LETHA2, AdDxASPH, Q52SP02P, Q48WEIGH, Q50HEART, Q51RESPI, Q53TEMPE, FullOxygen, Q43CONVU2, Q12SEX, Q40JAUND2, Died) %>%
          mutate(membership = "1")) %>%
  mutate(membership = factor(membership, levels = c("0", "1")))

#XGBoost model
m_model = glm(membership ~ Q52SP02P + Q48WEIGH + Q50HEART + Q51RESPI + Q53TEMPE + Q37RESPI2 + Died,
              data = comb_data,
              family = "binomial")

#LR model
m_model = glm(membership ~ Q48WEIGH + Q52SP02P + Q37RESPI2 + Q42UNBRE2 + Q53TEMPE + Q42LETHA2 + Died,
              data = comb_data,
              family = "binomial")

#DT model
m_model = glm(membership ~ Q52SP02P + Q37RESPI2 + Q48WEIGH + Q42LETHA2 + Q50HEART + Q51RESPI + Died,
              data = comb_data,
              family = "binomial")


pred_res = augment(m_model,
                   type.predict = "response")

pROC::roc(pred_res$membership, pred_res$.fitted)$auc


##################Performing external validation for sLR##################
#Load the model
load("sim_Lasso_fit_baggedtree_neo_poly.Rda", verbose = T)

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

#Before recalibration

for (i in unique(final_neo_kenya$hosp_id)) {
  
  #Validation process
  lasso_val = augment(lasso_fit, new_data = final_neo_kenya %>% filter(hosp_id == i))
  
  lasso_val = lasso_val %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  lasso_val <- lasso_val %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  if (i == "21") {
    simpen = lasso_val
  }
  
  if (i != "21") {
    simpen = simpen %>%
      rbind(lasso_val)
  }
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = lasso_val) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = lasso_val)
  
  store[row,1] = pROC::roc(lasso_val$Died, lasso_val$.pred_1)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(lasso_val$Died, lasso_val$.pred_1)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


#After recalibration
simpen = simpen %>%
  mutate(raw_lp = log(.pred_1/(1-.pred_1)))

logistic_recalibration = glm(Died ~ raw_lp, #Performing logistic recalibration.
                             data = simpen,
                             family = binomial())

simpen = simpen %>%
  mutate(.pred_1_recal = predict(logistic_recalibration, newdata = simpen, type = "response"))

simpen = simpen %>%
  mutate(raw_lp_recal = log(.pred_1_recal/(1-.pred_1_recal)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_neo_kenya$hosp_id)) {
  
  simpen2 = simpen %>%
    filter(hosp_id == i)
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp_recal, 
                  family = binomial, 
                  data = simpen2) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp_recal, 
                      family = binomial,
                      data = simpen2)
  
  store[row,1] = pROC::roc(simpen2$Died, simpen2$.pred_1_recal)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(simpen2$Died, simpen2$.pred_1_recal)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}

##################Performing external validation for XGBoost##################
#Load the models
mod_bundle <- readRDS("final_xgb_bundle_neo.rds")
xgb_fit <- bundle::unbundle(mod_bundle)

#Before recalibration
store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_neo_kenya$hosp_id)) {
  xgb_val = augment(xgb_fit, new_data = final_neo_kenya %>% filter(hosp_id == i))
  
  xgb_val = xgb_val %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  xgb_val <- xgb_val %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  if (i == "21") {
    simpen = xgb_val
  }
  
  if (i != "21") {
    simpen = simpen %>%
      rbind(xgb_val)
  }
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = xgb_val) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = xgb_val)
  
  store[row,1] = pROC::roc(xgb_val$Died, xgb_val$.pred_1)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(xgb_val$Died, xgb_val$.pred_1)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}

#After recalibration
simpen = simpen %>%
  mutate(raw_lp = log(.pred_1/(1-.pred_1)))

logistic_recalibration = glm(Died ~ raw_lp,
                             data = simpen,
                             family = binomial())


simpen = simpen %>%
  mutate(.pred_1_recal = predict(logistic_recalibration, newdata = simpen, type = "response")) #Performing logistic recalibration.

simpen = simpen %>%
  mutate(raw_lp_recal = log(.pred_1_recal/(1-.pred_1_recal)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_neo_kenya$hosp_id)) {
  
  simpen2 = simpen %>%
    filter(hosp_id == i)
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp_recal, 
                  family = binomial, 
                  data = simpen2) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp_recal, 
                      family = binomial,
                      data = simpen2)
  
  store[row,1] = pROC::roc(simpen2$Died, simpen2$.pred_1_recal)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(simpen2$Died, simpen2$.pred_1_recal)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


##################Performing external validation for DT##################
#Load the model
load("DT_fit_noimp_neo.Rda", verbose = T)

final_kenya_neo_dt = final_neo_kenya

#Performing independent imputation for decision tree using bagged tree imputation model created using external validation dataset.
impute_rec <- recipe(Died ~ 
                       Q37RESPI2 + Q42UNBRE2 + Q42LETHA2 + AdDxASPH + Q52SP02P + Q48WEIGH + Q50HEART + Q51RESPI + Q53TEMPE + Q43CONVU2 + Q12SEX + Q40JAUND2, 
                     data = final_kenya_neo_dt) %>%
  step_impute_bag(all_predictors())

final_kenya_neo_dt = bake(prep(impute_rec, training = final_kenya_neo_dt), new_data = NULL)

final_kenya_neo_dt = final_kenya_neo_dt %>% 
  mutate(hosp_id = final_neo_kenya$hosp_id,
         Q52SP02P = final_neo_kenya$Q52SP02P)

#Transforming all missing values for SpO2 to be 150 as explained in the method section of the main writing.
final_kenya_neo_dt = final_kenya_neo_dt %>%
  mutate(Q52SP02P = if_else(is.na(Q52SP02P), 150, Q52SP02P))


#These steps are necessary because the decision tree model requires all the candidate predictors to be present as columns in the data frame. 
#When fitting the initial decision tree, we used all 26 candidate predictors. Even though at the end there's 6 final predictors. 
final_neo_dt = final_neo

final_neo_dt[1:nrow(final_kenya_neo_dt),] = NA

data2 = final_kenya_neo_dt %>%
  cbind(final_neo_dt %>%
          select(-c(Q37RESPI2, Q42UNBRE2, Q42LETHA2, AdDxASPH, Q52SP02P, Q48WEIGH, Q50HEART, Q51RESPI, Q53TEMPE, Q43CONVU2, Q12SEX, Q40JAUND2, Died, AgeD)))


store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

#Before recalibration
for (i in unique(final_neo_kenya$hosp_id)) {
  
  #Validation process
  tree_val = augment(tree_fit, new_data = data2 %>% dplyr::filter(hosp_id == i))
  
  tree_val = tree_val %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  tree_val <- tree_val %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  if (i == "21") {
    simpen = tree_val
  }
  
  if (i != "21") {
    simpen = simpen %>%
      rbind(tree_val)
  }
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = tree_val) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = tree_val)
  
  store[row,1] = pROC::roc(tree_val$Died, tree_val$.pred_1)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(tree_val$Died, tree_val$.pred_1)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


#After recalibration
simpen = simpen %>%
  mutate(raw_lp = log(.pred_1/(1-.pred_1)))

logistic_recalibration = glm(Died ~ raw_lp,
                             data = simpen,
                             family = binomial())

simpen = simpen %>% #Performing logistic recalibration
  mutate(.pred_1_recal = predict(logistic_recalibration, newdata = simpen, type = "response"))

simpen = simpen %>%
  mutate(raw_lp_recal = log(.pred_1_recal/(1-.pred_1_recal)))

store = data.frame(auc = 1, se = 1, slo = 1, slo_se = 1, int = 1, int_se=1,  cluster = "A")
row = 1

for (i in unique(final_neo_kenya$hosp_id)) {
  
  simpen2 = simpen %>%
    filter(hosp_id == i)
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp_recal, 
                  family = binomial, 
                  data = simpen2) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp_recal, 
                      family = binomial,
                      data = simpen2)
  
  store[row,1] = pROC::roc(simpen2$Died, simpen2$.pred_1_recal)$auc
  store[row, 2] = sqrt(pROC::var(pROC::roc(simpen2$Died, simpen2$.pred_1_recal)))
  store[row, 3] = coef(summary(slope_pc))[2,1]
  store[row, 4] = coef(summary(slope_pc))[2,2]
  store[row, 5] = coef(summary(intercept_pc))[1,1]
  store[row, 6] = coef(summary(intercept_pc))[1,2]
  store[row, 7] = i
  
  row = row + 1
}


##################Performing analysis of the results of external validation##################
#These code are for analysing the results attained from running the loops above.
fk = final_neo_kenya 

store = store %>%
  mutate(cluster = as.numeric(cluster)) %>%
  arrange(cluster) %>%
  mutate(cluster = factor(cluster))

#Forest plot for C-statistic (can be used for both prior and after recalibration).
forest(metagen(TE = auc,
               seTE = se,
               studlab = cluster,
               data =  store %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("cluster" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("cluster" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("C-statistic", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       xlim = c(0.4,1))


#Forest plot for calibration slope (can be used for both prior and after recalibration).
forest(metagen(TE = slo,
               seTE = slo_se,
               studlab = cluster,
               data =  store %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("cluster" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("cluster" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("Calibration slope", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       xlim = c(0.2,1.7))

#Forst plot for calibration intercept (can be used for both prior and after recalibration).
forest(metagen(TE = int,
               seTE = int_se,
               studlab = cluster,
               data =  store %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("cluster" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("cluster" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("Calibration intercept", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       xlim = c(-1.3,1.5))

#Forest plot for O/E ratio (this is for prior recalibration).
O_E = simpen %>% 
  dplyr::group_by(hosp_id) %>% 
  dplyr::summarize(O = sum(as.numeric(as.character(Died))), E = sum(.pred_1)) %>% mutate(O_E = O/E) %>%
  left_join(simpen %>%
              mutate(mult = .pred_1*(1-.pred_1)) %>%
              select(mult, hosp_id) %>%
              dplyr::group_by(hosp_id) %>%
              dplyr::summarize(mult = sum(mult)),
            by = c("hosp_id" = "hosp_id"))

O_E = O_E %>%
  mutate(sd = 1/E * sqrt(mult))

O_E = O_E %>%
  mutate(hosp_id = factor(hosp_id, levels = store$cluster)) %>%
  arrange(hosp_id)

forest(metagen(TE = O_E,
               seTE = sd,
               studlab = hosp_id,
               data =  O_E %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("hosp_id" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("hosp_id" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("O/E ratio", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       ref = 1,
       xlim = c(-0.2, 2.5))

#Forest plot for O/E ratio (this is for after recalibration).
O_E = simpen %>% 
  mutate(.pred_1 = .pred_1_recal) %>%
  dplyr::group_by(hosp_id) %>% 
  dplyr::summarize(O = sum(as.numeric(as.character(Died))), E = sum(.pred_1)) %>% mutate(O_E = O/E) %>%
  left_join(simpen %>%
              mutate(.pred_1 = .pred_1_recal) %>%
              mutate(mult = .pred_1*(1-.pred_1)) %>%
              select(mult, hosp_id) %>%
              dplyr::group_by(hosp_id) %>%
              dplyr::summarize(mult = sum(mult)),
            by = c("hosp_id" = "hosp_id"))

O_E = O_E %>%
  mutate(sd = 1/E * sqrt(mult))

O_E = O_E %>%
  mutate(hosp_id = factor(hosp_id, levels = store$cluster)) %>%
  arrange(hosp_id)

forest(metagen(TE = O_E,
               seTE = sd,
               studlab = hosp_id,
               data =  O_E %>%
                 left_join(fk %>% count(hosp_id) %>% rename("SampleSize" = "n"), by = c("hosp_id" = "hosp_id")) %>% 
                 left_join(fk %>% count(hosp_id, Died) %>% filter(Died == "1") %>% rename("Events" = "n"), , by = c("hosp_id" = "hosp_id")),
               prediction = F, 
               method.random.ci = "HK",
               method.predict = "HK",
               n.e = Events,
               n.c = SampleSize,
               label.e = "",
               label.c = "",
               common = F,
               text.random = "Random-effects pooled estimate"),
       pooled.totals = F,
       right.cols = c("effect", "ci", "w.random"),
       rightlabs = c("O/E ratio", "95% CI", "Weight"),
       leftcols = c("studlab", "n.e", "n.c"),
       leftlabs = c("Hospital ID", "No. of deaths", "Sample Size"),
       ref = 1,
       xlim = c(-0.4, 2))

#Brier score prior to recalibration
simpen %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)


#Brier score after recalibration
simpen %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1_recal)


##################Performing decision curve analysis##################
###Prior to recalibration###
load("simpen_xgb_ev_prior.Rda", verbose = T) #Saved external validation results for simplified XGBoost (sXGB)
simpen_combine_before = simpen

load("simpen_LR_ev_prior.Rda") #Saved external validation results for simplified lasso logistic regression (sLR)
simpen_combine_before = simpen_combine_before %>%
  mutate(sLR = simpen$.pred_1)

load("simpen_DT_ev_prior_BAGGED.Rda") #Saved external validation results for decision tree
simpen_combine_before = simpen_combine_before %>%
  mutate(DT = simpen$.pred_1)

simpen_combine_before = simpen_combine_before %>%
  rename(c(sXGB = ".pred_1"))

simpen_combine_before = simpen_combine_before %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_SRD` = if_else(Q52SP02P < 80 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<85_OR_SRD` = if_else(Q52SP02P < 85 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<90_OR_SRD` = if_else(Q52SP02P < 90 | Q37RESPI2 == "YES", 1, 0))

#Decision curve analysis for results prior to recalibration
dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`, 
    data = simpen_combine_before,
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT", "SpO2<80_OR_SRD", "SpO2<85_OR_SRD", "SpO2<90_OR_SRD") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.13)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_SRD" = "#030303",
                                "SpO2<85_OR_SRD" = "#888888",
                                "SpO2<90_OR_SRD" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#DCA for SpO2
dca(Died ~ `SpO2<80` + `SpO2<85` + `SpO2<90`, 
    data = simpen_combine_before,
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("SpO2<80", "SpO2<85", "SpO2<90") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.13)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c(
                                "SpO2<80" = "#030303",
                                "SpO2<85" = "#888888",
                                "SpO2<90" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288"
      )) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")


###After recalibration###
load("simpen_xgb_ev_after.Rda", verbose = T) #Saved external validation results for simplified XGBoost (sXGB)
simpen_combine_after = simpen

load("simpen_LR_ev_after.Rda") #Saved external validation results for simplified lasso logistic regression (sLR)
simpen_combine_after = simpen_combine_after %>%
  mutate(sLR = simpen$.pred_1_recal)

load("simpen_DT_ev_after_BAGGED.Rda") #Saved external validation results for decision tree
simpen_combine_after = simpen_combine_after %>%
  mutate(DT = simpen$.pred_1_recal)

simpen_combine_after = simpen_combine_after %>%
  rename(c(sXGB = ".pred_1_recal"))

simpen_combine_after = simpen_combine_after %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_SRD` = if_else(Q52SP02P < 80 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<85_OR_SRD` = if_else(Q52SP02P < 85 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<90_OR_SRD` = if_else(Q52SP02P < 90 | Q37RESPI2 == "YES", 1, 0))


#Decision curve analysis for results after recalibration
dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`, 
    data = simpen_combine_after,
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT", "SpO2<80_OR_SRD", "SpO2<85_OR_SRD", "SpO2<90_OR_SRD") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.13)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_SRD" = "#030303",
                                "SpO2<85_OR_SRD" = "#888888",
                                "SpO2<90_OR_SRD" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")



##################Subgroup analysis based on Sex##################
###Prior recalibration###
#Load as needed
load("simpen_xgb_ev_prior.Rda", verbose = T) #Saved external validation results for simplified XGBoost (sXGB)
load("simpen_LR_ev_prior.Rda") #Saved external validation results for simplified lasso logistic regression (sLR)
load("simpen_DT_ev_prior.Rda")

simpen = simpen %>%
  mutate(child_sex = Q12SEX)

#This loop would print out the C-statistic, calibration slope, calibration intercept, and O/E ratio for each subgroup.
for (i in c("Female", "Male")) {
  print(i)
  
  dat = simpen %>%
    filter(child_sex == i) %>%
    mutate(Died = factor(Died, levels = c("0", "1")))
  
  #C-stat
  c_se = sqrt(pROC::var(pROC::roc(dat$Died, dat$.pred_1)))
  print("C-stat")
  print(pROC::roc(dat$Died, dat$.pred_1)$auc)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc - 1.96 * c_se)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc + 1.96 * c_se)
  
  
  
  #Calibration slope
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = dat)
  
  
  print("CS")
  print(coef(summary(slope_pc))[2,1])
  print(coef(summary(slope_pc))[2,1] - 1.96 * coef(summary(slope_pc))[2,2])
  print(coef(summary(slope_pc))[2,1] + 1.96 * coef(summary(slope_pc))[2,2])
  
  
  #Calibration intercept
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = dat)
  
  print("CI") 
  print(coef(summary(intercept_pc))[1,1])
  print(coef(summary(intercept_pc))[1,1] - 1.96 * coef(summary(intercept_pc))[1,2])
  print(coef(summary(intercept_pc))[1,1] + 1.96 * coef(summary(intercept_pc))[1,2])
  
  #O/E ratio
  dat = dat %>%
    mutate(mult = .pred_1*(1-.pred_1))
  
  O = sum(as.numeric(as.character(dat$Died)))
  E = sum(dat$.pred_1)
  O_E = O/E
  mult = sum(dat$mult)
  sd = (1/E) * sqrt(mult)
  
  
  print("O/E")
  print(O_E)
  print(O_E - 1.96*sd)
  print(O_E + 1.96*sd)
  
}

#Brier Class for female
simpen %>%
  filter(child_sex == "Female") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)

#Brier Class for male
simpen %>%
  filter(child_sex == "Male") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)

#Compiling all the results for decision curve analysis
load("simpen_xgb_ev_prior.Rda", verbose = T) #sXGB
simpen_combine_before = simpen

load("simpen_LR_ev_prior.Rda") #sLR
simpen_combine_before = simpen_combine_before %>%
  mutate(sLR = simpen$.pred_1)

load("simpen_DT_ev_prior.Rda") #DT
simpen_combine_before = simpen_combine_before %>%
  mutate(DT = simpen$.pred_1)

simpen_combine_before = simpen_combine_before %>%
  rename(c(sXGB = ".pred_1"))

simpen_combine_before = simpen_combine_before %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_SRD` = if_else(Q52SP02P < 80 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<85_OR_SRD` = if_else(Q52SP02P < 85 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<90_OR_SRD` = if_else(Q52SP02P < 90 | Q37RESPI2 == "YES", 1, 0))


#Decision curve analysis for female
fem = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`, 
          data = simpen_combine_before %>%
            filter(child_sex == "Female"),
          thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT", "SpO2<80_OR_SRD", "SpO2<85_OR_SRD", "SpO2<90_OR_SRD") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_SRD" = "#030303",
                                "SpO2<85_OR_SRD" = "#888888",
                                "SpO2<90_OR_SRD" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#Decision curve analysis for male
mal = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`, 
          data = simpen_combine_before %>%
            filter(child_sex == "Male"),
          thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT", "SpO2<80_OR_SRD", "SpO2<85_OR_SRD", "SpO2<90_OR_SRD") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_SRD" = "#030303",
                                "SpO2<85_OR_SRD" = "#888888",
                                "SpO2<90_OR_SRD" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

###After recalibration###
#Load as needed.
load("simpen_xgb_ev_after.Rda", verbose = T) #Saved external validation results for simplified XGBoost (sXGB)
load("simpen_LR_ev_after.Rda", verbose = T) #Saved external validation results for simplified lasso logistic regression (sLR)
load("simpen_DT_ev_after.Rda", verbose = T) #Saved external validation results for decision tree

simpen = simpen %>%
  mutate(child_sex = Q12SEX)

for (i in c("Female", "Male")) {
  print(i)
  
  dat = simpen %>%
    filter(child_sex == i) %>%
    mutate(Died = factor(Died, levels = c("0", "1"))) %>%
    mutate(.pred_1 = .pred_1_recal, #In the recalibrated dataset, the recalibrated predicted probability and linear predictor has .pred_1_recal and raw_lp_recal as its name, respectively. 
           raw_lp = raw_lp_recal) #They were both transformed so we could use existing coude without changing everything.
  
  #C-stat
  c_se = sqrt(pROC::var(pROC::roc(dat$Died, dat$.pred_1)))
  print("C-stat")
  print(pROC::roc(dat$Died, dat$.pred_1)$auc)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc - 1.96 * c_se)
  print(pROC::roc(dat$Died, dat$.pred_1)$auc + 1.96 * c_se)
  
  
  
  #Calibration slope
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = dat)
  
  
  print("CS")
  print(coef(summary(slope_pc))[2,1])
  print(coef(summary(slope_pc))[2,1] - 1.96 * coef(summary(slope_pc))[2,2])
  print(coef(summary(slope_pc))[2,1] + 1.96 * coef(summary(slope_pc))[2,2])
  
  
  #Calibration intercept
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = dat)
  
  print("CI") 
  print(coef(summary(intercept_pc))[1,1])
  print(coef(summary(intercept_pc))[1,1] - 1.96 * coef(summary(intercept_pc))[1,2])
  print(coef(summary(intercept_pc))[1,1] + 1.96 * coef(summary(intercept_pc))[1,2])
  
  #O/E ratio
  dat = dat %>%
    mutate(mult = .pred_1*(1-.pred_1))
  
  O = sum(as.numeric(as.character(dat$Died)))
  E = sum(dat$.pred_1)
  O_E = O/E
  mult = sum(dat$mult)
  sd = (1/E) * sqrt(mult)
  
  
  print("O/E")
  print(O_E)
  print(O_E - 1.96*sd)
  print(O_E + 1.96*sd)
  
}

#Brier Class for female
simpen %>%
  filter(child_sex == "Female") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1_recal)

#Brier Class for male
simpen %>%
  filter(child_sex == "Male") %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1_recal)

#Compiling all the results for decision curve analysis
load("simpen_xgb_ev_after.Rda", verbose = T)
simpen_combine_after = simpen

load("simpen_LR_ev_after.Rda", verbose = T)
simpen_combine_after = simpen_combine_after %>%
  mutate(sLR = simpen$.pred_1_recal)

load("simpen_DT_ev_after.Rda", verbose = T)
simpen_combine_after = simpen_combine_after %>%
  mutate(DT = simpen$.pred_1_recal)

simpen_combine_after = simpen_combine_after %>%
  rename(c(sXGB = ".pred_1_recal"))

simpen_combine_after = simpen_combine_after %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_SRD` = if_else(Q52SP02P < 80 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<85_OR_SRD` = if_else(Q52SP02P < 85 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<90_OR_SRD` = if_else(Q52SP02P < 90 | Q37RESPI2 == "YES", 1, 0))


#Decision curve analysis for female
fem2 = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`, 
           data = simpen_combine_after %>%
             filter(child_sex == "Female"),
           thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT", "SpO2<80_OR_SRD", "SpO2<85_OR_SRD", "SpO2<90_OR_SRD") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_SRD" = "#030303",
                                "SpO2<85_OR_SRD" = "#888888",
                                "SpO2<90_OR_SRD" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#Decision curve analysis for male
mal2 = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`, 
           data = simpen_combine_after %>%
             filter(child_sex == "Male"),
           thresholds = seq(0, 0.4, 0.01)) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  dplyr::filter(!is.na(standardized_net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("sXGB", "sLR", "DT", "SpO2<80_OR_SRD", "SpO2<85_OR_SRD", "SpO2<90_OR_SRD") ~ "Model",
    label %in% c("Treat All", "Treat None") ~ "Default"
  )) %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("sXGB" = "#117733", 
                                "sLR" = "#DDCC77",
                                "DT" = "#D55E00",
                                "SpO2<80_OR_SRD" = "#030303",
                                "SpO2<85_OR_SRD" = "#888888",
                                "SpO2<90_OR_SRD" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  scale_linetype_manual(values = c(
    "Model" = "solid",
    "Individual" = "dashed",
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

