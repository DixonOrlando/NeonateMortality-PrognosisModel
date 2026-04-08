###R script for running models.

###Getting the packages

library(tidyverse)
library(haven)
library(naniar)
library(visdat)
library(tidyselect)
library(do)
library(broom)
library(eeptools)
library(gtsummary)
library(mice)
library(tidymodels)
library(rlang)
library(themis)
library(yardstick)
library(ggparallel)
library(mice)
library(vip)
library(probably)
library(xgboost)
library(MLmetrics)
library(CalibrationCurves)
library(meta)
library(pimeta)
library(rpart.plot)
library(rpart)
library(ggpubr)
library(DescTools)
library(pROC)
library(grid)
library(visreg)
library(mice)
#library(micemd)
library(miceadds)
library(tcltk)
library(glmm)
library(lme4)
library(pseudo)
library(this.path)

setwd(this.path::here())

###Reading the data

ori <- read_csv("O2_DatsetShare_20190612.csv")

neo <- ori

neo <- neo %>%
  filter(AgeGroup == "Neonate <28 days") #Initial observations number = 16,529

neo <- neo %>%  # Create new column, length of stay 
  
  mutate(DODnew = dmy(DOD)) %>% # date of death or discharge 
  mutate(DOAnew = dmy(DOA)) %>%  
  mutate(AgeD = as.numeric(DODnew-DOAnew, na.rm=TRUE))

neo <- neo %>% ###########CORRECT
  filter(StudyPeriod != "Pre-study") %>%  #Restrict to these time periods as data most reliable; N dropped = 8,813
  filter(as.numeric(Q13DOA2)>=2016) %>% # High % missing SpO2 in POx only period in 2015; N dropped = 582
  mutate(Died = as.factor(if_else(Died == "YES" & AgeD <8, 1, 0))) %>% 
  drop_na(Died) #N dropped = 12

neo = neo %>%
  drop_na(AgeD) #N dropped = 3

neo <- neo %>% #Run this final code.
  mutate(Q48WEIGH = if_else((dmy(DOA) == dmy(DOB) | dmy(DOA) == dmy(DOB) %m+% days(1)) & is.na(Q48WEIGH) == TRUE, Q12BIRTH/1000, Q48WEIGH))

neo <- neo %>% #Run this final code.
  mutate(preterm = if_else(is.na(preterm) == T, DxPreterm, preterm)) %>%
  mutate(preterm = case_when(
    preterm == "Not Preterm" ~ "no",
    preterm == "Preterm" ~ "YES",
    TRUE ~ preterm
  ))

neo <- neo %>% #Run this final code.
  mutate(AdDxSEPS = if_else(AdDxSKIN == "Skin infection" | AdDxLRTI == "LRTI" | AdDxLRTI == "severe LRTI" | AdDxMENG == "Meningitis / Encephalitis", "neonatal sepsis", AdDxSEPS))

neo_mut <- neo %>% #14 observations have been converted into NA. RUN THIS CODE!
  mutate(Q48WEIGH = if_else(Q48WEIGH < 0.550 | Q48WEIGH > 5.5, NA, Q48WEIGH))

neo_mut <- neo_mut %>% #RUN THIS CODE!
  mutate(Q50HEART = if_else(Q50HEART < 50 | Q50HEART > 230, NA, Q50HEART))

neo_mut <- neo_mut %>% #RUN THIS CODE!
  mutate(Q51RESPI = if_else(Q51RESPI >= 120, NA, Q51RESPI))

neo_mut <- neo_mut %>%
  mutate(Q52SP02P = if_else(Q52SP02P <= 30, NA, Q52SP02P))

neo_mut_drop <- neo_mut[rowSums(is.na(neo_mut %>% #RUN THIS CODE
                                        dplyr::select(AdDxMENG, AdDxSEPS, AdDxASPH, AdDxMEC, AdDxRDS,  Q12SEX, Q37RESPI2, Q38CYANO2, Q38MATER2, Q39PROLO2, Q40JAUND2, Q40OFFEN2, Q42LETHA2, Q42UNBRE2, Q43CONVU2, Q43HYPOT2, Q48WEIGH, Q49SEVER2, Q50UMBLI2, Q50HEART, Q51RESPI, Q51BULGI2, Q52SP02P, Q53TEMPE, preterm))) < 15, ] #Dropped from 7,119 to 7,116; 3 observations dropped.


neo_mut_drop <- neo_mut_drop %>% #Run this code.
  mutate(Q005HEAL = case_when(
    Hospital == "H8" ~ 1,
    Hospital == "H2" ~ 2,
    Hospital == "H7" ~ 3,
    Hospital == "H10" ~ 4,
    Hospital == "H5" ~ 5,
    Hospital == "H11" ~ 6,
    Hospital == "H1" ~ 7,
    Hospital == "H6" ~ 8,
    Hospital == "H3" ~ 9,
    Hospital == "H12" ~ 10,
    Hospital == "H9" ~ 11,
    Hospital == "H4" ~ 12
  ))

neo_mut_drop <- neo_mut_drop %>%
  mutate(HospName = case_when(
    Q005HEAL == 1 ~ "Adeoyo Maternity Hospital, IBADAN",
    Q005HEAL == 2 ~ "Baptist Medical Centre, SAKI",
    Q005HEAL == 3 ~ "Mother & Child Hospital, AKURE",
    Q005HEAL == 4 ~ "Oluyoro Catholic Hospital, IBADAN",
    Q005HEAL == 5 ~ "Oni Memorial Childrens, IBADAN",
    Q005HEAL == 6 ~ "Our Lady Fatima Catholic, OSOGBO",
    Q005HEAL == 7 ~ "Sacred Heart Hospital, ABEOKUTA",
    Q005HEAL == 8 ~ "Seventh Day Adventist Hospital, ILE IFE",
    Q005HEAL == 9 ~ "State Hospital, ABEOKUTA",
    Q005HEAL == 10 ~ "State Hospital, OYO",
    Q005HEAL == 11 ~ "State Hospital, SAKI",
    Q005HEAL == 12 ~ "State Specialist Hospital, AKURE"
  ))

final_neo <- neo_mut_drop %>%
  filter(!(Q005HEAL %in% c(6, 11)))

final_neo = final_neo %>%
  mutate(cluster_code = Q005HEAL)

final_neo <- final_neo %>% 
  mutate(across(where(is.character), as.factor))

final_neo = final_neo %>%
  mutate(cluster_code = factor(as.character(cluster_code), levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")))

store = final_neo

final_neo = final_neo %>%
  mutate(AdDxMENG = factor(AdDxMENG, levels = c("none", "Meningitis / Encephalitis")),
         AdDxASPH = factor(AdDxASPH, levels = c("none", "Birth Asphyxia")),
         AdDxMEC = factor(AdDxMEC, levels = c("none", "Mec Aspiration")),
         AdDxSEPS = factor(AdDxSEPS, levels = c("none", "neonatal sepsis")))

###########PREPARATION FOR MICE IMPUTATION###########################

###GETTING NON-LINEARITY###

results = data.frame(var1 = "A", p_val = 1, var2 = "B")
row = 1

#Rescale and center prior to fitting the mixed model, so the model is stable.

final_neo_res = final_neo

final_neo_res[sapply(final_neo_res , is.numeric)] <- scale(final_neo_res [sapply(final_neo_res , is.numeric)])  


for (i in c((final_neo %>% select(AdDxMENG, AdDxSEPS, AdDxASPH, AdDxMEC, AdDxRDS,  Q12SEX, Q37RESPI2, Q38CYANO2, Q38MATER2, Q39PROLO2, Q40JAUND2, Q40OFFEN2, Q42LETHA2, Q42UNBRE2, Q43CONVU2, Q43HYPOT2, Q48WEIGH, Q49SEVER2, Q50UMBLI2, Q50HEART, Q51RESPI, Q51BULGI2, Q52SP02P, Q53TEMPE, preterm) %>% miss_var_summary() %>% filter(n_miss > 0))$variable)) {
  
  if (!(i %in% c("Q48WEIGH", "Q50HEART", "Q51RESPI", "Q52SP02P", "Q53TEMPE"))) {
    form_A1 = as.formula(paste(i, paste("Q48WEIGH", " (1|Hospital)", sep = " +"), sep = " ~ "))
    form_A2 = as.formula(paste(i, paste("Q48WEIGH", " I(Q48WEIGH^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
    
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_A1, 
                                  data = final_neo_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_A2, 
                                                                        data = final_neo_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q48WEIGH"
    row = row + 1
    
    
    form_B1 = as.formula(paste(i, paste("Q50HEART", " (1|Hospital)", sep = " +"), sep = " ~ "))
    form_B2 = as.formula(paste(i, paste("Q50HEART", " I(Q50HEART^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_B1, 
                                  data = final_neo_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_B2, 
                                                                        data = final_neo_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q50HEART"
    row = row + 1
    
    form_C1 = as.formula(paste(i, paste("Q51RESPI", " (1|Hospital)", sep = " +"), sep = " ~ "))
    form_C2 = as.formula(paste(i, paste("Q51RESPI", " I(Q51RESPI^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_C1, 
                                  data = final_neo_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_C2, 
                                                                        data = final_neo_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q51RESPI"
    row = row + 1
    
    form_D1 = as.formula(paste(i, paste("Q52SP02P", " (1|Hospital)", sep = " +"), sep = " ~ "))
    form_D2 = as.formula(paste(i, paste("Q52SP02P", " I(Q52SP02P^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
    results[row, 1] = i
    results[row, 2] = anova(glmer(form_D1, 
                                  data = final_neo_res, 
                                  family = binomial(), nAGQ=30) , glmer(form_D2, 
                                                                        data = final_neo_res, 
                                                                        family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
    results[row, 3] = "Q52SP02P"
    row = row + 1
    
    if (i != "AdDxMEC") {
      form_E1 = as.formula(paste(i, paste("Q53TEMPE", " (1|Hospital)", sep = " +"), sep = " ~ "))
      form_E2 = as.formula(paste(i, paste("Q53TEMPE", " I(Q53TEMPE^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
      results[row, 1] = i
      results[row, 2] = anova(glmer(form_E1, 
                                    data = final_neo_res, 
                                    family = binomial(), nAGQ=30) , glmer(form_E2, 
                                                                          data = final_neo_res, 
                                                                          family = binomial(), nAGQ=30))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q53TEMPE"
      row = row + 1
    }
  }
  
  else {
    if (i != "Q48WEIGH") {
      form_A1 = as.formula(paste(i, paste("Q48WEIGH", " (1|Hospital)", sep = " +"), sep = " ~ "))
      form_A2 = as.formula(paste(i, paste("Q48WEIGH", " I(Q48WEIGH^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
      
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_A1, 
                                   data = final_neo_res) , lmer(form_A2, 
                                                                data = final_neo_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q48WEIGH"
      row = row + 1
    }
    
    if (i != "Q50HEART") {
      form_B1 = as.formula(paste(i, paste("Q50HEART", " (1|Hospital)", sep = " +"), sep = " ~ "))
      form_B2 = as.formula(paste(i, paste("Q50HEART", " I(Q50HEART^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_B1, 
                                   data = final_neo_res) , lmer(form_B2, 
                                                                data = final_neo_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q50HEART"
      row = row + 1
    }
    
    
    if (i != "Q51RESPI") {
      form_C1 = as.formula(paste(i, paste("Q51RESPI", " (1|Hospital)", sep = " +"), sep = " ~ "))
      form_C2 = as.formula(paste(i, paste("Q51RESPI", " I(Q51RESPI^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_C1, 
                                   data = final_neo_res) , lmer(form_C2, 
                                                                data = final_neo_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q51RESPI"
      row = row + 1
    }
    
    if (i != "Q52SP02P") {
      form_D1 = as.formula(paste(i, paste("Q52SP02P", " (1|Hospital)", sep = " +"), sep = " ~ "))
      form_D2 = as.formula(paste(i, paste("Q52SP02P", " I(Q52SP02P^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_D1, 
                                   data = final_neo_res) , lmer(form_D2, 
                                                                data = final_neo_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q52SP02P"
      row = row + 1
    }
    
    
    if (i != "Q53TEMPE") {
      form_E1 = as.formula(paste(i, paste("Q53TEMPE", " (1|Hospital)", sep = " +"), sep = " ~ "))
      form_E2 = as.formula(paste(i, paste("Q53TEMPE", " I(Q53TEMPE^2)", " (1|Hospital)", sep = " +"), sep = " ~ "))
      results[row, 1] = i
      results[row, 2] = anova(lmer(form_E1, 
                                   data = final_neo_res) , lmer(form_E2, 
                                                                data = final_neo_res))$`Pr(>Chisq)`[2]
      results[row, 3] = "Q53TEMPE"
      row = row + 1
    }
  }
  
}

###MICE IMPUTATION FUNCTION###

mice_imp <- function(train, test, id) {
  
  
  #Storing the scaling value so it can be used for reverting the scaled data back to original scale.
  scaling_params_train <- train  %>%
    summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_")
  
  if (id != "Full") {
    scaling_params_test <- test  %>%
      summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_")
  }
  
  
  #Scaling the data prior to imputation
  train[sapply(train , is.numeric)] <- scale(train [sapply(train , is.numeric)]) 
  
  if (id != "Full") {
    test[sapply(test , is.numeric)] <- scale(test [sapply(test , is.numeric)])
  }
  
#  #Loop to remove variables to simplify the model.
#  cat_var <- c("AdDxMENG", "AdDxSEPS", "AdDxASPH", "AdDxMEC", "AdDxRDS",  
#               "Q12SEX", "Q37RESPI2", "Q38CYANO2", "Q38MATER2", "Q39PROLO2",  
#               "Q40JAUND2", "Q40OFFEN2", "Q42LETHA2", "Q42UNBRE2", "Q43CONVU2",  
#               "Q43HYPOT2", "Q49SEVER2", "Q50UMBLI2", "Q51BULGI2", "preterm", "Died")
#  
#  cat_mis <- train %>% #Getting list of categorical variables with missingness.
#    select(AdDxMENG, AdDxSEPS, AdDxASPH, AdDxMEC, AdDxRDS,  Q12SEX, Q37RESPI2, Q38CYANO2, Q38MATER2, Q39PROLO2, Q40JAUND2, Q40OFFEN2, Q42LETHA2, Q42UNBRE2, Q43CONVU2, Q43HYPOT2, Q48WEIGH, Q49SEVER2, Q50UMBLI2, Q50HEART, Q51RESPI, Q51BULGI2, Q52SP02P, Q53TEMPE, preterm, cluster_code) %>%
#    miss_var_summary() %>% 
#    filter(n_miss != 0,
#           !(variable %in% c("Q48WEIGH", "Q50HEART", "Q51RESPI", "Q52SP02P", "Q53TEMPE"))) %>%
#    select(variable) %>%
#    c()
#  
#  pair <- data.frame(A = "a", B = "b")
#  index <- 1
#  
#  for (i in 1:(length(cat_var)-1)){ #Run this code.
#    for (j in (i+1):length(cat_var)){
#      for (k in unique(train$cluster_code)) {
#        len <- train %>% 
#          filter(cluster_code == k) %>%
#          count(A = get(cat_var[i]), B = get(cat_var[j])) %>%
#          filter(is.na(A) == F & is.na(B) == F) %>%
#          nrow()
#        
#        if (len != 4) {
#          pair[index, 1] = cat_var[i]
#          pair[index, 2] = cat_var[j]
#          index = index + 1
#        }
#      }
#    }
#  }
#  
#  pair_unique <- pair %>% #Avoiding imputation not working because some factor variables don't have enough observation for certain level when predicting the missingness.
#    filter(A %in% cat_mis[[1]]) %>%
#    distinct(A, B)
  
  
  #Starting the MICE imputation process.
  imp <- mice(train %>% 
                select(AdDxMENG, AdDxSEPS, AdDxASPH, AdDxMEC, AdDxRDS,  Q12SEX, Q37RESPI2, Q38CYANO2, Q38MATER2, Q39PROLO2, Q40JAUND2, Q40OFFEN2, Q42LETHA2, Q42UNBRE2, Q43CONVU2, Q43HYPOT2, Q48WEIGH, Q49SEVER2, Q50UMBLI2, Q50HEART, Q51RESPI, Q51BULGI2, Q52SP02P, Q53TEMPE, preterm, FullOxygen, cluster_code, Died) %>%
                mutate(cluster_code = as.integer(as.character(cluster_code))) %>%
                mutate(Q48WEIGH_sq = Q48WEIGH^2,
                       Q50HEART_sq = Q50HEART^2,
                       Q51RESPI_sq = Q51RESPI^2,
                       Q52SP02P_sq = Q52SP02P^2,
                       Q53TEMPE_sq = Q53TEMPE^2) 
              ,printFlag = FALSE,
              maxit = 0) # Dry run
  
  pred <- imp$predictorMatrix
  
  pred[pred == 1] <- 1 # Accounts for between-study heterogeneity in the effect of all variables; 2 actually implies a random effect.
  pred["Q53TEMPE_sq", ] = 1 #This step is necessary to fix some issues.
  
  pred[, "Q48WEIGH_sq"] = 0
  pred[, "Q50HEART_sq"] = 0
  pred[, "Q51RESPI_sq"] = 0
  pred[, "Q52SP02P_sq"] = 0
  pred[, "Q53TEMPE_sq"] = 0
  
  pred["Q48WEIGH_sq", "Q48WEIGH"] = 0
  pred["Q50HEART_sq", "Q50HEART"] = 0
  pred["Q51RESPI_sq", "Q51RESPI"] = 0
  pred["Q52SP02P_sq", "Q52SP02P"] = 0
  pred["Q53TEMPE_sq", "Q53TEMPE"] = 0
  
  
  pred[, "cluster_code"] <- -2 # Specify the cluster variable to allow random effects on the intercept term across each practice
  
  results_filt = results %>%
    filter(p_val <= 0.05)
  
  for (i in 1:nrow(results_filt)) {
    pred[results_filt[i,1], paste(results_filt[i,3], "_sq", sep="")] = 1
  }
  
  meth <- imp$method
  
  meth[meth == "pmm"] <- "2l.pmm" # Multi-level predictive mean matching for continuous variables
  
  meth[meth == "logreg"] <- "2l.pmm" # Multi-level logistic regression for binary variables
  
  meth["Q48WEIGH_sq"] <- "~ I(Q48WEIGH^2)"
  meth["Q50HEART_sq"] <- "~ I(Q50HEART^2)"
  meth["Q51RESPI_sq"] <- "~ I(Q51RESPI^2)"
  meth["Q52SP02P_sq"] <- "~ I(Q52SP02P^2)"
  meth["Q53TEMPE_sq"] <- "~ I(Q53TEMPE^2)"
  
  mice <- mice(train %>%
                 select(AdDxMENG, AdDxSEPS, AdDxASPH, AdDxMEC, AdDxRDS,  Q12SEX, Q37RESPI2, Q38CYANO2, Q38MATER2, Q39PROLO2, Q40JAUND2, Q40OFFEN2, Q42LETHA2, Q42UNBRE2, Q43CONVU2, Q43HYPOT2, Q48WEIGH, Q49SEVER2, Q50UMBLI2, Q50HEART, Q51RESPI, Q51BULGI2, Q52SP02P, Q53TEMPE, preterm, FullOxygen, cluster_code, Died) %>%
                 mutate(cluster_code = as.integer(as.character(cluster_code))) %>%
                 mutate(Q48WEIGH_sq = Q48WEIGH^2,
                        Q50HEART_sq = Q50HEART^2,
                        Q51RESPI_sq = Q51RESPI^2,
                        Q52SP02P_sq = Q52SP02P^2,
                        Q53TEMPE_sq = Q53TEMPE^2)
               , m = 1, maxit = 10, meth = meth, pred = pred, 
               seed = 1111)
  
  if (id != "Full") {
    mice_test = mice.mids(mice, 
                          newdata = test %>%
                            select(AdDxMENG, AdDxSEPS, AdDxASPH, AdDxMEC, AdDxRDS,  Q12SEX, Q37RESPI2, Q38CYANO2, Q38MATER2, Q39PROLO2, Q40JAUND2, Q40OFFEN2, Q42LETHA2, Q42UNBRE2, Q43CONVU2, Q43HYPOT2, Q48WEIGH, Q49SEVER2, Q50UMBLI2, Q50HEART, Q51RESPI, Q51BULGI2, Q52SP02P, Q53TEMPE, preterm, FullOxygen, cluster_code, Died) %>%
                            mutate(cluster_code = as.integer(as.character(cluster_code))) %>%
                            mutate(Q48WEIGH_sq = Q48WEIGH^2,
                                   Q50HEART_sq = Q50HEART^2,
                                   Q51RESPI_sq = Q51RESPI^2,
                                   Q52SP02P_sq = Q52SP02P^2,
                                   Q53TEMPE_sq = Q53TEMPE^2), 
                          maxit = 1, 
                          seed = 1111)
  }
  
  #For the training data:
  
  final_train = complete(mice) %>%
    select(-c(Q48WEIGH_sq, Q50HEART_sq, Q51RESPI_sq, Q52SP02P_sq, Q53TEMPE_sq))
  
  Hosp_store = final_train$cluster_code
  
  #Convert back to original scale
  final_train = final_train %>%
    mutate(across(where(is.numeric), ~ .x * scaling_params_train$sd[match(cur_column(), scaling_params_train$variable)] +
                    scaling_params_train$mean[match(cur_column(), scaling_params_train$variable)])) %>%
    mutate(cluster_code = train$cluster_code) %>%
    mutate(tt_id = 1,
           index = id)
  
  if (id != "Full") {
    #For the test data
    final_test = complete(mice_test) %>%
      select(-c(Q48WEIGH_sq, Q50HEART_sq, Q51RESPI_sq, Q52SP02P_sq, Q53TEMPE_sq))
    
    Hosp_store_test = final_test$cluster_code
    
    #Convert back to original scale
    final_test = final_test %>%
      mutate(across(where(is.numeric), ~ .x * scaling_params_test$sd[match(cur_column(), scaling_params_test$variable)] +
                      scaling_params_test$mean[match(cur_column(), scaling_params_test$variable)])) %>%
      mutate(cluster_code = test$cluster_code) %>%
      mutate(tt_id = 2, #2 is for the test data.
             index = id)
    
    final_data = final_train %>% rbind(final_test)
    
    return(final_data)
  }
  
  if (id == "Full") {
    return(final_train)
  }
  
}



######################Lasso logistic regression with bagged imputation; prior simplification#####################
for (i in unique(final_neo$cluster_code)) {
  
  if (i == "1"){
    
    index = 1
    row = 1
    
    metric_measure_clas_imp <- data.frame(pr_auc = 1,
                                          roc_auc = 1,
                                          log_loss = 1,
                                          brier = 1,
                                          cluster_code = "A")
    
    calibration_measure_imp <- data.frame(cal_slope = 1,
                                          cal_int = 1,
                                          
                                          se_slope = 1,
                                          
                                          se_int = 1,
                                          cluster_code = "A")
    
    c_stat_imp <- data.frame(roc_auc = 1,
                             se_roc_auc = 1,
                             cluster_code = "A")
    
    mn_cal <- list(pred = 1, obs = 1)
    
  }
  
  child_train1 <- final_neo %>% #Continuous specification
    filter(cluster_code != i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_test1 <- final_neo %>%
    filter(cluster_code == i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_rec <- recipe(Died ~ AdDxMENG + AdDxSEPS + AdDxASPH + AdDxMEC + AdDxRDS + Q12SEX + Q37RESPI2 + Q38CYANO2 + Q38MATER2 + Q39PROLO2 + Q40JAUND2 + Q40OFFEN2 + Q42LETHA2 + Q42UNBRE2 + Q43CONVU2 + Q43HYPOT2 + Q48WEIGH + Q49SEVER2 + Q50UMBLI2 + Q50HEART + Q51RESPI + Q51BULGI2 + Q52SP02P + Q53TEMPE + preterm + FullOxygen, 
                      data = child_train1) %>%
    step_impute_bag(all_predictors()) %>%
    step_poly(Q50HEART, Q51RESPI, Q53TEMPE, Q52SP02P, degree = tune()) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_numeric_predictors(), -all_outcomes()) %>%
    step_scale(all_numeric_predictors(), -all_outcomes()) 
  
  wf <- workflow() %>%
    add_recipe(child_rec)
  
  set.seed(1111)
  child_boot <- vfold_cv(child_train1, strata = Died, v = 5)
  
  tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", standardize = FALSE)
  
  set.seed(1111)
  lambda_grid <- grid_random(penalty(), degree = degree_int(range = c(2,3)), 
                             size = 50)
  
  set.seed(1111)
  lasso_rs <- tune_grid(
    wf %>%
      add_model(tune_spec),
    resamples = child_boot,
    grid = lambda_grid,
    metrics = metric_set(mn_log_loss))
  
  final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), select_best(lasso_rs, metric = "mn_log_loss"))
  
  set.seed(1111)
  
  lasso_fit <- fit(final_lasso, data = child_train1)
  
  cell_test_pred <- augment(lasso_fit, new_data = child_test1)
  
  cell_test_pred <- cell_test_pred %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  cell_test_pred <- cell_test_pred %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = cell_test_pred) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = cell_test_pred)
  
  metric_measure_clas_imp[row, 1] <- (cell_test_pred %>% pr_auc(Died, .pred_1))[3] #PR-AUC
  metric_measure_clas_imp[row, 2] <- (cell_test_pred %>% roc_auc(Died, .pred_1))[3] #ROC-AUC
  metric_measure_clas_imp[row, 3] <- (cell_test_pred %>% mn_log_loss(truth = Died, .pred_1))[3] #log-loss
  metric_measure_clas_imp[row, 4] <-  (cell_test_pred %>% brier_class(truth = Died, .pred_1))[3] #Brier
  metric_measure_clas_imp[row, 5] <- i
  
  calibration_measure_imp[row, 1] <- coef(summary(slope_pc))[2,1] #cal_slope
  calibration_measure_imp[row, 2] <- coef(summary(intercept_pc))[1,1] #cal_int
  calibration_measure_imp[row, 3] <- coef(summary(slope_pc))[2,2] #se_slope
  calibration_measure_imp[row, 4] <- coef(summary(intercept_pc))[1,2]#se_int
  calibration_measure_imp[row, 5] <- i #cluster_code
  
  c_stat_imp[row, 1] <- pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)$auc
  c_stat_imp[row, 2] <- sqrt(var(pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)))
  c_stat_imp[row, 3] <- i
  
  
  if (i == "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- p
    mn_cal$obs <- y
    
    simpen <- cell_test_pred
  }
  
  if (i != "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- c(mn_cal$pred, p)
    mn_cal$obs <- c(mn_cal$obs, y)
    
    simpen <- rbind(simpen, cell_test_pred)
  }
  
  
  
  row = row + 1
}


#######################Lasso logistic regression with bagged imputation; after simplification#####################
for (i in unique(final_neo$cluster_code)) {
  
  if (i == "1"){
    
    index = 1
    row = 1
    
    metric_measure_clas_imp <- data.frame(pr_auc = 1,
                                          roc_auc = 1,
                                          log_loss = 1,
                                          brier = 1,
                                          cluster_code = "A")
    
    calibration_measure_imp <- data.frame(cal_slope = 1,
                                          cal_int = 1,
                                          
                                          se_slope = 1,
                                          
                                          se_int = 1,
                                          cluster_code = "A")
    
    c_stat_imp <- data.frame(roc_auc = 1,
                             se_roc_auc = 1,
                             cluster_code = "A")
    
    mn_cal <- list(pred = 1, obs = 1)
    
  }
  
  child_train1 <- final_neo %>% #Continuous specification
    filter(cluster_code != i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_test1 <- final_neo %>%
    filter(cluster_code == i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_rec <- recipe(Died ~ Q48WEIGH + Q52SP02P + Q37RESPI2 + Q42UNBRE2  + Q53TEMPE + Q42LETHA2, 
                      data = child_train1) %>%
    step_impute_bag(all_predictors()) %>%
    step_poly(Q52SP02P, Q53TEMPE, degree = tune()) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_numeric_predictors(), -all_outcomes()) %>%
    step_scale(all_numeric_predictors(), -all_outcomes())
  
  wf <- workflow() %>%
    add_recipe(child_rec)
  
  set.seed(1111)
  child_boot <- vfold_cv(child_train1, strata = Died, v = 5)
  
  tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", standardize = FALSE)
  
  set.seed(1111)
  lambda_grid <- grid_random(penalty(), degree = degree_int(range = c(2,3)), 
                             size = 50)
  
  set.seed(1111)
  lasso_rs <- tune_grid(
    wf %>%
      add_model(tune_spec),
    resamples = child_boot,
    grid = lambda_grid,
    metrics = metric_set(mn_log_loss))
  
  final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), select_best(lasso_rs, metric = "mn_log_loss"))
  
  set.seed(1111)
  
  lasso_fit <- fit(final_lasso, data = child_train1)
  
  cell_test_pred <- augment(lasso_fit, new_data = child_test1)
  
  cell_test_pred <- cell_test_pred %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  cell_test_pred <- cell_test_pred %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = cell_test_pred) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = cell_test_pred)
  
  metric_measure_clas_imp[row, 1] <- (cell_test_pred %>% pr_auc(Died, .pred_1))[3] #PR-AUC
  metric_measure_clas_imp[row, 2] <- (cell_test_pred %>% roc_auc(Died, .pred_1))[3] #ROC-AUC
  metric_measure_clas_imp[row, 3] <- (cell_test_pred %>% mn_log_loss(truth = Died, .pred_1))[3] #log-loss
  metric_measure_clas_imp[row, 4] <-  (cell_test_pred %>% brier_class(truth = Died, .pred_1))[3] #Brier
  metric_measure_clas_imp[row, 5] <- i
  
  calibration_measure_imp[row, 1] <- coef(summary(slope_pc))[2,1] #cal_slope
  calibration_measure_imp[row, 2] <- coef(summary(intercept_pc))[1,1] #cal_int
  calibration_measure_imp[row, 3] <- coef(summary(slope_pc))[2,2] #se_slope
  calibration_measure_imp[row, 4] <- coef(summary(intercept_pc))[1,2]#se_int
  calibration_measure_imp[row, 5] <- i #cluster_code
  
  c_stat_imp[row, 1] <- pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)$auc
  c_stat_imp[row, 2] <- sqrt(var(pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)))
  c_stat_imp[row, 3] <- i
  
  
  if (i == "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- p
    mn_cal$obs <- y
    
    simpen <- cell_test_pred
  }
  
  if (i != "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- c(mn_cal$pred, p)
    mn_cal$obs <- c(mn_cal$obs, y)
    
    simpen <- rbind(simpen, cell_test_pred)
  }
  
  
  
  row = row + 1
}


######################Lasso logistic regression with MICE imputation; prior simplification#####################
final_neo = store

for (i in unique(final_neo$cluster_code)) {
  
  if (i == "1"){
    
    index = 1
    row = 1
    
    metric_measure_clas_imp <- data.frame(pr_auc = 1,
                                          roc_auc = 1,
                                          log_loss = 1,
                                          brier = 1,
                                          cluster_code = "A")
    
    calibration_measure_imp <- data.frame(cal_slope = 1,
                                          cal_int = 1,
                                          
                                          se_slope = 1,
                                          
                                          se_int = 1,
                                          cluster_code = "A")
    
    c_stat_imp <- data.frame(roc_auc = 1,
                             se_roc_auc = 1,
                             cluster_code = "A")
    
    mn_cal <- list(pred = 1, obs = 1)
    
  }
  
  child_train1 <- final_neo %>% #Continuous specification
    filter(cluster_code != i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_test1 <- final_neo %>%
    filter(cluster_code == i) %>%
    mutate(Died = factor(Died, levels = c("1","0")))
  
  child_rec <- recipe(Died ~ AdDxMENG + AdDxSEPS + AdDxASPH + AdDxMEC + AdDxRDS + Q12SEX + Q37RESPI2 + Q38CYANO2 + Q38MATER2 + Q39PROLO2 + Q40JAUND2 + Q40OFFEN2 + Q42LETHA2 + Q42UNBRE2 + Q43CONVU2 + Q43HYPOT2 + Q48WEIGH + Q49SEVER2 + Q50UMBLI2 + Q50HEART + Q51RESPI + Q51BULGI2 + Q52SP02P + Q53TEMPE + preterm + FullOxygen, 
                      data = child_train1) %>%
    step_poly(Q50HEART, Q51RESPI, Q53TEMPE, Q52SP02P, degree = tune()) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_center(all_numeric_predictors(), -all_outcomes()) %>%
    step_scale(all_numeric_predictors(), -all_outcomes())
  
  wf <- workflow() %>%
    add_recipe(child_rec)
  
  set.seed(1111)
  child_boot <- vfold_cv(child_train1, strata = Died, v = 5)
  
  ####LOOP TO PERFORM IMPUTATION INSIDE THE VFOLD CV####
  
  for (k in 1:5){
    df_train <- get_rsplit(child_boot, index = k) %>%
      analysis() 
    
    df_test <- get_rsplit(child_boot, index = k) %>%
      assessment() 
    
    if (k == 1) {
      boot_data = mice_imp(train = df_train, test = df_test, id = k)
    }
    
    if (k == 2) {
      add_boot = mice_imp(train = df_train, test = df_test, id = k)
      
      boot_data = boot_data %>%
        rbind(add_boot)
      
      a = list(analysis = which(boot_data$tt_id == 1 & boot_data$index == 1)[1] : which(boot_data$tt_id == 1 & boot_data$index == 1)[length(which(boot_data$tt_id == 1 & boot_data$index == 1))],
               assessment = which(boot_data$tt_id == 2 & boot_data$index == 1)[1] : which(boot_data$tt_id == 2 & boot_data$index == 1)[length(which(boot_data$tt_id == 2 & boot_data$index == 1))]
      )
      
      b = list(analysis = which(boot_data$tt_id == 1 & boot_data$index == 2)[1] : which(boot_data$tt_id == 1 & boot_data$index == 2)[length(which(boot_data$tt_id == 1 & boot_data$index == 2))],
               assessment = which(boot_data$tt_id == 2 & boot_data$index == 2)[1] : which(boot_data$tt_id == 2 & boot_data$index == 2)[length(which(boot_data$tt_id == 2 & boot_data$index == 2))]
      )
      
      indices = append(list(a), list(b))
    }
    
    if (k > 2) {
      boot_data = boot_data %>%
        rbind(mice_imp(train = df_train, test = df_test, id = k))
      
      c = list(analysis = which(boot_data$tt_id == 1 & boot_data$index == k)[1] : which(boot_data$tt_id == 1 & boot_data$index == k)[length(which(boot_data$tt_id == 1 & boot_data$index == k))],
               assessment = which(boot_data$tt_id == 2 & boot_data$index == k)[1] : which(boot_data$tt_id == 2 & boot_data$index == k)[length(which(boot_data$tt_id == 2 & boot_data$index == k))]
      )
      
      indices = indices %>%
        append(list(c))
    }
    
    
  }
  
  splits <- lapply(indices, make_splits, data = boot_data)
  
  child_boot <- manual_rset(splits, c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5"))
  
  ######################################################
  
  tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet", standardize = FALSE)
  
  set.seed(1111)
  lambda_grid <- grid_random(penalty(), degree = degree_int(range = c(2,3)), 
                             size = 50)
  
  set.seed(1111)
  lasso_rs <- tune_grid(
    wf %>%
      add_model(tune_spec),
    resamples = child_boot,
    grid = lambda_grid,
    metrics = metric_set(mn_log_loss))
  
  final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), select_best(lasso_rs, metric = "mn_log_loss"))
  
  #####PERFORMING MICE IMPUTATION ON THE FULL DATA########
  
  full_imp <- mice_imp(train = child_train1,
                       test = child_test1,
                       id = "Full Data")
  
  child_train1 <- full_imp %>%
    filter(cluster_code != i) 
  
  child_test1 <- full_imp %>%
    filter(cluster_code == i)
  
  ########################################################
  
  set.seed(1111)
  
  lasso_fit <- fit(final_lasso, data = child_train1)
  
  cell_test_pred <- augment(lasso_fit, new_data = child_test1)
  
  cell_test_pred <- cell_test_pred %>%
    mutate(.pred_1 = ifelse(.pred_1 == 0, 1e-8, .pred_1),
           .pred_1 = ifelse(.pred_1 == 1, 1 - 1e-8, .pred_1))
  
  cell_test_pred <- cell_test_pred %>%
    mutate(raw_lp = log(.pred_1/(1-.pred_1)))
  
  
  slope_pc <- glm(factor(Died, levels = c("0","1")) ~ raw_lp, 
                  family = binomial, 
                  data = cell_test_pred) 
  
  intercept_pc <- glm(factor(Died, levels = c("0","1")) ~ 1, 
                      offset = raw_lp, 
                      family = binomial,
                      data = cell_test_pred)
  
  metric_measure_clas_imp[row, 1] <- (cell_test_pred %>% pr_auc(Died, .pred_1))[3] #PR-AUC
  metric_measure_clas_imp[row, 2] <- (cell_test_pred %>% roc_auc(Died, .pred_1))[3] #ROC-AUC
  metric_measure_clas_imp[row, 3] <- (cell_test_pred %>% mn_log_loss(truth = Died, .pred_1))[3] #log-loss
  metric_measure_clas_imp[row, 4] <-  (cell_test_pred %>% brier_class(truth = Died, .pred_1))[3] #Brier
  metric_measure_clas_imp[row, 5] <- i
  
  calibration_measure_imp[row, 1] <- coef(summary(slope_pc))[2,1] #cal_slope
  calibration_measure_imp[row, 2] <- coef(summary(intercept_pc))[1,1] #cal_int
  calibration_measure_imp[row, 3] <- coef(summary(slope_pc))[2,2] #se_slope
  calibration_measure_imp[row, 4] <- coef(summary(intercept_pc))[1,2]#se_int
  calibration_measure_imp[row, 5] <- i #cluster_code
  
  c_stat_imp[row, 1] <- pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)$auc
  c_stat_imp[row, 2] <- sqrt(var(pROC::roc(cell_test_pred$Died, cell_test_pred$.pred_1)))
  c_stat_imp[row, 3] <- i
  
  
  if (i == "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- p
    mn_cal$obs <- y
    
    simpen <- cell_test_pred
  }
  
  if (i != "1") {
    p <- unlist(predict(lasso_fit, child_test1, type = "prob")[1])
    y <- as.numeric(as.character((child_test1)$Died))
    
    mn_cal$pred <- c(mn_cal$pred, p)
    mn_cal$obs <- c(mn_cal$obs, y)
    
    simpen <- rbind(simpen, cell_test_pred)
  }
  
  
  
  row = row + 1
}



