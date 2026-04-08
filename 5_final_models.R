###Building final models###
#This code display the final models built for looking at the variable importance and prediction purposes.

###Getting the packages

#install.packages(c("tidyverse", "haven", "naniar", "visdat", "tidyselect", "do", "broom", "eeptools", "gtsummary", "mice", "tidymodels", "rlang", "themis", "yardstick", "ggparallel", "vip", "probably", "xgboost", "MLmetrics", "CalibrationCurves", "meta", "pimeta", "rpart.plot", "rpart", "ggpubr", "DescTools", "pROC", "grid", "visreg", "regressinator", "micemd", "miceadds", "tcltk", "glmm", "lme4", "pseudo", "this.path"))

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
library(miceadds)
library(tcltk)
library(glmm)
library(lme4)
library(pseudo)
library(splines2)
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


#######################Final Lasso logistic regression with bagged tree imputation; prior simplification#####################
final_data = final_neo %>%
  mutate(Died = factor(Died, levels = c("1","0"))) %>%
  mutate(AdDxMENG = factor(AdDxMENG, levels = c("none", "Meningitis / Encephalitis")),
         AdDxASPH = factor(AdDxASPH, levels = c("none", "Birth Asphyxia")),
         AdDxMEC = factor(AdDxMEC, levels = c("none", "Mec Aspiration")),
         AdDxSEPS = factor(AdDxSEPS, levels = c("none", "neonatal sepsis")))

child_rec <- recipe(Died ~ AdDxMENG + AdDxSEPS + AdDxASPH + AdDxMEC + AdDxRDS + Q12SEX + Q37RESPI2 + Q38CYANO2 + Q38MATER2 + Q39PROLO2 + Q40JAUND2 + Q40OFFEN2 + Q42LETHA2 + Q42UNBRE2 + Q43CONVU2 + Q43HYPOT2 + Q48WEIGH + Q49SEVER2 + Q50UMBLI2 + Q50HEART + Q51RESPI + Q51BULGI2 + Q52SP02P + Q53TEMPE + preterm + FullOxygen, 
                    data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_poly(Q50HEART, Q51RESPI, Q53TEMPE, Q52SP02P, deg_free = 2) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>% 
  step_scale(all_predictors(), -all_outcomes()) %>% 

wf <- workflow() %>%
  add_recipe(child_rec)

set.seed(1111)
child_boot <- vfold_cv(final_data, strata = Died, v = 5)

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") 

set.seed(1111)
lambda_grid <- grid_random(penalty(), deg_free = deg_free(range = c(2,4)), 
                           size = 50)

set.seed(1111)
lasso_rs <- tune_grid(
  wf %>%
    add_model(tune_spec),
  resamples = child_boot,
  grid = lambda_grid,
  metrics = metric_set(mn_log_loss))

final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), select_best(lasso_rs, metric = "mn_log_loss"))

lowest_logloss = select_best(lasso_rs, metric = "mn_log_loss")

set.seed(1111)

lasso_fit <- fit(final_lasso, data = final_data)

set.seed(1111)
lasso_varimp = lasso_fit %>%
  fit(final_data) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_logloss$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

#######################Final Lasso logistic regression with bagged tree imputation; after simplification#####################
final_data = final_neo %>%
  mutate(Died = factor(Died, levels = c("1","0"))) %>%
  mutate(AdDxMENG = factor(AdDxMENG, levels = c("none", "Meningitis / Encephalitis")),
         AdDxASPH = factor(AdDxASPH, levels = c("none", "Birth Asphyxia")),
         AdDxMEC = factor(AdDxMEC, levels = c("none", "Mec Aspiration")),
         AdDxSEPS = factor(AdDxSEPS, levels = c("none", "neonatal sepsis")))

child_rec <- recipe(Died ~ Q48WEIGH + Q52SP02P + Q37RESPI2 + Q42UNBRE2  + Q53TEMPE + Q42LETHA2, 
                    data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_poly(Q52SP02P, Q53TEMPE, degree = tune(), options = list(raw = TRUE)) %>%
  #step_center(all_numeric_predictors(), -all_outcomes()) %>% #Not doing this so that the equation can be written more easily.
  #step_scale(all_numeric_predictors(), -all_outcomes(), factor = 2) %>% #This process was done only for the variable importance.
  step_dummy(all_nominal(), -all_outcomes()) 

wf <- workflow() %>%
  add_recipe(child_rec)

set.seed(1111)
child_boot <- vfold_cv(final_data, strata = Died, v = 5)

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") #standardize = TRUE so that the coefficient returned will automatically be turned into original scale.

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

lowest_logloss = select_best(lasso_rs, metric = "mn_log_loss")

set.seed(1111)

lasso_fit <- fit(final_lasso, data = final_data)

set.seed(1111)
lasso_varimp = lasso_fit %>%
  fit(final_data) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_logloss$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)


###########Final XGBoost prior simplification###########
final_data = final_neo %>%
  mutate(Died = factor(Died, levels = c("1","0")))

set.seed(1111)
child_res_xg <- vfold_cv(final_data, 
                         v = 5,
                         strata = Died)

xgb_rec <- recipe(Died ~ AdDxMENG + AdDxSEPS + AdDxASPH + AdDxMEC + AdDxRDS + Q12SEX + Q37RESPI2 + Q38CYANO2 + Q38MATER2 + Q39PROLO2 + Q40JAUND2 + Q40OFFEN2 + Q42LETHA2 + Q42UNBRE2 + Q43CONVU2 + Q43HYPOT2 + Q48WEIGH + Q49SEVER2 + Q50UMBLI2 + Q50HEART + Q51RESPI + Q51BULGI2 + Q52SP02P + Q53TEMPE + preterm + FullOxygen,
                  data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 


###RECIPE

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

set.seed(1111)
xgb_grid <- grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), final_data),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

set.seed(1111)

xgb_rs <- tune_grid(
  xgb_wf,
  resamples = child_res_xg,
  grid = xgb_grid,
  metrics = metric_set(mn_log_loss))

final_xgb <- finalize_workflow(xgb_wf, select_best(xgb_rs, metric = "mn_log_loss"))

xgb_fit <- fit(final_xgb, data = final_data)

#Getting the variable importance
set.seed(1111)
var_imp_xgb = xgb_fit %>%
  fit(data = final_data) %>%
  pull_workflow_fit() %>%
  vip(num_features = 32) 


############Final XGBoost after simplification###########
final_data = final_neo %>%
  mutate(Died = factor(Died, levels = c("1","0")))

set.seed(1111)
child_res_xg <- vfold_cv(final_data, 
                         v = 5,
                         strata = Died)

xgb_rec <- recipe(Died ~ Q52SP02P + Q48WEIGH + Q50HEART + Q53TEMPE + Q51RESPI + Q37RESPI2, data = final_data) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 


###RECIPE

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

set.seed(1111)
xgb_grid <- grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), final_data),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

set.seed(1111)

xgb_rs <- tune_grid(
  xgb_wf,
  resamples = child_res_xg,
  grid = xgb_grid,
  metrics = metric_set(mn_log_loss))

final_xgb <- finalize_workflow(xgb_wf, select_best(xgb_rs, metric = "mn_log_loss"))

xgb_fit <- fit(final_xgb, data = final_data)

#Getting the variable importance
set.seed(1111)
var_imp_xgb = xgb_fit %>%
  fit(data = final_data) %>%
  pull_workflow_fit() %>%
  vip(num_features = 32) 


#######Final Decision tree model######
final_data = final_neo %>%
  mutate(Died = factor(Died, levels = c("1","0")),
         Q52SP02P = ifelse(is.na(Q52SP02P), 150, Q52SP02P))

set.seed(1111)
tree_folds <- vfold_cv(final_data, 
                       v = 5,
                       strata = Died)

tree_rec <- recipe(Died ~ AdDxMENG + AdDxSEPS + AdDxASPH + AdDxMEC + AdDxRDS + Q12SEX + Q37RESPI2 + Q38CYANO2 + Q38MATER2 + Q39PROLO2 + Q40JAUND2 + Q40OFFEN2 + Q42LETHA2 + Q42UNBRE2 + Q43CONVU2 + Q43HYPOT2 + Q48WEIGH + Q49SEVER2 + Q50UMBLI2 + Q50HEART + Q51RESPI + Q51BULGI2 + Q52SP02P + Q53TEMPE + preterm + FullOxygen, 
                   data = final_data)

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

set.seed(1111)
tree_grid <- grid_random(
  cost_complexity(),
  tree_depth(c(1,5)),
  min_n(),
  size = 50
)

tree_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tree_spec)


set.seed(1111)
tree_rs <- tune_grid(
  tree_wf,
  resamples = tree_folds,
  grid = tree_grid,
  metrics = metric_set(mn_log_loss))

final_tree <- finalize_workflow(tree_wf, select_best(tree_rs, metric = "mn_log_loss"))

tree_fit = fit(final_tree, data =  final_data)

var_imp_DT = tree_fit %>%
  vip(geom = "col", num_features = 32) +
  scale_y_continuous(expand = c(0, 0)) 

rpart_plot = tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(extra = 104)
