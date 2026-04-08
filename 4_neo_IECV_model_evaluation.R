###Presenting the results of the neonate models

library(tidyverse)
library(meta)
library(dcurves)
library(tidymodels)
library(CalibrationCurves)
library(pmcalibration)
library(MuMIn)
library(metafor)
library(netmeta)
library(rmda)
library(grid)
library(moments)

#########Comparing Lasso logistic regression method#########

#Load the results
setwd("C:/Users/Dorla/OneDrive/NeoPredModel")

load("mn_cal_lasso_baggedtree_neo.Rda", verbose = T)
load("metric_lasso_baggedtree_neo.Rda", verbose = T)
load("cal_lasso_baggedtree_neo.Rda", verbose = T)
load("cstat_lasso_baggedtree_neo.Rda", verbose = T)
load("simpen_lasso_baggedtree_neo.Rda", verbose = T)

load("mn_cal_lasso_baggedtree_neo_poly.Rda", verbose = T)
load("metric_lasso_baggedtree_neo_poly.Rda", verbose = T)
load("cal_lasso_baggedtree_neo_poly.Rda", verbose = T)
load("cstat_lasso_baggedtree_neo_poly.Rda", verbose = T)
load("simpen_lasso_baggedtree_neo_poly.Rda", verbose = T)

load("modified_mn_cal_lasso_baggedtree_neo.Rda", verbose = T)
load("modified_metric_lasso_baggedtree_neo.Rda", verbose = T)
load("modified_cal_lasso_baggedtree_neo.Rda", verbose = T)
load("modified_cstat_lasso_baggedtree_neo.Rda", verbose = T)
load("modified_simpen_lasso_baggedtree_neo.Rda", verbose = T)

load("modifiedadd_mn_cal_lasso_baggedtree_neo.Rda", verbose = T)
load("modifiedadd_metric_lasso_baggedtree_neo.Rda", verbose = T)
load("modifiedadd_cal_lasso_baggedtree_neo.Rda", verbose = T)
load("modifiedadd_cstat_lasso_baggedtree_neo.Rda", verbose = T)
load("modifiedadd_simpen_lasso_baggedtree_neo.Rda", verbose = T)

load("modified_mn_cal_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)
load("modified_metric_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)
load("modified_cal_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)
load("modified_cstat_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)
load("modified_simpen_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)

load("mn_cal_lasso_MICE_neo.Rda", verbose = T)
load("metric_lasso_MICE_neo.Rda", verbose = T)
load("cal_lasso_MICE_neo.Rda", verbose = T)
load("cstat_lasso_MICE_neo.Rda", verbose = T)
load("simpen_lasso_MICE_neo.Rda", verbose = T)

load("mn_cal_lasso_MICE_neo_poly.Rda", verbose = T)
load("metric_lasso_MICE_neo_poly.Rda", verbose = T)
load("cal_lasso_MICE_neo_poly.Rda", verbose = T)
load("cstat_lasso_MICE_neo_poly.Rda", verbose = T)
load("simpen_lasso_MICE_neo_poly.Rda", verbose = T)

##############################Only lasso logistic regression is used as an example/demonstration for this code##############################

#The reason we do not provide examples for all the models because there are too many of them. However, the other models could use the same code below to evaluate its performance.

###Load all the results for the simplified lasso logistic regression (sLR) in the IECV###
load("modified_mn_cal_lasso_baggedtree_neo_poly_notre.Rda", verbose = T) #This will give calibration_measure_imp data frame, which contains the quantitative measures of calibration performance.
load("modified_cstat_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)  #This will give c_stat_imp data frame, which contains the C-statistic.
load("modified_simpen_lasso_baggedtree_neo_poly_notre.Rda", verbose = T) #This will give simpen data frame, which contains the whole data frame used for prediction, including the predicted probability from the model.

###O/E ratio###
O_E = simpen %>% 
  dplyr::group_by(cluster_code) %>% 
  dplyr::summarize(O = sum(as.numeric(as.character(Died))), E = sum(.pred_1)) %>% mutate(O_E = O/E) %>%
  left_join(simpen %>%
              mutate(mult = .pred_1*(1-.pred_1)) %>%
              select(mult, cluster_code) %>%
              dplyr::group_by(cluster_code) %>%
              dplyr::summarize(mult = sum(mult)),
            by = c("cluster_code" = "cluster_code"))

O_E = O_E %>%
  mutate(sd = 1/E * sqrt(mult))

forest(metagen(TE = O_E,
               seTE = sd,
               studlab = cluster_code,
               data =  O_E,
               prediction = T, 
               method.random.ci = "HK",
               method.predict = "HK"),
       xlim = c(0.5, 1.5))
grid.text("Lasso logistic regression O/E ratio", .5, 0.8, gp=gpar(cex=1.5))

#Calibration slope
forest(metagen(TE = cal_slope,
               seTE = se_slope,
               studlab = cluster_code,
               data = calibration_measure_imp,
               null = 1,
               prediction = T,
               method.random.ci  = "HK",
               method.predict = "HK", 
               method.tau = "REML"),
       #sortvar = TE,
       ref = 1,
       xlim = c(0.4, 1.6))
grid.text("Lasso logistic regression calibration slope", .5, .8, gp=gpar(cex=2))

###Calibration intercept###
forest(metagen(TE = cal_int,
               seTE = se_int,
               studlab = cluster_code,
               data =  calibration_measure_imp,
               null = 0,
               method.random.ci  = "HK",
               method.predict = "HK",
               method.tau = "REML",
               prediction = T),
       ref = 0)
grid.text("Lasso logistic regression calibration intercept", .5, .8, gp=gpar(cex=2))


###C-statistic###
forest(metagen(TE = roc_auc,
               seTE = se_roc_auc,
               studlab = cluster_code,
               data =  c_stat_imp,
               method.random.ci = "HK",
               method.predict = "HK",
               prediction = T),
       xlim = c(0.6, 1))
grid.text("Lasso logistic regression C-statistics", .5, 0.8, gp=gpar(cex=1.5))


###Brier Score###
simpen %>%
  mutate(Died = factor(Died, levels = c("1", "0"))) %>%
  brier_class(Died, .pred_1)


###Smoothed calibration plot###
pmcalibration(y = (simpen %>% mutate(Died = as.numeric(as.character(Died))))$Died, p = simpen$.pred_1, smooth = "loess", ci = "boot", n = 1000) %>% plot()


###Distribution of predicted probability###
ggplot(simpen, aes(x = .pred_1)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100), 
                 bins = 20, fill = "skyblue", color = "black") +
  labs(x = "Predicted Probability", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() 

###Decision curve analysis for all the models in IECV###
#Loading the results for all the models in IECV for decision curve analysis
load("modified_simpen_xgb_baggedtree_neo.Rda", verbose = T)
simpen_comb = simpen
simpen_comb = simpen_comb %>%
  rename(sXGB = .pred_1)

load("modified_simpen_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)
simpen_comb = simpen_comb %>%
  mutate(sLR = simpen$.pred_1)

load("simpen_DT_noimp_neo.Rda", verbose = T)
simpen_comb = simpen_comb %>%
  mutate(DT = simpen$.pred_1)

simpen_comb = simpen_comb %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_SRD` = if_else(Q52SP02P < 80 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<85_OR_SRD` = if_else(Q52SP02P < 85 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<90_OR_SRD` = if_else(Q52SP02P < 90 | Q37RESPI2 == "YES", 1, 0))

#SpO2 only
#SpO2 only.
dca(Died ~ `SpO2<80` + `SpO2<85` + `SpO2<90`, 
    data = simpen_comb %>%
      mutate(Died = factor(Died, levels = c("0", "1"))),
    thresholds = seq(0, 0.4, 0.01)) %>%
  as_tibble() %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  mutate(cat = case_when(
    label %in% c("Treat All", "Treat None") ~ "Default",
    !(label %in% c("Treat All", "Treat None")) ~ "Individual"
  )) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label, linetype = cat)) +
  stat_smooth(method = "loess", 
              se = FALSE, 
              formula = "y ~ x", 
              span = 0.2) +
  coord_cartesian(ylim = c(-0.01, 0.1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "labels") +
  theme_bw() +
  guides(linetype = "none") +
  scale_color_manual(values = c("SpO2<80" = "#030303", 
                                "SpO2<85" = "#888888",
                                "SpO2<90" = "#661100",
                                "Treat All" = "#CC79A7",
                                "Treat None" = "#332288")) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#All models + SpO2 and severe respiratory distress.
dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`,
    data = simpen_comb %>%
      mutate(Died = factor(Died, levels = c("0", "1"))),
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
  coord_cartesian(ylim = c(-0.01, 0.1)) +
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
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")


##################PERFORMING SUBGROUP ANALYSES BASED on SEX##################
load("modified_simpen_xgb_baggedtree_neo.Rda", verbose = T)


load("modified_simpen_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)


load("simpen_DT_noimp_neo.Rda", verbose = T)


#Loop to perform subgroup analyses for performance metrics based on sex.
for (i in c("female", "male")) {
  print(i)
  
  dat = simpen %>%
    filter(Q12SEX == i) %>%
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

#Getting Brier Score
simpen %>%
  filter(Q12SEX == "female") %>%
  brier_class(truth = Died, .pred_1)

simpen %>%
  filter(Q12SEX == "male") %>%
  brier_class(truth = Died, .pred_1)


###Decision curve###
load("modified_simpen_xgb_baggedtree_neo.Rda", verbose = T)
simpen_comb = simpen
simpen_comb = simpen_comb %>%
  rename(sXGB = .pred_1)

load("modified_simpen_lasso_baggedtree_neo_poly_notre.Rda", verbose = T)
simpen_comb = simpen_comb %>%
  mutate(sLR = simpen$.pred_1)

load("simpen_DT_noimp_neo.Rda", verbose = T)
simpen_comb = simpen_comb %>%
  mutate(DT = simpen$.pred_1)

simpen_comb = simpen_comb %>%
  mutate(`SpO2<90` = if_else(Q52SP02P < 90, 1, 0),
         `SpO2<85` = if_else(Q52SP02P < 85, 1, 0),
         `SpO2<80` = if_else(Q52SP02P < 80, 1, 0),
         `SpO2<80_OR_SRD` = if_else(Q52SP02P < 80 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<85_OR_SRD` = if_else(Q52SP02P < 85 | Q37RESPI2 == "YES", 1, 0),
         `SpO2<90_OR_SRD` = if_else(Q52SP02P < 90 | Q37RESPI2 == "YES", 1, 0)) 

#DCA for female subgroup.
fem = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`,
          data = simpen_comb %>%
            mutate(Died = factor(Died, levels = c("0", "1"))) %>%
            filter(Q12SEX == "female"),
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
  coord_cartesian(ylim = c(-0.01, 1)) +
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
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

#DCA for male subgroup
mal = dca(Died ~ sXGB + sLR + DT + `SpO2<80_OR_SRD` + `SpO2<85_OR_SRD` + `SpO2<90_OR_SRD`,
          data = simpen_comb %>%
            mutate(Died = factor(Died, levels = c("0", "1"))) %>%
            filter(Q12SEX == "male"),
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
  coord_cartesian(ylim = c(-0.01, 1)) +
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
    "Default" = "solid"
  )) +
  theme(legend.title = element_blank()) +
  annotate("rect",
           xmin = 0.05, xmax = 0.2,   # x-range to shade
           ymin = -Inf, ymax = Inf,  # entire y-axis
           alpha = 0.2, fill = "lightblue")

