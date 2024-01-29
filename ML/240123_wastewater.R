#########################
# AUTHOR: Won-tak       #
# DATE:   2024-01-23    #
#########################

# Load libraries
# .libPaths("/blue/wjoo/R/4.2.2")
library(data.table)
setDTthreads(0)
library(bit64)
library(readstata13)

# devtools::install_github("MartinSpindler/hdm")
library(hdm)

library(fastDummies)

library(arm)           # coefplot
library(modelsummary)  # model plot
library(dplyr)
library(ggplot2)

library(dplyr)
library (tibble)

options(max.print=999999)

# Paths
# raw_path     <- "/blue/wjoo/project/wastewater/data/raw/"
# data_path     <- "/blue/wjoo/project/wastewater/data/"
# result_path   <- "/blue/wjoo/project/wastewater/result/"

# MacOS path
raw_path     <- "~/Documents/GitHub/R_UF/ML/data/raw/"
data_path     <- "~/Documents/GitHub/R_UF/ML/data/data/"
result_path   <- "~/Documents/GitHub/R_UF/ML/data/result/"

# # Windows path
# raw_path     <- "D:/GitHub/R_UF/ML/data/raw/"
# data_path     <- "D:/GitHub/R_UF/ML/data/data/"
# result_path   <- "D:/GitHub/R_UF/ML/data/result/"

# set seed
set.seed(1234)

########## data

# case: c_usaf c_r_usaf c_r_log_usaf c_nyt c_r_nyt c_r_log_nyt
# mortality:  d_usaf d_r_usaf d_r_log_usaf d_nyt d_r_nyt d_r_log_nyt d_log_r_bw_cov d_log_r_hw_cov
# segregation: seg_theil_all seg_theil_bw seg_theil_hw seg_reldiv_all seg_reldiv_bw seg_reldiv_hw

# cumulative_confirmed_cases_by_100k_pop cumulative_deaths_by_100k_pop
# c_r_log_nyt d_r_log_nyt
# seg_reldiv_all (relative diversity index)

torrats <- as.data.table(read.dta13(paste0(raw_path, "merged-for-analysis-1.dta")))
colnames(torrats) <- tolower(colnames(torrats))
torrats <- torrats[, -c("c_usaf", "c_r_usaf", "c_r_log_usaf", "c_nyt", "c_r_nyt")]
torrats <- torrats[, -c("d_usaf", "d_r_usaf", "d_r_log_usaf", "d_nyt", "d_r_nyt", "d_log_r_bw_cov", "d_log_r_hw_cov")]
torrats <- torrats[, -c("seg_theil_all", "seg_theil_bw", "seg_theil_hw", "seg_reldiv_bw", "seg_reldiv_hw")]

torrats <- dummy_cols(torrats, select_columns=c("state_ab"),
                    remove_first_dummy=TRUE, ignore_na=TRUE, remove_selected_columns=TRUE)
torrats <- torrats[,-c("county_name_state", "state_name", "region_name")]
torrats <- torrats[,fips:=as.numeric(fips)]

covid <- fread(paste0(raw_path, "US_Covid_19_data_by_county_30092020log.csv")) #"US_Covid_19_data_by_county.csv"))
colnames(covid) <- tolower(colnames(covid))
covid <- covid[, c("fips", "cumulative_deaths_by_100k_pop", "cumulative_confirmed_cases_by_100k_pop")]

covid <- merge(torrats, covid, by="fips")

cor(covid$c_r_log_nyt, covid$cumulative_confirmed_cases_by_100k_pop)
cor(covid$d_r_log_nyt, covid$cumulative_deaths_by_100k_pop)

##### analysis 1. deaths per 100k (measured by September 30, 2020) PNAS
covid_a <- copy(covid)
covid_a$outcome_PNAS <- covid_a$d_r_log_nyt
covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")]
model1 <- summary(lm(outcome_PNAS ~ seg_reldiv_all, data = covid_a))

# Get all PNAS parameters of Fig 1

pnas <- grep("dem_", names(covid_a), value=TRUE)
pnas1 <- grep("air_", names(covid_a), value=TRUE)
pnas2 <- grep("cbp_", names(covid_a), value=TRUE)
pnas3 <- grep("rwj_", names(covid_a), value=TRUE)
state <- grep("state_", names(covid_a), value=TRUE)

# Model 2: Linear Regression outcome ~ seg_reldiv_all + pnas_
formula <- paste(c("outcome_PNAS ~ seg_reldiv_all", pnas), collapse = "+")
# , pnas1, pnas2, pnas3
model2 <- summary(lm(formula, data = covid_a))

# # Standardize coefficient data
# df <- model2$coefficients[-c(1:48),]
# df
# dt <- data.table::as.data.table(df, .keep.rownames = "word")
# df2 <- dt %>% mutate_at(c('Estimate'), ~(scale(.) %>% as.vector))
# # df2 <- dt %>% mutate_at(c('Estimate', 'Std. Error', 't value'), ~(scale(.) %>% as.vector))
# # df2 <- dt %>% mutate_all(~(scale(.) %>% as.vector))
# df2
# # # Calculate SD and mean
# # sd(df2$Estimate)
# # mean(df2$Estimate)
# model2$coefficients <- df2

# Coefficient plots
# cm <- c("dem_65over" = "% older 65",
#        'dem_25under' = '% younger than 25')
# coef_map = cm,
modelplot(model2, size=1) + # color="blue",, coef_omit=c(1, 2)
  labs(title="Coefficient plots plots of regression of controls predicting COVID ouctomes and segregation") +
  theme_linedraw() +
  geom_vline(aes(xintercept = 0), color="red") +
  aes(color = ifelse(p.value < 0.001, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "blue"))

#### analysis 2. cumulative deaths deaths per 100k (measured by Sep 2020)
covid_a <- copy(covid)
covid_a$outcome_JH <- covid_a$cumulative_deaths_by_100k_pop
covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")]
model12 <- summary(lm(outcome_JH ~ seg_reldiv_all, data = covid_a))

# Get all PNAS parameters of Fig 1

pnas <- grep("dem_", names(covid_a), value=TRUE)
pnas1 <- grep("air_", names(covid_a), value=TRUE)
pnas2 <- grep("cbp_", names(covid_a), value=TRUE)
pnas3 <- grep("rwj_", names(covid_a), value=TRUE)
state <- grep("state_", names(covid_a), value=TRUE)

# Model 2: Linear Regression outcome ~ seg_reldiv_all + pnas_
formula <- paste(c("outcome_JH ~ seg_reldiv_all", pnas), collapse = "+")
# , pnas1, pnas2, pnas3
# Coefficient plots
model22 <- summary(lm(formula, data = covid_a))
# cm <- c("dem_65over" = "% older 65",
#        'dem_25under' = '% younger than 25')
# coef_map = cm,
modelplot(model22, coef_omit=c(1, 2), size=1) + #color="green",
  labs(title="Coefficient plots plots of regression of controls predicting COVID ouctomes and segregation") +
  theme_linedraw() +
  geom_vline(aes(xintercept = 0), color="red") +
  aes(color = ifelse(p.value < 0.001, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "green"))

#### analysis 3. cumulative deaths deaths per 100k (measured by March 2023)
covid <- fread(paste0(raw_path, "US_Covid_19_data_by_county_log.csv")) #"US_Covid_19_data_by_county.csv"))
colnames(covid) <- tolower(colnames(covid))
covid <- covid[, c("fips", "cumulative_deaths_by_100k_pop", "cumulative_confirmed_cases_by_100k_pop")]

covid <- merge(torrats, covid, by="fips")

cor(covid$c_r_log_nyt, covid$cumulative_confirmed_cases_by_100k_pop)
cor(covid$d_r_log_nyt, covid$cumulative_deaths_by_100k_pop)

covid_a <- copy(covid)
covid_a$outcome_JH_full <- covid_a$cumulative_deaths_by_100k_pop
covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")]
model13 <- summary(lm(outcome_JH_full ~ seg_reldiv_all, data = covid_a))

# Get all PNAS parameters of Fig 1

pnas <- grep("dem_", names(covid_a), value=TRUE)
pnas1 <- grep("air_", names(covid_a), value=TRUE)
pnas2 <- grep("cbp_", names(covid_a), value=TRUE)
pnas3 <- grep("rwj_", names(covid_a), value=TRUE)
state <- grep("state_", names(covid_a), value=TRUE)

# Model 2: Linear Regression outcome ~ seg_reldiv_all + pnas_
formula <- paste(c("outcome_JH_full ~ seg_reldiv_all", pnas), collapse = "+")
# , pnas1, pnas2, pnas3
# Coefficient plots
model23 <- summary(lm(formula, data = covid_a))
# cm <- c("dem_65over" = "% older 65",
#        'dem_25under' = '% younger than 25')
# coef_map = cm,
modelplot(model23, coef_omit=c(1, 2), size=1) + #color="green",
  labs(title="Coefficient plots plots of regression of controls predicting COVID ouctomes and segregation") +
  theme_linedraw() +
  geom_vline(aes(xintercept = 0), color="red") +
  aes(color = ifelse(p.value < 0.001, "Significant", "Not significant")) +
  scale_color_manual(values = c("grey", "orange"))

# Compare 2 model PNAS vs JH (09/30/2020)
models <- list(model2, model22) # Create list of models to compare
models <- dvnames(models)       # dvnames(): rename our list with names matching the dependent variable in each model
modelplot(models, draw = FALSE) # show the raw data used to draw the plot
modelplot(models, coef_omit=c(1, 2), color = "darkblue") + facet_grid(~model) # display models side by side

# Compare 2 model JH 09/30/2020 vs JH 03/09/2023
models <- list(model22, model23) # Create list of models to compare
models <- dvnames(models)       # dvnames(): rename our list with names matching the dependent variable in each model
modelplot(models, draw = FALSE) # show the raw data used to draw the plot
modelplot(models, coef_omit=c(1, 2), color = "darkblue") + facet_grid(~model) # display models side by side

# Compare 3 model PNAS vs JH (09/30/2020) vs JH (03/09/2023)
models <- list(model2, model22, model23) # Create list of models to compare
models <- dvnames(models)       # dvnames(): rename our list with names matching the dependent variable in each model
modelplot(models, draw = FALSE) # show the raw data used to draw the plot
modelplot(models, coef_omit=c(1, 2), color = "darkblue") + facet_grid(~model) # display models side by side

##### analysis 1. deaths per 100k (measured by September 30, 2020)
# covid_a <- copy(covid)
# covid_a$outcome <- covid_a$d_r_log_nyt
# covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")]
# covid_a <- covid_a[, -c("state_ab")]

# # Model 1: Linear Regression outcome ~ seg_reldiv_all
# # model1 <- summary(lm(outcome ~ seg_reldiv_all, data = covid_a))
# # model1 <- summary(lm(outcome ~ ., data = covid_a))
# 
# #Plot data
# plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
# abline(model1, col = "darkgreen") #Add a regression line
# plot(model1$residuals, pch = 16, col = "red") #Plot the residuals
# abline(model1, col = "darkgreen") #Add a regression line
# 
# 
# # Get all PNAS parameters of Fig 1
# pnas <- grep("dem_", names(covid_a), value=TRUE)
# pnas1 <- grep("air_", names(covid_a), value=TRUE)
# pnas2 <- grep("cbp_", names(covid_a), value=TRUE)
# pnas3 <- grep("rwj_", names(covid_a), value=TRUE)
# 
# # Model 2: Linear Regression outcome ~ seg_reldiv_all + state_
# # state <- grep("state_", names(covid_a), value=TRUE)
# # formula <- paste(c("outcome ~ seg_reldiv_all", state), collapse = "+")
# formula <- paste(c("outcome ~ seg_reldiv_all", pnas), collapse = "+")
# # , pnas1, pnas2, pnas3
# # Coefficient plots 
# model2 <- summary(lm(formula, data = covid_a))
# modelplot(model2, coef_omit=c(1, 2), color="blue", size=1) + labs(title="Coefficient plots plots of regression of controls predicting COVID ouctomes and segregation")
# 
# 
# # model 3
# lasso <- rlasso(outcome ~ ., data = covid_a, post = FALSE)
# selected <- which(coef(lasso)[-c(1:1)]!=0)
# formula <- paste(c("outcome ~ seg_reldiv_all", names(selected)), collapse = "+")
# model3 <- summary(lm(formula, data = covid_a))
# 
# state <- grep("state_", names(covid_a), value=TRUE)
# control <- paste0("~", paste(state, collapse = "+"))
# model4 <- rlassoEffects(outcome ~ ., data = covid_a, post = FALSE, method = "double selection", I = ~ seg_reldiv_all, included = control)
# 
# 
# #######################################################
# ##### analysis 2. cumulative deaths deaths per 100k (measured by March 2023)
# covid_a <- copy(covid)
# covid_a$outcome <- covid_a$cumulative_deaths_by_100k_pop
# covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")] 
# 
# model1 <- summary(lm(outcome ~ seg_reldiv_all, data = covid_a)) #+ sqrt(seg_reldiv_all)
# # I(seg_reldiv_all^2) + I(seg_reldiv_all^3)
# 
# #Plot data
# plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
# abline(model1, col = "darkgreen") #Add a regression line
# plot(model1$residuals, pch = 16, col = "red") #Plot the residuals
# abline(model1, col = "darkgreen") #Add a regression line
# 
# 
# # Get all PNAS parameters of Fig 1
# 
# pnas <- grep("dem_", names(covid_a), value=TRUE)
# pnas1 <- grep("air_", names(covid_a), value=TRUE)
# pnas2 <- grep("cbp_", names(covid_a), value=TRUE)
# pnas3 <- grep("rwj_", names(covid_a), value=TRUE)
# 
# # Model 2: Linear Regression outcome ~ seg_reldiv_all + state_
# # state <- grep("state_", names(covid_a), value=TRUE)
# # formula <- paste(c("outcome ~ seg_reldiv_all", state), collapse = "+")
# formula <- paste(c("outcome ~ seg_reldiv_all", pnas), collapse = "+")
# # , pnas1, pnas2, pnas3
# # Coefficient plots 
# model2 <- summary(lm(formula, data = covid_a))
# # cm <- c("dem_65over" = "% older 65",
# #        'dem_25under' = '% younger than 25')
# # coef_map = cm,
# modelplot(model2, coef_omit=c(1, 2), color="darkgreen", size=1) + 
#   labs(title="Coefficient plots plots of regression of controls predicting COVID ouctomes and segregation") +
#   theme_linedraw() +
#   geom_vline(aes(xintercept = 0), color="red")
# # theme_classic() theme_minimal() theme_linedraw()
# # arm::coefplot(model2, col.pts="red", cex.pts=1.5)
# # plot()
# # abline(V = 0, col = "darkgreen")
# 
# # state <- grep("state_", names(covid_a), value=TRUE)
# # formula <- paste(c("outcome ~ seg_reldiv_all", state), collapse = "+")
# # model2 <- summary(lm(formula, data = covid_a))
# 
# 
# # model 3
# lasso <- rlasso(outcome ~ ., data = covid_a, post = FALSE)
# selected <- which(coef(lasso)[-c(1:1)]!=0)
# formula <- paste(c("outcome ~ seg_reldiv_all", names(selected)), collapse = "+")
# model3 <- summary(lm(formula, data = covid_a))
# 
# state <- grep("state_", names(covid_a), value=TRUE)
# control <- paste0("~", paste(state, collapse = "+"))
# model4 <- rlassoEffects(outcome ~ ., data = covid_a, post = FALSE, method = "double selection", I = ~ seg_reldiv_all, included = control)
# 
# ### glmet works only with complete cases
# #outcome <- covid_a$outcome
# #control <- covid_a[, -"outcome"]
# #state <- !grepl("state_", names(control))
# #model5 <- glmnet(control, outcome, family = "gaussian", alpha = 1, penalty.factor = state)
# #coef(model5, s = c("lambda.min", "lambda.1se"))

########## clear
# rm(list = ls())

