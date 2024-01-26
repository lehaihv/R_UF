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

options(max.print=999999)

# Paths
# raw_path     <- "/blue/wjoo/project/wastewater/data/raw/"
# data_path     <- "/blue/wjoo/project/wastewater/data/"
# result_path   <- "/blue/wjoo/project/wastewater/result/"

raw_path     <- "~/Documents/GitHub/R_UF/ML/data/raw/"
data_path     <- "~/Documents/GitHub/R_UF/ML/data/data/"
result_path   <- "~/Documents/GitHub/R_UF/ML/data/result/"

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

covid <- fread(paste0(raw_path, "US_Covid_19_data_by_county.csv"))
colnames(covid) <- tolower(colnames(covid))
covid <- covid[, c("fips", "cumulative_deaths_by_100k_pop", "cumulative_confirmed_cases_by_100k_pop")]

covid <- merge(torrats, covid, by="fips")

cor(covid$c_r_log_nyt, covid$cumulative_confirmed_cases_by_100k_pop)
cor(covid$d_r_log_nyt, covid$cumulative_deaths_by_100k_pop)


##### analysis 1. deaths per 100k (measured by September 30, 2020)

covid_a <- copy(covid)
covid_a$outcome <- covid_a$d_r_log_nyt
covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")] 

# Model 1: Linear Regression outcome ~ seg_reldiv_all
model1 <- summary(lm(outcome ~ seg_reldiv_all, data = covid_a))

#Plot data
plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
abline(model1, col = "darkgreen") #Add a regression line
# plot(model1$residuals, covid_a$outcome, pch = 16, col = "red") #Plot the residuals
# abline(model1, col = "green") #Add a regression line
plot(model1$residuals, pch = 16, col = "red") #Plot the residuals
abline(model1, col = "darkgreen") #Add a regression line

# Model 2: Linear Regression outcome ~ seg_reldiv_all + state_
state <- grep("state_", names(covid_a), value=TRUE)
formula <- paste(c("outcome ~ seg_reldiv_all", state), collapse = "+")
model2 <- summary(lm(formula, data = covid_a))

#Plot data
# plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
# abline(model2, col = "green") #Add a regression line
# plot(model2$residuals, covid_a$outcome, pch = 16, col = "red") #Plot the residuals
# abline(model2, col = "green") #Add a regression line

lasso <- rlasso(outcome ~ ., data = covid_a, post = FALSE)
selected <- which(coef(lasso)[-c(1:1)]!=0)
formula <- paste(c("outcome ~ seg_reldiv_all", names(selected)), collapse = "+")
model3 <- summary(lm(formula, data = covid_a))

#Plot data
plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
abline(model3, col = "green") #Add a regression line
plot(model3$residuals, pch = 16, col = "red") #Plot the residuals
abline(model3, col = "green") #Add a regression line

state <- grep("state_", names(covid_a), value=TRUE)
control <- paste0("~", paste(state, collapse = "+"))
model4 <- rlassoEffects(outcome ~ ., data = covid_a, post = FALSE, method = "double selection", I = ~ seg_reldiv_all, included = control)

#Plot data
plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
abline(model4, col = "green") #Add a regression line
plot(model4$residuals$e, model4$residuals$v, pch = 16, col = "red") #Plot the residuals
abline(model4, col = "green") #Add a regression line

##### analysis 2. cumulative deaths deaths per 100k (measured by March 2023)

covid_a <- copy(covid)
covid_a$outcome <- covid_a$cumulative_deaths_by_100k_pop
covid_a <- covid_a[, -c("fips", "cumulative_confirmed_cases_by_100k_pop", "cumulative_deaths_by_100k_pop", "c_r_log_nyt", "d_r_log_nyt")] 

model1 <- summary(lm(outcome ~ seg_reldiv_all, data = covid_a))

#Plot data
plot(covid_a$seg_reldiv_all, covid_a$outcome, pch = 16, col = "blue") #Plot the results
abline(model1, col = "green") #Add a regression line
plot(model1$residuals, covid_a$outcome, pch = 16, col = "red") #Plot the residuals
abline(model1, col = "green") #Add a regression line


state <- grep("state_", names(covid_a), value=TRUE)
formula <- paste(c("outcome ~ seg_reldiv_all", state), collapse = "+")
model2 <- summary(lm(formula, data = covid_a))

lasso <- rlasso(outcome ~ ., data = covid_a, post = FALSE)
selected <- which(coef(lasso)[-c(1:1)]!=0)
formula <- paste(c("outcome ~ seg_reldiv_all", names(selected)), collapse = "+")
model3 <- summary(lm(formula, data = covid_a))

state <- grep("state_", names(covid_a), value=TRUE)
control <- paste0("~", paste(state, collapse = "+"))
model4 <- rlassoEffects(outcome ~ ., data = covid_a, post = FALSE, method = "double selection", I = ~ seg_reldiv_all, included = control)

### glmet works only with complete cases
#outcome <- covid_a$outcome
#control <- covid_a[, -"outcome"]
#state <- !grepl("state_", names(control))
#model5 <- glmnet(control, outcome, family = "gaussian", alpha = 1, penalty.factor = state)
#coef(model5, s = c("lambda.min", "lambda.1se"))

########## clear
rm(list = ls())

