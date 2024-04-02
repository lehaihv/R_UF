#########################
# AUTHOR: Hai LE        #
# DATE:   2024-03-09    #
#########################

# Load libraries
library(data.table)
setDTthreads(0)
library(bit64)
# library(readstata13)

# devtools::install_github("MartinSpindler/hdm")
# library(hdm)

# library(fastDummies)

#library(arm)           # coefplot
#library(modelsummary)  # model plot
library(dplyr)
library(ggplot2)

# library(dplyr)
# library (tibble)

library(readxl)
library(writexl)
library(zoo)
# options(max.print=999999)

# MacOS path
# raw_path     <- "~/Documents/GitHub/R_UF/ML/WWScan/"
# data_path     <- "~/Documents/GitHub/R_UF/ML/WWScan/"
# result_path   <- "~/Documents/GitHub/R_UF/ML/WWScan/"

# set seed
set.seed(1234)

##################################################
##################################################
# loading data cdc covid cases
# 1. Absolute cases 226 counties
# covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases_03202024.xlsx", sheet = 1))
# covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed", "population_served")]

# 2. Cases per 100k 226 counties
covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/Key_WW/covid_cdc_cases_03202024_100k_key_ww.xlsx", sheet = 1))
#covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed", "population_served")]

# # 3. Absolute cases 812 counties
# covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases_03202024.xlsx", sheet = 1))
# covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed", "population_served")]
##################################################
##################################################

# quantile_cdc_cases_66 = 0
# quantile_cdc_cases_33 = 0
buffer_unique= covid_cases_cdc %>% distinct(county_names, .keep_all = TRUE)
buffer_full = covid_cases_cdc %>% distinct()

##################################################
##################################################
# loading data virus concentration for only 226 counties (same counites with CDC_cases)
covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/Key_WW/working_data_key_ww.xlsx", sheet = 2)) #_onlyN1
buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
buffer_concen_full = covid_concen %>% distinct()
# quantile_wastewater_concen_33 = 0
# quantile_wastewater_concen_66 = 0
# concen_range = 226 #815 226
# write_xlsx(buffer_concen_unique, "~/Documents/GitHub/R_UF/ML/JH_CC/County_ww.xlsx")
##################################################
# Get the same county list of WW and CC
virus_counties <- buffer_concen_unique[, -c("pcr_target_flowpop_lin", "sample_collect_date")]
cases_counties <- buffer_unique[, -c("cases_by_cdc_case_earliest_date", "sample_collect_date")]
same_county <- merge(virus_counties, cases_counties, by = "county_names")
#length(same_county$county_names)
##################################################
###################################################
# extract data CDC_covid_case of county 6001
# county_no = 6001
# H_days = 0
# M_days = 0
# L_days = 0
# Risk_level_county_name = 0
# 
# H_days_virus = 0
# M_days_virus = 0
# L_days_virus = 0
# Risk_level_county_name_virus = 0
# 
# H_case <- 0
# M_case <- 0
# L_case <- 0
# H_virus <- 0
# M_virus <- 0
# L_virus <- 0
# 
# high_percentile_range = 0.5
# low_percentile_range = 0.33
# window_size = 28
# slope_high = 1.1
# slope_low = 0.8
Mi_case = L_case = Mo_case = H_case = VH_case = 0
Mi_virus = L_virus = Mo_virus = H_virus = VH_virus = 0
span_const = 0.1
for (z in 1:length(same_county$county_names)) { # 226length(same_county$county_names)
  z = 3 
  covid_county <- buffer_full[county_names == same_county$county_names[z]] 
  # Arranging cases according to the dates
  covid_county.cases_by_cdc_case_earliest_date <- arrange(covid_county, sample_collect_date)
  covid_county <- covid_county.cases_by_cdc_case_earliest_date
  covid_county <- covid_county[, -c("county_names")]
  full_date <- seq(from = as.Date(covid_county$sample_collect_date[1]), 
                   to = as.Date(covid_county$sample_collect_date[length(covid_county$sample_collect_date)]), by = 'day') # V
  # create data frame
  sample_collect_date <- full_date
  full_dates <- data.frame(sample_collect_date)
  # get full dates with cases, NA will be assigned 0
  full_cdc_cases = merge(x = full_dates, y = covid_county, by = "sample_collect_date", all = TRUE)
  ################################################################
  # temp_data <- data.frame(
  #   day = seq(1, length(full_cdc_cases$sample_collect_date)),
  #   daily_cases = full_cdc_cases$cases_by_cdc_case_earliest_date
  # )
  # temp_data$day <- as.numeric(temp_data$day)
  # temp_data$daily_cases <- na.aggregate(temp_data$daily_cases)
  # temp_lowess_CDC_cases = loess(daily_cases ~ day, data = temp_data)
  # temp_data$daily_cases <- predict(temp_lowess_CDC_cases, newdata = NULL)
  # # add lowess data back
  # full_cdc_cases$lowess_data <- temp_data$daily_cases
  ################################################################
  ################################################################
  # Create example data
  x <- 1:length(full_cdc_cases$sample_collect_date)
  y <- full_cdc_cases$cases_by_cdc_case_earliest_date
  x.all <- seq(1, length(full_cdc_cases$sample_collect_date), 1)
  # Fit LOESS model
  loess_model <- loess(y ~ x, na.action = na.exclude, span = span_const)
  
  # Predict missing values
  predicted_values <- predict(loess_model, newdata=x.all)
  
  # Replace missing values with predicted values
  #y[is.na(y)] <- predicted_values[is.na(y)]
  
  full_cdc_cases$lowess_data <- predicted_values #y
  
  # View the updated data
  #print(y)
  
  ################################################################
  
  # add Ln_e()
  full_cdc_cases$lowess_data_lne <- log(full_cdc_cases$lowess_data)
  # get 1 year period to calculate 10th percentile
  first_day <- full_cdc_cases$sample_collect_date[1]
  last_day <- first_day + 365
  temp <- which(grepl(last_day, full_cdc_cases$sample_collect_date))
  # in case less than 365 days
  if (length(temp) < 1) {temp[1] = length(full_cdc_cases$sample_collect_date)}
  full_cdc_cases$lowess_data_lne_quantile_10th <- quantile(full_cdc_cases$lowess_data_lne[1:temp[1]], probs = c(0.1), na.rm = TRUE) 
  # calculate standard deviation
  full_cdc_cases$lowess_data_lne_stdev <- sd(full_cdc_cases$lowess_data_lne[1:temp[1]], na.rm=TRUE)
  # calculate activity level with cases
  for (t in 1:length(full_cdc_cases$sample_collect_date)) {
    full_cdc_cases$viral_activity_cases[t] <- exp((full_cdc_cases$lowess_data_lne[t] - full_cdc_cases$lowess_data_lne_quantile_10th[t])/full_cdc_cases$lowess_data_lne_stdev[t])
  }
  # assign activity level
  full_cdc_cases[is.na(full_cdc_cases)] <- 0
  for (t in 1:length(full_cdc_cases$sample_collect_date)) {
    if (full_cdc_cases$viral_activity_cases[t] == 0) {full_cdc_cases$viral_level_cases[t] <- "Null"}
    else if (full_cdc_cases$viral_activity_cases[t] < 1.5) {full_cdc_cases$viral_level_cases[t] <- "Mi"}
    else if (full_cdc_cases$viral_activity_cases[t] < 3) {full_cdc_cases$viral_level_cases[t] <- "L"}
    else if (full_cdc_cases$viral_activity_cases[t] < 4.5) {full_cdc_cases$viral_level_cases[t] <- "Mo"}
    else if (full_cdc_cases$viral_activity_cases[t] < 8) {full_cdc_cases$viral_level_cases[t] <- "H"}
    else {full_cdc_cases$viral_level_cases[t] <- "VH"}
  }
  # write_xlsx(count_days, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_CDC_cases_226_counties.xlsx")
  
  ###############################################
  # viral concentration
  virus_county <- buffer_concen_full[county_names == same_county$county_names[z]] # buffer_unique
  virus_county.pcr_target_flowpop_lin <- arrange(virus_county, sample_collect_date)
  virus_county <- virus_county.pcr_target_flowpop_lin
  virus_county <- virus_county[, -c("county_names")]
  full_date <- seq(from = as.Date(virus_county$sample_collect_date[1]), 
                   to = as.Date(virus_county$sample_collect_date[length(virus_county$sample_collect_date)]), by = 'day') # V
  # create data frame
  sample_collect_date <- full_date
  full_dates <- data.frame(sample_collect_date)
  # get full dates with cases, NA will be assigned 0
  full_ww_virus = merge(x = full_dates, y = virus_county, by = "sample_collect_date", all = TRUE)
  ################################################################
  # temp_data_virus <- data.frame(
  #   days = seq(1, length(full_ww_virus$sample_collect_date)),
  #   virus_concen = full_ww_virus$pcr_target_flowpop_lin
  # )
  # temp_data_virus$days <- as.numeric(temp_data_virus$days)
  # temp_data_virus$virus_concen <- na.aggregate(temp_data_virus$virus_concen)
  # temp_lowess_ww_virus = loess(virus_concen ~ days, data = temp_data_virus)
  # temp_data_virus$virus_concen <- predict(temp_lowess_ww_virus, newdata = NULL)
  # # add lowess data back
  # full_ww_virus$lowess_data_virus <- temp_data_virus$virus_concen
  ################################################################
  ################################################################
  # Create example data
  x <- 1:length(full_ww_virus$sample_collect_date)
  y <- full_ww_virus$pcr_target_flowpop_lin
  x.all <- seq(1, length(full_ww_virus$sample_collect_date), 1)
  # Fit LOESS model
  loess_model <- loess(y ~ x, na.action = na.exclude, span = span_const)
  
  # Predict missing values
  predicted_values <- predict(loess_model, newdata=x.all)
  
  # Replace missing values with predicted values
  #y[is.na(y)] <- predicted_values[is.na(y)]
  full_ww_virus$lowess_data_virus <- predicted_values #y
  
  # View the updated data
  #print(y)
  
  ################################################################
  
  # add Ln_e()
  full_ww_virus$lowess_data_virus_lne <- log(full_ww_virus$lowess_data_virus)
  # get 1 year period to calculate 10th percentile
  first_day <- full_ww_virus$sample_collect_date[1]
  last_day <- first_day + 365
  temp <- which(grepl(last_day, full_ww_virus$sample_collect_date))
  # in case less than 365 days
  if (length(temp) < 1) {temp[1] = length(full_ww_virus$sample_collect_date)}
  full_ww_virus$lowess_data_virus_lne_quantile_10th <- quantile(full_ww_virus$lowess_data_virus_lne[1:temp[1]], probs = c(0.1), na.rm=TRUE) 
  # calculate standard deviation
  full_ww_virus$lowess_data_virus_lne_stdev <- sd(full_ww_virus$lowess_data_virus_lne[1:temp[1]], na.rm=TRUE)
  # calculate activity level with cases
  for (t in 1:length(full_ww_virus$sample_collect_date)) {
    full_ww_virus$viral_activity_virus[t] <- exp((full_ww_virus$lowess_data_virus_lne[t] - full_ww_virus$lowess_data_virus_lne_quantile_10th[t])/full_ww_virus$lowess_data_virus_lne_stdev[t])
  }
  # assign activity level
  full_ww_virus[is.na(full_ww_virus)] <- 0
  for (t in 1:length(full_ww_virus$sample_collect_date)) {
    if (full_ww_virus$viral_activity_virus[t] == 0) {full_ww_virus$viral_level_virus[t] <- "Null"}
    else if (full_ww_virus$viral_activity_virus[t] < 1.5) {full_ww_virus$viral_level_virus[t] <- "Mi"}
    else if (full_ww_virus$viral_activity_virus[t] < 3) {full_ww_virus$viral_level_virus[t] <- "L"}
    else if (full_ww_virus$viral_activity_virus[t] < 4.5) {full_ww_virus$viral_level_virus[t] <- "Mo"}
    else if (full_ww_virus$viral_activity_virus[t] < 8) {full_ww_virus$viral_level_virus[t] <- "H"}
    else {full_ww_virus$viral_level_virus[t] <- "VH"}
  }
  
  #abline(full_ww_virus$sample_collect_date, full_ww_virus$viral_activity_virus)
  # write_xlsx(count_days, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_CDC_cases_226_counties.xlsx")
  # full_cdc_cases <- subset(full_cdc_cases, select = -c(cases_by_cdc_case_earliest_date, lowess_data, lowess_data_lne, lowess_data_lne_quantile_10th, 
  #                                                      lowess_data_lne_stdev, viral_activity_cases))
  # full_ww_virus <- subset(full_ww_virus, select = -c(pcr_target_flowpop_lin, lowess_data_virus, lowess_data_virus_lne, lowess_data_virus_lne_quantile_10th, 
  #                                                    lowess_data_virus_lne_stdev, viral_activity_virus))
  # # Join WW and CC to get the overlap
  # join_data <- 0
  join_data = merge(x = full_cdc_cases, y = full_ww_virus, by = "sample_collect_date")
  # write_xlsx(join_data, "~/Documents/GitHub/R_UF/ML/Key_WW/Viral_activity_join_CC_WW_sewershed__span_0_1_all_values.xlsx")
  plot(join_data$sample_collect_date,
       join_data$viral_activity_cases,
       type = "l",
       col = 2,
       ylim = c(0, 30),
       main = "key_sewershed: ca_158",
       xlab = "Date",
       ylab = "Viral Activity")

  # Add line graphs of other two dataset
  lines(join_data$sample_collect_date,
        join_data$viral_activity_virus,
        type = "l",
        col = 3)
  # Add legend in top right corner
  legend("topright",
         c("CDC cases", "WW water"),
         lty = 1,
         col = 2:4)
  
  # join_data <- filter(join_data, categories != 0, categories_virus != 0)
  ###
  # write_xlsx(full_cdc_cases, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_cases_8069.xlsx")
  # write_xlsx(full_ww_virus, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_virus_concentration_8069.xlsx")
  # write_xlsx(join_data, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_join_CC_WW_8069.xlsx")
  ###
  # length(join_data$sample_collect_date)
  Mi_case[z] = L_case[z] = Mo_case[z] = H_case[z] = VH_case[z] = 0
  Mi_virus[z] = L_virus[z] = Mo_virus[z] = H_virus[z] = VH_virus[z] = 0
  
  if (length(join_data$sample_collect_date)>0) {
    for (t in 1:length(join_data$sample_collect_date)) {
      if(join_data$viral_level_cases[t] == "Mi") { 
        Mi_case[z] = Mi_case[z] + 1 
      } else if(join_data$viral_level_cases[t] == "L"){ 
        L_case[z] = L_case[z] + 1
      } else if(join_data$viral_level_cases[t] == "Mo"){ 
        Mo_case[z] = Mo_case[z] + 1
      } else if(join_data$viral_level_cases[t] == "H"){ 
        H_case[z] = H_case[z] + 1
      }
      else if(join_data$viral_level_cases[t] == "VH"){
        VH_case[z] = VH_case[z] + 1
      }
    }
    for (t in 1:length(join_data$sample_collect_date)) {
      if(join_data$viral_level_virus[t] == "Mi") { 
        Mi_virus[z] = Mi_virus[z] + 1 
      } else if(join_data$viral_level_virus[t] == "L"){ 
        L_virus[z] = L_virus[z] + 1
      } else if(join_data$viral_level_virus[t] == "Mo"){ 
        Mo_virus[z] = Mo_virus[z] + 1
      } else if(join_data$viral_level_virus[t] == "H"){ 
        H_virus[z] = H_virus[z] + 1
      }
      else if(join_data$viral_level_virus[t] == "VH"){
        VH_virus[z] = VH_virus[z] + 1
      }
    }
  }
}



# Plot data
df_mul <- data.frame(key_sewershed = same_county$county_names,
                     No_of_days_VHigh_Virus_concen = VH_virus,
                     No_of_days_High_Virus_concen = H_virus,
                     No_of_days_Mod_Virus_concen = Mo_virus,
                     No_of_days_Low_Virus_concen = L_virus,
                     No_of_days_Mi_Virus_concen = Mi_virus,
                     No_of_days_VHigh_Covid_case = VH_case,
                     No_of_days_High_Covid_case = H_case,
                     No_of_days_Mod_Covid_case = Mo_case,
                     No_of_days_Low_Covid_case = L_case,
                     No_of_days_Mi_Covid_case = Mi_case)


# write data to excel file
#write_xlsx(df_mul, "~/Documents/GitHub/R_UF/ML/WWScan/Viral_activity_overlap_time_221_counties_LOESS.xlsx")

# Plot VHigh category
plot(df_mul$No_of_days_VHigh_Virus_concen, df_mul$No_of_days_VHigh_Covid_case,
     main="Number of days in High Risk",
     xlab="Virus concentration", ylab="CDC covid cases",
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(df_mul$No_of_days_VHigh_Virus_concen ~ df_mul$No_of_days_VHigh_Covid_case), col = "orange", lwd = 3)
model1 <- summary(lm(df_mul$No_of_days_VHigh_Virus_concen ~ df_mul$No_of_days_VHigh_Covid_case, data = df_mul))
model1

# Plot High category
plot(df_mul$No_of_days_High_Virus_concen, df_mul$No_of_days_High_Covid_case,
     main="Number of days in High Risk",
     xlab="Virus concentration", ylab="CDC covid cases",
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(df_mul$No_of_days_High_Virus_concen ~ df_mul$No_of_days_High_Covid_case), col = "orange", lwd = 3)
model11 <- summary(lm(df_mul$No_of_days_High_Virus_concen ~ df_mul$No_of_days_High_Covid_case, data = df_mul))
model11

# Plot Moderate category
plot(df_mul$No_of_days_Mod_Virus_concen, df_mul$No_of_days_Mod_Covid_case,
     main="Number of days in Medium Risk",
     xlab="Virus concentration", ylab="CDC covid cases",
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(df_mul$No_of_days_Mod_Virus_concen ~ df_mul$No_of_days_Mod_Covid_case), col = "orange", lwd = 3)
model2 <- summary(lm(df_mul$No_of_days_Mod_Virus_concen ~ df_mul$No_of_days_Mod_Covid_case, data = df_mul))
model2

# Plot Low category
plot(df_mul$No_of_days_Low_Virus_concen, df_mul$No_of_days_Low_Covid_case,
     main="Number of days in Low Risk",
     xlab="Virus concentration", ylab="CDC covid cases",
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(df_mul$No_of_days_Low_Virus_concen ~ df_mul$No_of_days_Low_Covid_case), col = "orange", lwd = 3)
model3 <- summary(lm(df_mul$No_of_days_Low_Virus_concen ~ df_mul$No_of_days_Low_Covid_case, data = df_mul))
model3

# Plot High + Medium category
H_M_virus = df_mul$No_of_days_High_Virus_concen + df_mul$No_of_days_VHigh_Virus_concen
H_M_cases = df_mul$No_of_days_High_Covid_case + df_mul$No_of_days_VHigh_Covid_case
plot(H_M_virus, H_M_cases,
     main="Number of days in High and Medium Risk",
     xlab="Virus concentration", ylab="CDC covid cases",
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(H_M_virus ~ H_M_cases, data = df_mul), col = "orange", lwd = 3)
model4 <- summary(lm(H_M_virus ~ H_M_cases, data = df_mul))
# plot(model4$residuals, pch = 16, col = "red")
model4
# 
# ###############################################
