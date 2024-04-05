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
# set seed
set.seed(1234)
##################################################
# load CC data
covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/Key_WW/covid_cdc_cases_03202024_100k_key_ww.xlsx", sheet = 1))
buffer_unique= covid_cases_cdc %>% distinct(county_names, .keep_all = TRUE)
buffer_full = covid_cases_cdc %>% distinct()
# load WW data 
covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/Key_WW/working_data_key_ww.xlsx", sheet = 2)) #_onlyN1
buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
buffer_concen_full = covid_concen %>% distinct()
##################################################
# Get the same county list of WW and CC
virus_counties <- buffer_concen_unique[, -c("pcr_target_flowpop_lin", "sample_collect_date")]
cases_counties <- buffer_unique[, -c("cases_by_cdc_case_earliest_date", "sample_collect_date")]
same_county <- merge(virus_counties, cases_counties, by = "county_names")
###################################################
# 8069 include of 7 "key_sewershed" (co_142, co_150, co_151, co_152, co_227, co_370, co_437)
Mi_case = L_case = Mo_case = H_case = VH_case = 0
Mi_virus = L_virus = Mo_virus = H_virus = VH_virus = 0
lowess_data_lne_quantile_10th = lowess_data_lne_stdev = 0
lowess_data_virus_lne_quantile_10th = lowess_data_virus_lne_stdev = 0
span_const = 0.1
for (z in 1:3) { #length(same_county$county_names)
  # z = 1
  lowess_data_lne_quantile_10th = lowess_data_lne_stdev = 0
  lowess_data_virus_lne_quantile_10th = lowess_data_virus_lne_stdev = 0
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
  ################################################################
  # add Ln_e()
  full_cdc_cases$lowess_data_lne <- log(full_cdc_cases$lowess_data)
  ################################################################
  # check if data is more than 1 year
  if (length(full_cdc_cases$sample_collect_date) > 365){
    # from 1 to 365 day
    lowess_data_lne_quantile_10th[1:365] <- quantile(full_cdc_cases$lowess_data_lne[1:365], probs = c(0.1), na.rm=TRUE) 
    lowess_data_lne_stdev[1:365] <- sd(full_cdc_cases$lowess_data_lne[1:365], na.rm=TRUE)
    #from 366 to end day
    for (t in 366:length(full_cdc_cases$sample_collect_date)) {
      lowess_data_lne_quantile_10th[t] <- quantile(full_cdc_cases$lowess_data_lne[(t-364):t], probs = c(0.1), na.rm=TRUE) 
      # calculate standard deviation
      lowess_data_lne_stdev[t] <- sd(full_cdc_cases$lowess_data_lne[(t-364):t], na.rm=TRUE)
    }
    full_cdc_cases$lowess_data_lne_quantile_10th <- lowess_data_lne_quantile_10th 
    full_cdc_cases$lowess_data_lne_stdev <- lowess_data_lne_stdev
  } else {
    full_cdc_cases$lowess_data_lne_quantile_10th <- quantile(full_cdc_cases$lowess_data_lne[1:length(full_cdc_cases$sample_collect_date)], probs = c(0.1), na.rm = TRUE) 
    full_cdc_cases$lowess_data_lne_stdev <- sd(full_cdc_cases$lowess_data_lne[1:length(full_cdc_cases$sample_collect_date)], na.rm=TRUE)
  }
  ################################################################
  # calculate activity level with cases
  for (t in 1:length(full_cdc_cases$sample_collect_date)) {
    full_cdc_cases$viral_activity_cases[t] <- exp((full_cdc_cases$lowess_data_lne[t] - full_cdc_cases$lowess_data_lne_quantile_10th[t])/full_cdc_cases$lowess_data_lne_stdev[t])
  }
  ################################################################
  # plot CC data
  # plot(full_cdc_cases$sample_collect_date,
  #      full_cdc_cases$viral_activity_cases,
  #      type = "l",
  #      col = 2,
  #      ylim = c(0, 120),
  #      main = "key_sewershed: co_152",
  #      xlab = "Date",
  #      ylab = "Viral Activity")
  ################################################################
  # assign activity level
  full_cdc_cases$viral_activity_cases[is.na(full_cdc_cases$viral_activity_cases)] <- 0
  for (t in 1:length(full_cdc_cases$sample_collect_date)) {
    if (full_cdc_cases$viral_activity_cases[t] == 0) {full_cdc_cases$viral_level_cases[t] <- "Null"}
    else if (full_cdc_cases$viral_activity_cases[t] < 1.5) {full_cdc_cases$viral_level_cases[t] <- "Mi"}
    else if (full_cdc_cases$viral_activity_cases[t] < 3) {full_cdc_cases$viral_level_cases[t] <- "L"}
    else if (full_cdc_cases$viral_activity_cases[t] < 4.5) {full_cdc_cases$viral_level_cases[t] <- "Mo"}
    else if (full_cdc_cases$viral_activity_cases[t] < 8) {full_cdc_cases$viral_level_cases[t] <- "H"}
    else {full_cdc_cases$viral_level_cases[t] <- "VH"}
  }
  # write_xlsx(full_cdc_cases, "~/Documents/GitHub/R_UF/ML/Key_WW/Viral_activity_CC_co_152_span_0_1_all_values.xlsx")
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
  # Create example data
  x <- 1:length(full_ww_virus$sample_collect_date)
  y <- full_ww_virus$pcr_target_flowpop_lin
  x.all <- seq(1, length(full_ww_virus$sample_collect_date), 1)
  # Fit LOESS model
  loess_model <- loess(y ~ x, na.action = na.exclude, span = span_const)
  # Predict missing values
  predicted_values <- predict(loess_model, newdata=x.all)
  # Replace missing values with predicted values
  # y[is.na(y)] <- predicted_values[is.na(y)]
  full_ww_virus$lowess_data_virus <- predicted_values #y
  ################################################################
  # add Ln_e()
  full_ww_virus$lowess_data_virus_lne <- log(full_ww_virus$lowess_data_virus)
  ################################################################
  # check if data is more than 1 year
  if (length(full_ww_virus$sample_collect_date) > 365){
    # from 1 to 365 day
    lowess_data_virus_lne_quantile_10th[1:365] <- quantile(full_ww_virus$lowess_data_virus_lne[1:365], probs = c(0.1), na.rm=TRUE) 
    lowess_data_virus_lne_stdev[1:365] <- sd(full_ww_virus$lowess_data_virus_lne[1:365], na.rm=TRUE)
    #from 366 to end day
    for (t in 366:length(full_ww_virus$sample_collect_date)) {
      lowess_data_virus_lne_quantile_10th[t] <- quantile(full_ww_virus$lowess_data_virus_lne[(t-364):t], probs = c(0.1), na.rm=TRUE) 
      lowess_data_virus_lne_stdev[t] <- sd(full_ww_virus$lowess_data_virus_lne[(t-364):t], na.rm=TRUE)
    }
    full_ww_virus$lowess_data_virus_lne_quantile_10th <- lowess_data_virus_lne_quantile_10th 
    full_ww_virus$lowess_data_virus_lne_stdev <- lowess_data_virus_lne_stdev
  } else {
    full_ww_virus$lowess_data_virus_lne_quantile_10th <- quantile(full_ww_virus$lowess_data_virus_lne[1:length(full_ww_virus$sample_collect_date)], probs = c(0.1), na.rm=TRUE) 
    full_ww_virus$lowess_data_virus_lne_stdev <- sd(full_ww_virus$lowess_data_virus_lne[1:length(full_ww_virus$sample_collect_date)], na.rm=TRUE)
  }
  
  ################################################################
  # calculate activity level with cases
  for (t in 1:length(full_ww_virus$sample_collect_date)) {
    full_ww_virus$viral_activity_virus[t] <- exp((full_ww_virus$lowess_data_virus_lne[t] - full_ww_virus$lowess_data_virus_lne_quantile_10th[t])/full_ww_virus$lowess_data_virus_lne_stdev[t])
  }
  ################################################################
  # Plot WW data
  # plot(full_ww_virus$sample_collect_date,
  #      full_ww_virus$viral_activity_virus,
  #      type = "l",
  #      col = 2,
  #      ylim = c(0, 40),
  #      main = "key_sewershed: co_152",
  #      xlab = "Date",
  #      ylab = "Viral Activity")
  ################################################################
  # assign activity level
  full_ww_virus$viral_activity_virus[is.na(full_ww_virus$viral_activity_virus)] <- 0
  for (t in 1:length(full_ww_virus$sample_collect_date)) {
    if (full_ww_virus$viral_activity_virus[t] == 0) {full_ww_virus$viral_level_virus[t] <- "Null"}
    else if (full_ww_virus$viral_activity_virus[t] < 1.5) {full_ww_virus$viral_level_virus[t] <- "Mi"}
    else if (full_ww_virus$viral_activity_virus[t] < 3) {full_ww_virus$viral_level_virus[t] <- "L"}
    else if (full_ww_virus$viral_activity_virus[t] < 4.5) {full_ww_virus$viral_level_virus[t] <- "Mo"}
    else if (full_ww_virus$viral_activity_virus[t] < 8) {full_ww_virus$viral_level_virus[t] <- "H"}
    else {full_ww_virus$viral_level_virus[t] <- "VH"}
  }
  # write_xlsx(full_ww_virus, "~/Documents/GitHub/R_UF/ML/Key_WW/Viral_activity_WW_co_152_span_0_1_all_values.xlsx")
  # Join WW and CC to get the overlap
  join_data = merge(x = full_cdc_cases, y = full_ww_virus, by = "sample_collect_date")
  # write_xlsx(join_data, "~/Documents/GitHub/R_UF/ML/Key_WW/Viral_activity_join_CC_WW_sewershed_co_152_span_0_1_all_values.xlsx")
  ################################################################
  # save graph of each sewersheds to file
  png(filename=paste("~/Documents/GitHub/R_UF/ML/Key_WW/", same_county$county_names[z],".png"),
      width	= 2050,
      height = 1020, 
      res = 180)
  #jpeg(paste("~/Documents/GitHub/R_UF/ML/Key_WW/", same_county$county_names[z],".jpeg"), quality = 100)
  mylims <- range(with(join_data, c(viral_activity_virus, viral_activity_cases)))
  
  plot(join_data$sample_collect_date,
       join_data$viral_activity_cases,
       type = "l",
       col = 2,
       #xlim = mylims,
       ylim = mylims, # c(0, 120),
       main = paste("key_sewershed: ", same_county$county_names[z]),
       xlab = "Date",
       ylab = "Viral Activity")

  # Add line graphs of other two dataset
  lines(join_data$sample_collect_date,
        join_data$viral_activity_virus,
        type = "l",
        col = 3)
  # Add legend in top right corner
  legend("topright",
         c("CC data", "WW data"),
         lty = 1,
         col = 2:4)
  
  ################################################################
  # export plot of each sewersheds
  dev.off()
  
  ################################################################
  # wait after complete each sewersheds
  # readline(prompt="Press [enter] to continue")
  ################################################################
  
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
