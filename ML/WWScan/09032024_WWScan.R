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

library(fastDummies)

#library(arm)           # coefplot
#library(modelsummary)  # model plot
library(dplyr)
library(ggplot2)

# library(dplyr)
# library (tibble)

library(readxl)
library(writexl)

options(max.print=999999)

# MacOS path
# raw_path     <- "~/Documents/GitHub/R_UF/ML/WWScan/"
# data_path     <- "~/Documents/GitHub/R_UF/ML/WWScan/"
# result_path   <- "~/Documents/GitHub/R_UF/ML/WWScan/"

# set seed
set.seed(1234)

##################################################
##################################################
# # load data covid cases
# covid_key_sewershed <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 1))
# # covid_key_sewershed <- covid_key_sewershed[, -c("population_served")]
# covid_cases <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 3))
# 
# #covid_key_sewershed = covid_key_sewershed %>% distinct(county_names, .keep_all = TRUE)
# 
# 
# covid_cases_cdc <- inner_join(covid_key_sewershed %>% distinct(), covid_cases, by = "key_sewershed") #"county_names")
# # covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed")]
# write_xlsx(covid_cases_cdc, "~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases_03202024.xlsx")
# 
# # Cases per 100k
# covid_cases_cdc_100k <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases_per100k.xlsx", sheet = 1))
# #covid_cases_cdc <- covid_cases_cdc_100k[, -c("population_served")]
# # need remove duplicate sample_collect_date
# 
# full_cdc_cases <- cbind(covid_cases_cdc, covid_cases_cdc_100k$cases_by_cdc_case_earliest_date)


##################################################
##################################################
# load data covid cases
# covid_key_sewershed <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 1))
# covid_key_sewershed <- covid_key_sewershed[, -c("population_served")]
# covid_cases <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 3))
# 
# covid_cases_cdc <- inner_join(covid_key_sewershed %>% distinct(), covid_cases, by = "key_sewershed") #"county_names")


##################################################
##################################################
# loading data cdc covid cases
# 1. Absolute cases
covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases_03202024.xlsx", sheet = 1))
covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed", "population_served")]

# 2. Cases per 100k
covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases_03202024_100k.xlsx", sheet = 1))
covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed", "population_served")]

##################################################
##################################################
quantile_cdc_cases_66 = 0
quantile_cdc_cases_33 = 0
buffer_unique= covid_cases_cdc %>% distinct(county_names, .keep_all = TRUE)
buffer_full = covid_cases_cdc %>% distinct()

##################################################
##################################################
# loading data virus concentration for only 226 counties (same counites with CDC_cases)
covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 2))
buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
buffer_concen_full = covid_concen %>% distinct()
# quantile_wastewater_concen_33 = 0
# quantile_wastewater_concen_66 = 0
concen_range = 226 #815 226

###################################################
# extract data CDC_covid_case of county 6001
# county_no = 6001
H_days = 0
M_days = 0
L_days = 0
Risk_level_county_name = 0

H_days_virus = 0
M_days_virus = 0
L_days_virus = 0
Risk_level_county_name_virus = 0

H_case <- 0
M_case <- 0
L_case <- 0
H_virus <- 0
M_virus <- 0
L_virus <- 0

for (z in 1:226) {
  #z = 26
  covid_county <- buffer_full[county_names == buffer_unique$county_names[z]] #1
  quantile_cdc_cases_33 = quantile(covid_county$cases_by_cdc_case_earliest_date, probs = c(0.33))
  quantile_cdc_cases_66 = quantile(covid_county$cases_by_cdc_case_earliest_date, probs = c(0.66))
  # write_xlsx(covid_county_6001, "~/Documents/GitHub/R_UF/ML/WWScan/CDC_Covid_cases_county_6001.xlsx")
  # Arranging name according to the age
  covid_county.cases_by_cdc_case_earliest_date <- arrange(covid_county, sample_collect_date)
  covid_county <- covid_county.cases_by_cdc_case_earliest_date
  covid_county <- covid_county[, -c("county_names")]
  length(covid_county$sample_collect_date)
  full_date <- seq(from = as.Date(covid_county$sample_collect_date[1]), 
                   to = as.Date(covid_county$sample_collect_date[length(covid_county$sample_collect_date)]), by = 'day') # V
  # create data frame
  sample_collect_date <- full_date
  full_dates <- data.frame(sample_collect_date)
  
  # get full dates with cases, NA will be assigned 0
  full_cdc_cases = merge(x = full_dates, y = covid_county, by = "sample_collect_date", all = TRUE)
  full_cdc_cases[is.na(full_cdc_cases)] <- 0
  
  # Lowess smoothing data
  # # Plot with raw data
  # plot(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, main="Scatterplot CDC Cases 75th Percentile",
  #      xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
  # # Smooth fit
  # lines(lowess(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, f=0.001), col = "yellow", lwd = 3) # f=0.01
  lowess_CDC_cases = lowess(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, f=0.001)
  
  # add lowess data back
  full_cdc_cases <- cbind(full_cdc_cases, lowess_data = lowess_CDC_cases$y)
  # # Plot with lowess data
  # plot(full_cdc_cases$sample_collect_date, full_cdc_cases$lowess_data, main="Scatterplot CDC Cases 75th Percentile",
  #      xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
  
  # add Risk assessment columns to data levels, trends, categories
  # create data frame
  levels <- 0
  trends <- 0
  categories <- 0
  Risk <- data.frame(levels, trends, categories)
  full_cdc_cases <- cbind(full_cdc_cases, Risk)
  # Calculate levels
  full_range = length(full_cdc_cases$sample_collect_date)
  if (full_range > 22) {
    for (x in 22:full_range) {
      if(full_cdc_cases$lowess_data[x] > quantile_cdc_cases_66){ 
        full_cdc_cases$levels[x] = 6
      } else if(full_cdc_cases$lowess_data[x] < quantile_cdc_cases_33){ 
        full_cdc_cases$levels[x] = 2
      }
      else{ full_cdc_cases$levels[x] = 4
      }
    }
    
    # Calculate trends
    for (x in 22:full_range) {
      if(full_cdc_cases$lowess_data[x] > full_cdc_cases$lowess_data[x-21] * 1.2) { 
        full_cdc_cases$trends[x] = 1
      } else if(full_cdc_cases$lowess_data[x] < full_cdc_cases$lowess_data[x-21] * 0.8){ 
        full_cdc_cases$trends[x] = -1
      }
      else{ full_cdc_cases$trends[x] = 0
      }
    }
    
    # Calculate categories
    H_days[z] = 0
    M_days[z] = 0
    L_days[z] = 0
    Risk_level_county_name[z] = 0
    #z=1
    for (x in 22:full_range) {
      Risk_level_county_name[z] = buffer_unique$county_names[z]
      if((as.numeric(full_cdc_cases$levels[x]) + as.numeric(full_cdc_cases$trends[x])) > as.numeric(4)) { 
        full_cdc_cases$categories[x] = "H"
        H_days[z] = H_days[z] + 1 
      } else if((as.numeric(full_cdc_cases$levels[x]) + as.numeric(full_cdc_cases$trends[x])) < as.numeric(3)){ 
        full_cdc_cases$categories[x] = "L"
        L_days[z] = L_days[z] + 1
      }
      else{ full_cdc_cases$categories[x] = "M"
      M_days[z] = M_days[z] + 1
      }
    }
    
    # create data frame
    # count_days <- data.frame(Risk_level_county_name, H_days, M_days, L_days)
    #write_xlsx(count_days, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_CDC_cases_226_counties.xlsx")
    
    # write to file
    # filenames = paste("~/Documents/GitHub/R_UF/ML/WWScan/Auto_risk/Risk_levels_CDC_cases_county_", buffer_unique$county_names[z], ".xlsx")
    # write_xlsx(full_cdc_cases, filenames)
    # write count_days to 1 file
  }
  #}
  ###############################################
  # loading data virus concentration for only 226 counties (same counites with CDC_cases)
  # covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 2))
  # buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
  # buffer_concen_full = covid_concen %>% distinct()
  # quantile_wastewater_concen_33 = 0
  # quantile_wastewater_concen_66 = 0
  # concen_range = 226 #815 226
  
  ###################################################
  ###################################################
  
  # H_days_virus = 0
  # M_days_virus = 0
  # L_days_virus = 0
  # Risk_level_county_name_virus = 0
  #for (z in 1:1) {
  #z = 10
  virus_county <- buffer_concen_full[county_names == buffer_unique$county_names[z]] #1
  if (length(virus_county$sample_collect_date) > 22) {
    quantile_cdc_cases_33 = quantile(virus_county$pcr_target_flowpop_lin, probs = c(0.33))
    quantile_cdc_cases_66 = quantile(virus_county$pcr_target_flowpop_lin, probs = c(0.66))
    # write_xlsx(covid_county_6001, "~/Documents/GitHub/R_UF/ML/WWScan/CDC_Covid_cases_county_6001.xlsx")
    # Arranging name according to the age
    virus_county.pcr_target_flowpop_lin <- arrange(virus_county, sample_collect_date)
    virus_county <- virus_county.pcr_target_flowpop_lin
    virus_county <- virus_county[, -c("county_names")]
    # length(virus_county$sample_collect_date)
    full_date <- seq(from = as.Date(virus_county$sample_collect_date[1]), 
                     to = as.Date(virus_county$sample_collect_date[length(virus_county$sample_collect_date)]), by = 'day') # V
    # create data frame
    sample_collect_date <- full_date
    full_dates <- data.frame(sample_collect_date)
    
    # get full dates with cases, NA will be assigned 0
    full_ww_virus = merge(x = full_dates, y = virus_county, by = "sample_collect_date", all = TRUE)
    full_ww_virus[is.na(full_ww_virus)] <- 0
    
    # Lowess smoothing data
    # # Plot with raw data
    # plot(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, main="Scatterplot CDC Cases 75th Percentile",
    #      xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
    # # Smooth fit
    # lines(lowess(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, f=0.001), col = "yellow", lwd = 3) # f=0.01
    lowess_ww_virus = lowess(full_ww_virus$sample_collect_date, full_ww_virus$pcr_target_flowpop_lin, f=0.001)
    
    # add lowess data back
    full_ww_virus <- cbind(full_ww_virus, lowess_data_virus = lowess_ww_virus$y)
    # Plot with lowess data
    # plot(full_cdc_cases$sample_collect_date, full_cdc_cases$lowess_data, main="Scatterplot CDC Cases 75th Percentile",
    #      xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
    
    # add Risk assessment columns to data levels, trends, categories
    # create data frame
    levels_virus <- 0
    trends_virus <- 0
    categories_virus <- 0
    Risk <- data.frame(levels_virus, trends_virus, categories_virus)
    full_ww_virus <- cbind(full_ww_virus, Risk)
    # Calculate levels
    full_range = length(full_ww_virus$sample_collect_date)
    if (full_range > 22) {
      for (x in 22:full_range) {
        if(full_ww_virus$lowess_data[x] > quantile_cdc_cases_66){ 
          full_ww_virus$levels_virus[x] = 6
        } else if(full_ww_virus$lowess_data[x] < quantile_cdc_cases_33){ 
          full_ww_virus$levels_virus[x] = 2
        }
        else{ full_ww_virus$levels_virus[x] = 4
        }
      }
      
      # Calculate trends
      for (x in 22:full_range) {
        if(full_ww_virus$lowess_data_virus[x] > full_ww_virus$lowess_data_virus[x-21] * 1.2) { 
          full_ww_virus$trends_virus[x] = 1
        } else if(full_ww_virus$lowess_data_virus[x] < full_ww_virus$lowess_data_virus[x-21] * 0.8){ 
          full_ww_virus$trends_virus[x] = -1
        }
        else{ full_ww_virus$trends_virus[x] = 0
        }
      }
      
      # Calculate categories
      H_days_virus[z] = 0
      M_days_virus[z] = 0
      L_days_virus[z] = 0
      Risk_level_county_name_virus[z] = 0
      #z=1
      for (x in 22:full_range) {
        Risk_level_county_name_virus[z] = buffer_unique$county_names[z]
        if((as.numeric(full_ww_virus$levels_virus[x]) + as.numeric(full_ww_virus$trends_virus[x])) > as.numeric(4)) { 
          full_ww_virus$categories_virus[x] = "H"
          H_days_virus[z] = H_days_virus[z] + 1 
        } else if((as.numeric(full_ww_virus$levels_virus[x]) + as.numeric(full_ww_virus$trends_virus[x])) < as.numeric(3)){ 
          full_ww_virus$categories_virus[x] = "L"
          L_days_virus[z] = L_days_virus[z] + 1
        }
        else{ full_ww_virus$categories_virus[x] = "M"
        M_days_virus[z] = M_days_virus[z] + 1
        }
      }
      
      # create data frame
      # count_days_virus <- data.frame(Risk_level_county_name_virus, H_days_virus, M_days_virus, L_days_virus)
      #write_xlsx(count_days_virus, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_virus_concentration_226_counties.xlsx")
      
      # write to file
      # filenames = paste("~/Documents/GitHub/R_UF/ML/WWScan/WW_risk/Risk_levels_virus_concentration_county_", buffer_unique$county_names[z], ".xlsx")
      # write_xlsx(full_ww_virus, filenames)
      # write count_days to 1 file
      #}
      #}
      
      
      # Join WW and CC to get the overlap
      # join_data <- 0
      join_data = merge(x = full_cdc_cases, y = full_ww_virus, by = "sample_collect_date")
      join_data <- filter(join_data, categories != 0, categories_virus != 0)
      # length(join_data$sample_collect_date)
      H_case[z] = 0
      M_case[z] = 0
      L_case[z] = 0
      H_virus[z] = 0
      M_virus[z] = 0
      L_virus[z] = 0
      if (length(join_data$sample_collect_date)>0) {
        for (t in 1:length(join_data$sample_collect_date)) {
          if(join_data$categories[t] == "H") { 
            H_case[z] = H_case[z] + 1 
          } else if(join_data$categories[t] == "L"){ 
            L_case[z] = L_case[z] + 1
          }
          else{M_case[z] = M_case[z] + 1
          }
        }
        for (t in 1:length(join_data$sample_collect_date)) {
          if(join_data$categories_virus[t] == "H") { 
            H_virus[z] = H_virus[z] + 1 
          } else if(join_data$categories_virus[t] == "L"){ 
            L_virus[z] = L_virus[z] + 1
          }
          else{M_virus[z] = M_virus[z] + 1
          }
        }
      }
    }
  }
}

# Plot data
df_mul <- data.frame(counties_name = buffer_unique$county_names,
                     No_of_days_High_Virus_concen = H_virus,
                     No_of_days_Medium_Virus_concen = M_virus,
                     No_of_days_Low_Virus_concen = L_virus,
                     No_of_days_High_Covid_case = H_case,
                     No_of_days_Medium_Covid_case = M_case,
                     No_of_days_Low_Covid_case = L_case)

# write data to excel file
# write_xlsx(df_mul, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_categories_overlap_time_226_counties_100k.xlsx")


# Plot High category
plot(df_mul$No_of_days_High_Virus_concen, df_mul$No_of_days_High_Covid_case, 
     main="Number of days in High Risk",
     xlab="Virus concentration", ylab="CDC covid cases", 
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(df_mul$No_of_days_High_Virus_concen ~ df_mul$No_of_days_High_Covid_case), col = "orange", lwd = 3)
model1 <- summary(lm(df_mul$No_of_days_High_Virus_concen ~ df_mul$No_of_days_High_Covid_case, data = df_mul))
model1

# Plot Medium category
plot(df_mul$No_of_days_Medium_Virus_concen, df_mul$No_of_days_Medium_Covid_case, 
     main="Number of days in Medium Risk",
     xlab="Virus concentration", ylab="CDC covid cases", 
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(df_mul$No_of_days_Medium_Virus_concen ~ df_mul$No_of_days_Medium_Covid_case), col = "orange", lwd = 3)
model2 <- summary(lm(df_mul$No_of_days_Medium_Virus_concen ~ df_mul$No_of_days_Medium_Covid_case, data = df_mul))
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
H_M_virus = df_mul$No_of_days_High_Virus_concen + df_mul$No_of_days_Medium_Virus_concen
H_M_cases = df_mul$No_of_days_High_Covid_case + df_mul$No_of_days_Medium_Covid_case
plot(H_M_virus, H_M_cases, 
     main="Number of days in High and Medium Risk",
     xlab="Virus concentration", ylab="CDC covid cases", 
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(H_M_virus ~ H_M_cases, data = df_mul), col = "orange", lwd = 3)
model4 <- summary(lm(H_M_virus ~ H_M_cases, data = df_mul))
plot(model4$residuals, pch = 16, col = "red")
model4

###############################################
###############################################
###############################################
# Plot risk levels data
set.seed(1234)                                             
df <- data.frame(HIGH = H_days)#,
# Medium = M_days,
# LOW = L_days)
plot.ts(df, main="Number of days in each risk category of CDC_cases data",
        xlab="226 counties", ylab="Number of day with High risk category", col="darkgreen")
# grid(nx = 20, ny = 20,
#      lty = 2,      # Grid line type
#      col = "gray", # Grid line color
#      lwd = 2)      # Grid line width

###################################################
###################################################

for (x in 1:226) {
  covid_county <- buffer_full[county_names == buffer_unique$county_names[x]]
  quantile_cdc_cases_33[x] = quantile(covid_county$cases_by_cdc_case_earliest_date, probs = c(0.33)) #,0.375,0.625,0.875))
  quantile_cdc_cases_66[x] = quantile(covid_county$cases_by_cdc_case_earliest_date, probs = c(0.66))
  # filenames = paste("~/Documents/GitHub/R_UF/ML/WWScan/", buffer_unique$county_names[x], ".xlsx")
  # write_xlsx(covid_county, filenames)
}
# create data frame
covid_case_county_names <- buffer_unique$county_names #1:226 #
# quantile_covid_66 <- quantile_cdc_cases_66
# quantile_covid_33 <- quantile_cdc_cases_33
quantile_covid_case <- data.frame(covid_case_county_names, quantile_cdc_cases_33, quantile_cdc_cases_66)
write_xlsx(quantile_covid_case, "~/Documents/GitHub/R_UF/ML/WWScan/CDC_Covid_cases_quantile_33_66_226_counties.xlsx")

# boxplot(order ~ quantile_covid, data = quantile_covid_case,
#         main = "Displacement by Gear",
#         xlab = "Gear",
#         ylab = "Displacement")
# boxplot(quantile_covid_case$quantile_covid, outline=FALSE)

plot(covid_case_county_names, quantile_covid, main="Scatterplot CDC Cases 75th Percentile",
     xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
# Smooth fit
lines(lowess(covid_case_county_names, quantile_covid, f=0.001), col = "yellow", lwd = 3) # f=0.01

boxplot(quantile_cases_75, main="Boxplot CDC Cases 75th Percentile",
        xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", outline=FALSE)

##################################################
# loading data virus concentration for only 226 counties (same counites with CDC_cases)
covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 2))
buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
buffer_concen_full = covid_concen %>% distinct()
quantile_wastewater_concen_33 = 0
quantile_wastewater_concen_66 = 0
concen_range = 226 #815 226

###################################################
###################################################

H_days_virus = 0
M_days_virus = 0
L_days_virus = 0
Risk_level_county_name_virus = 0
for (z in 1:1) {
  #z = 10
  virus_county <- buffer_concen_full[county_names == buffer_unique$county_names[z]] #1
  if (length(virus_county$sample_collect_date) > 22) {
    quantile_cdc_cases_33 = quantile(virus_county$pcr_target_flowpop_lin, probs = c(0.33))
    quantile_cdc_cases_66 = quantile(virus_county$pcr_target_flowpop_lin, probs = c(0.66))
    # write_xlsx(covid_county_6001, "~/Documents/GitHub/R_UF/ML/WWScan/CDC_Covid_cases_county_6001.xlsx")
    # Arranging name according to the age
    virus_county.pcr_target_flowpop_lin <- arrange(virus_county, sample_collect_date)
    virus_county <- virus_county.pcr_target_flowpop_lin
    virus_county <- virus_county[, -c("county_names")]
    # length(virus_county$sample_collect_date)
    full_date <- seq(from = as.Date(virus_county$sample_collect_date[1]), 
                     to = as.Date(virus_county$sample_collect_date[length(virus_county$sample_collect_date)]), by = 'day') # V
    # create data frame
    sample_collect_date <- full_date
    full_dates <- data.frame(sample_collect_date)
    
    # get full dates with cases, NA will be assigned 0
    full_ww_virus = merge(x = full_dates, y = virus_county, by = "sample_collect_date", all = TRUE)
    full_ww_virus[is.na(full_ww_virus)] <- 0
    
    # Lowess smoothing data
    # # Plot with raw data
    # plot(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, main="Scatterplot CDC Cases 75th Percentile",
    #      xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
    # # Smooth fit
    # lines(lowess(full_cdc_cases$sample_collect_date, full_cdc_cases$cases_by_cdc_case_earliest_date, f=0.001), col = "yellow", lwd = 3) # f=0.01
    lowess_ww_virus = lowess(full_ww_virus$sample_collect_date, full_ww_virus$pcr_target_flowpop_lin, f=0.001)
    
    # add lowess data back
    full_ww_virus <- cbind(full_ww_virus, lowess_data_virus = lowess_ww_virus$y)
    # Plot with lowess data
    # plot(full_cdc_cases$sample_collect_date, full_cdc_cases$lowess_data, main="Scatterplot CDC Cases 75th Percentile",
    #      xlab="226 counties", ylab="75th Percentile Covid Cases by CDC", pch=19, col="darkgreen", cex=0.75)
    
    # add Risk assessment columns to data levels, trends, categories
    # create data frame
    levels_virus <- 0
    trends_virus <- 0
    categories_virus <- 0
    Risk <- data.frame(levels_virus, trends_virus, categories_virus)
    full_ww_virus <- cbind(full_ww_virus, Risk)
    # Calculate levels
    full_range = length(full_ww_virus$sample_collect_date)
    if (full_range > 22) {
      for (x in 22:full_range) {
        if(full_ww_virus$lowess_data[x] > quantile_cdc_cases_66){ 
          full_ww_virus$levels_virus[x] = 6
        } else if(full_ww_virus$lowess_data[x] < quantile_cdc_cases_33){ 
          full_ww_virus$levels_virus[x] = 2
        }
        else{ full_ww_virus$levels_virus[x] = 4
        }
      }
      
      # Calculate trends
      for (x in 22:full_range) {
        if(full_ww_virus$lowess_data_virus[x] > full_ww_virus$lowess_data_virus[x-21] * 1.2) { 
          full_ww_virus$trends_virus[x] = 1
        } else if(full_ww_virus$lowess_data_virus[x] < full_ww_virus$lowess_data_virus[x-21] * 0.8){ 
          full_ww_virus$trends_virus[x] = -1
        }
        else{ full_ww_virus$trends_virus[x] = 0
        }
      }
      
      # Calculate categories
      H_days_virus[z] = 0
      M_days_virus[z] = 0
      L_days_virus[z] = 0
      Risk_level_county_name_virus[z] = 0
      #z=1
      for (x in 22:full_range) {
        Risk_level_county_name_virus[z] = buffer_unique$county_names[z]
        if((as.numeric(full_ww_virus$levels_virus[x]) + as.numeric(full_ww_virus$trends_virus[x])) > as.numeric(4)) { 
          full_ww_virus$categories_virus[x] = "H"
          H_days_virus[z] = H_days_virus[z] + 1 
        } else if((as.numeric(full_ww_virus$levels_virus[x]) + as.numeric(full_ww_virus$trends_virus[x])) < as.numeric(3)){ 
          full_ww_virus$categories_virus[x] = "L"
          L_days_virus[z] = L_days_virus[z] + 1
        }
        else{ full_ww_virus$categories_virus[x] = "M"
        M_days_virus[z] = M_days_virus[z] + 1
        }
      }
      
      # create data frame
      count_days_virus <- data.frame(Risk_level_county_name_virus, H_days_virus, M_days_virus, L_days_virus)
      #write_xlsx(count_days_virus, "~/Documents/GitHub/R_UF/ML/WWScan/Risk_levels_virus_concentration_226_counties.xlsx")
      
      # write to file
      # filenames = paste("~/Documents/GitHub/R_UF/ML/WWScan/WW_risk/Risk_levels_virus_concentration_county_", buffer_unique$county_names[z], ".xlsx")
      # write_xlsx(full_ww_virus, filenames)
      # write count_days to 1 file
    }
  }
}

# Join WW and CC to get the overlap
# join_data <- 0
join_data = merge(x = full_cdc_cases, y = full_ww_virus, by = "sample_collect_date")
join_data <- filter(join_data, categories_virus != 0, categories_virus != 0)
# length(join_data$sample_collect_date)
H_case <- 0
M_case <- 0
L_case <- 0
H_virus <- 0
M_virus <- 0
L_virus <- 0
for (t in 1:length(join_data$sample_collect_date)) {
  if(join_data$categories[t] == "H") { 
    H_case[1] = H_case[1] + 1 
  } else if(join_data$categories[t] == "L"){ 
    L_case[1] = L_case[1] + 1
  }
  else{M_case[1] = M_case[1] + 1
  }
}


# Plot risk levels data
set.seed(1234)                                             
df <- data.frame(HIGH_virus = H_days_virus)#,
# Medium = M_days,
# LOW = L_days)
plot.ts(df, main="Number of days in each risk categories of virus concentration",
        xlab="226 counties", ylab="Number of day with High risk category", col="darkgreen")

# plot 2 high day risk level
df_mul <- data.frame(x = 1:226,
                     Virus_concen = H_days_virus,
                     Covid_case = H_days)

plot(df_mul$x, df_mul$Virus_concen, main="Number of days in each risk categories",
     xlab="226 counties", ylab="Number of day with High risk category",
     type = "o", col = 1, ylim = c(0, 2000)) 
lines(df_mul$x, df_mul$Covid_case, type = "o", col = 2)


plot(df_mul$Covid_case, df_mul$Virus_concen, main="Number of days in each risk categories",
     xlab="CDC_case", ylab="Virus_concentration",
     type = "o", col = 1, ylim = c(0, 2000)) 
cor(df_mul$Covid_case, df_mul$Virus_concen)

plot(H_days_virus, H_days, 
     main="Number of days in High Risk",
     xlab="Virus concentration", ylab="CDC covid cases", 
     pch=19, col="darkgreen", cex=0.75)
# Linear fit
abline(lm(H_days_virus ~ H_days), col = "orange", lwd = 3)
model1 <- summary(lm(H_days_virus ~ H_days, data = df_mul))
# abline(model1, col = "orange", lwd = 3) #Add a regression line

# grid(nx = 20, ny = 20,
#      lty = 2,      # Grid line type
#      col = "gray", # Grid line color
#      lwd = 2)      # Grid line width

###################################################
###################################################

###################################################
# extract virus concentration data of county 6001
virus_county_6001 <- buffer_concen_full[county_names == 6001]
write_xlsx(virus_county_6001, "~/Documents/GitHub/R_UF/ML/WWScan/Virus_concentration_county_6001.xlsx")
###################################################

for (x in 1:concen_range) { 
  covid_concen_county <- buffer_concen_full[county_names == buffer_unique$county_names[x]] #buffer_concen_unique
  quantile_wastewater_concen_33[x] = quantile(covid_concen_county$pcr_target_flowpop_lin, probs = c(0.33)) #,0.375,0.625,0.875))
  # quantile_wastewater_concen_33[x] = quantile_wastewater_concen_33[x] / 2000000
  quantile_wastewater_concen_66[x] = quantile(covid_concen_county$pcr_target_flowpop_lin, probs = c(0.66)) #,0.375,0.625,0.875))
  # quantile_wastewater_concen_66[x] = quantile_wastewater_concen_66[x] / 2000000
}

# create data frame
covid_concen_county_names <- buffer_unique$county_names #1:concen_range # buffer_concen_unique$county_names # 
# quantile_concen <- quantile_concen_cases_75
quantile_virus_concen <- data.frame(covid_concen_county_names, quantile_wastewater_concen_33, quantile_wastewater_concen_66)
write_xlsx(quantile_virus_concen, "~/Documents/GitHub/R_UF/ML/WWScan/Virus_concentration_quantile_33_66_226_counties.xlsx")

# boxplot(order ~ quantile_covid, data = quantile_covid_case,
#         main = "",
#         xlab = "",
#         ylab = "")
# boxplot(quantile_covid_case$quantile_covid, outline=FALSE)

plot(covid_concen_county_names, quantile_concen, 
     main="Scatterplot Virus Concentration 75th Percentile",
     xlab="", ylab="75th Percentile Virus Concentration in Wastewater", 
     pch=19, col="darkgreen", cex=0.75)

# Smooth fit
lines(lowess(covid_concen_county_names, quantile_concen, f=0.001), col = "blue", lwd = 3)

# Add plot of CDC cases
# points(x2, y2, col = "green", pch = 19)
points(covid_case_county_names, quantile_covid, pch=19, col="red", cex=0.75)
# Smooth fit
lines(lowess(covid_case_county_names, quantile_covid, f=0.001), col = "yellow", lwd = 3)

# Legend
legend("topleft", legend = c("Virus concentration", "Lowess Virus concentration", 
                             "CDC covid cases", "Lowess CDC covid cases"),
       fill = c("darkgreen", "blue", "red", "yellow"))



boxplot(quantile_concen_cases_75, main="Boxplot Virus Concentration 75th Percentile",
        xlab="226 counties", ylab="75th Percentile Virus Concentration in Wastewater", outline=FALSE)


# Multiple Box plot
# boxplot(quantile_cases_75, outline=FALSE, add=TRUE, border="red")
# set.seed(20000)             
#quantile_cases_75[227:815] = 0
data <- data.frame( Virus_Concentration = quantile_concen_cases_75, 
                    CDC_Cases_per_100k_pop = quantile_cases_75 
) 

# Applying boxplot function 
boxplot(data, main="Boxplot 75th Percentile of Virus Concentration and CDC_cases_per_100k_pop of 226 counties",
        xlab="", ylab="75th Percentile", outline=FALSE)  

##################################################
# loading data virus concentration of 815 counties
covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 2))
buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
buffer_concen_full = covid_concen %>% distinct()
quantile_concen_cases_75 = 0
concen_range = 815 #815 226
for (x in 1:concen_range) { 
  covid_concen_county <- buffer_concen_full[county_names == buffer_concen_unique$county_names[x]] #buffer_unique
  quantile_concen_cases_75[x] = quantile(covid_concen_county$pcr_target_flowpop_lin, probs = c(0.75)) #,0.375,0.625,0.875))
  quantile_concen_cases_75[x] = quantile_concen_cases_75[x] / 2000000
}

# create data frame
covid_concen_county_names <- 1:concen_range # buffer_concen_unique$county_names # buffer_unique$county_names #
quantile_concen <- quantile_concen_cases_75
quantile_covid_concen <- data.frame(covid_concen_county_names, quantile_concen)
# write_xlsx(quantile_covid_concen, "~/Documents/GitHub/R_UF/ML/WWScan/Virus_concentration_226_counties.xlsx")

# boxplot(order ~ quantile_covid, data = quantile_covid_case,
#         main = "",
#         xlab = "",
#         ylab = "")
# boxplot(quantile_covid_case$quantile_covid, outline=FALSE)

plot(covid_concen_county_names, quantile_concen, 
     main="Scatterplot Virus Concentration 75th Percentile",
     xlab="", ylab="75th Percentile Virus Concentration in Wastewater", 
     pch=19, col="darkgreen", cex=0.75)

# Smooth fit
lines(lowess(covid_concen_county_names, quantile_concen, f=0.01), col = "blue", lwd = 3)

# Add plot of CDC cases
# points(x2, y2, col = "green", pch = 19)
points(covid_case_county_names, quantile_covid, pch=19, col="red", cex=0.75)
# Smooth fit
lines(lowess(covid_case_county_names, quantile_covid, f=0.01), col = "yellow", lwd = 3)

# Legend
legend("topleft", legend = c("Virus concentration", "Lowess Virus concentration", 
                             "CDC covid cases", "Lowess CDC covid cases"),
       fill = c("darkgreen", "blue", "red", "yellow"))



boxplot(quantile_concen_cases_75, main="Boxplot Virus Concentration 75th Percentile",
        xlab="", ylab="75th Percentile Virus Concentration in Wastewater", outline=FALSE)


# Multiple Box plot
# boxplot(quantile_cases_75, outline=FALSE, add=TRUE, border="red")
set.seed(20000)             
quantile_cases_75[227:815] = 0
data <- data.frame( Virus_Concentration = quantile_concen_cases_75, 
                    CDC_Cases = quantile_cases_75 
) 

# Applying boxplot function 
boxplot(data, main="Boxplot 75th Percentile of Virus Concentration (815 counties) and CDC_cases_per_100k_pop (226 counties)",
        xlab="", ylab="75th Percentile", outline=FALSE)  





