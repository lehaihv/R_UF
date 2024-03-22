#########################
# AUTHOR: Hai LE        #
# DATE:   2024-03-09    #
#########################

# Load libraries
library(data.table)
# setDTthreads(0)
# library(bit64)
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

# options(max.print=999999)

# set seed
# set.seed(1234)

##################################################
##################################################
# load data covid cases
# jh_cc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_confirmed_US.xlsx"))
# ww_county <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/County_ww.xlsx"))
# ww_county <- ww_county[, -c("sample_collect_date", "pcr_target_flowpop_lin")]
# jh_cc_cases = merge(x = jh_cc, y = ww_county, by = "county_names")
# write_xlsx(jh_cc_cases, "~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_ww.xlsx")
# jh_cc_cases <- jh_cc_cases[, -c(2:50)]
# order <- 1:1142
# lowess_county_data <- as.data.table(order)
# lowess_county_data_buff <- 0
# jh_cc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_ww.xlsx"))
# for (i in 1:812) { #812
#   #i=1
#   county_data <- jh_cc[i,]     # read the row data of county
#   county_data_buff <- data.frame(county_data[,3:1145])
#   # x = 100
#   # county_data_buff[,x] # as.numeric(x)
#   for (x in 1:1142) { #1142
#     county_data_buff[,x] = county_data_buff[,x+1] - county_data_buff[,x]
#   }
#   order <- 1:1142
#   lowess_county_data_buff <- lowess(order, county_data_buff[1:1142], f=0.001)
#   lowess_county_data <- cbind(lowess_county_data, lowess_county_data_buff$y)
# }
# # write_xlsx(lowess_county_data, "~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_lowess_ww.xlsx")
# 
# ##################################################
# ##################################################
# lowess_county_data <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_lowess_ww.xlsx"))
# jh_cc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_ww.xlsx"))
# sample_collect_date <- jh_cc$dates[1:1142]
# county_names <- 0
# lowess_col <- 0
# lowess_col <- data.frame(a=unlist(lowess_county_data[,2:813], use.names = FALSE))
# 
# 
# for (i in 2:813) {
#   county_names[((i-2)*1142 + 1):((i-1)*1142)] <- jh_cc$county_names[i-1]
# }
# 
# # cases_by_cdc_case_earliest_date = lowess_col
# lowess_col_full <- data.table(county_names, sample_collect_date, )
# write_xlsx(lowess_col_full, "~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_lowess_812_counties.xlsx")


# 3. Absolute cases 812 counties
covid_cases_cdc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_lowess_812_counties.xlsx", sheet = 1))
# covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed", "population_served")]
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
# write_xlsx(buffer_concen_unique, "~/Documents/GitHub/R_UF/ML/JH_CC/County_ww.xlsx")
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

for (z in 1:812) {
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
write_xlsx(df_mul, "~/Documents/GitHub/R_UF/ML/JH_CC/Risk_categories_overlap_time_812_counties.xlsx")


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
# plot(model4$residuals, pch = 16, col = "red")
model4

###############################################
###############################################
###############################################
















