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
order <- 1:1142
lowess_county_data <- as.data.table(order)
lowess_county_data_buff <- 0
jh_cc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_ww.xlsx"))
for (i in 1:812) { #812
  #i=1
  county_data <- jh_cc[i,]     # read the row data of county
  county_data_buff <- data.frame(county_data[,3:1145])
  # x = 100
  # county_data_buff[,x] # as.numeric(x)
  for (x in 1:1142) { #1142
    county_data_buff[,x] = county_data_buff[,x+1] - county_data_buff[,x]
  }
  order <- 1:1142
  lowess_county_data_buff <- lowess(order, county_data_buff[1:1142], f=0.001)
  lowess_county_data <- cbind(lowess_county_data, lowess_county_data_buff$y)
}
# write_xlsx(lowess_county_data, "~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_lowess_ww.xlsx")

##################################################
##################################################
lowess_county_data <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_lowess_ww.xlsx"))
jh_cc <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_County_ww.xlsx"))
sample_collect_date <- jh_cc$dates[1:1142]
county_names <- 0
lowess_col <- 0
lowess_col <- data.frame(a=unlist(lowess_county_data[,2:813], use.names = FALSE))


for (i in 2:813) {
  county_names[((i-2)*1142 + 1):((i-1)*1142)] <- jh_cc$county_names[i-1]
}

# cases_by_cdc_case_earliest_date = lowess_col
lowess_col_full <- data.table(county_names, sample_collect_date, )
write_xlsx(lowess_col_full, "~/Documents/GitHub/R_UF/ML/JH_CC/JH_CC_lowess_812_counties.xlsx")
















