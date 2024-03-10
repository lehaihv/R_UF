#########################
# AUTHOR: Hai LE        #
# DATE:   2024-03-09    #
#########################

# Load libraries
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

library(readxl)
library(writexl)

options(max.print=999999)

# MacOS path
raw_path     <- "~/Documents/GitHub/R_UF/ML/WWScan/"
data_path     <- "~/Documents/GitHub/R_UF/ML/WWScan/"
result_path   <- "~/Documents/GitHub/R_UF/ML/WWScan/"

# set seed
set.seed(1234)

# load data covid cases
covid_key_sewershed <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 1))
covid_key_sewershed <- covid_key_sewershed[, -c("population_served")]
covid_cases <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 3))

covid_cases_cdc <- inner_join(covid_key_sewershed %>% distinct(), covid_cases, by = "key_sewershed") #"county_names")
covid_cases_cdc <- covid_cases_cdc[, -c("key_sewershed")]

# covid_cases_cdc %>% distinct()
# covid_final <- inner_join(covid2, covid3, by = "key_sewershed")
# write_xlsx(covid3 %>% distinct(), "~/Documents/GitHub/R_UF/ML/WWScan/covid_cdc_cases.xlsx")
# buffer_unique= covid3 %>% distinct(county_names, .keep_all = TRUE)
# buffer_unique$county_names[1]
# buffer_full = covid3 %>% distinct()
# covid_county <- buffer_full[county_names == buffer_unique$county_names[1]]
# covid %>% distinct()
# df_to_join <- unique(covid1)
# buffer_unique$county_names[x]

buffer_unique= covid_cases_cdc %>% distinct(county_names, .keep_all = TRUE)
buffer_full = covid_cases_cdc %>% distinct()
for (x in 1:226) {
  covid_county <- buffer_full[county_names == buffer_unique$county_names[x]]
  quantile_cases_75[x] = quantile(covid_county$cases_by_cdc_case_earliest_date, probs = c(0.75)) #,0.375,0.625,0.875))
  # filenames = paste("~/Documents/GitHub/R_UF/ML/WWScan/", buffer_unique$county_names[x], ".xlsx")
  # write_xlsx(covid_county, filenames)
}
# create dataframe
# order <- 1:226
# quantile_covid <- quantile_cases_75
# quantile_covid_case <- data.frame(order, quantile_covid)
# boxplot(order ~ quantile_covid, data = quantile_covid_case,
#         main = "Displacement by Gear",
#         xlab = "Gear",
#         ylab = "Displacement")
# boxplot(quantile_covid_case$quantile_covid, outline=FALSE)

boxplot(quantile_cases_75, outline=FALSE)

# loading data virus concentration
covid_concen <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 2))
buffer_concen_unique= covid_concen %>% distinct(county_names, .keep_all = TRUE)
buffer_concen_full = covid_concen %>% distinct()
for (x in 1:815) {
  covid_concen_county <- buffer_concen_full[county_names == buffer_concen_unique$county_names[x]]
  quantile_concen_cases_75[x] = quantile(covid_concen_county$pcr_target_flowpop_lin, probs = c(0.75)) #,0.375,0.625,0.875))
}

boxplot(quantile_concen_cases_75, outline=FALSE)






