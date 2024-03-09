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

# load data
covid <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 1))
covid1 <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 2))
covid2 <- as.data.table(read_excel("~/Documents/GitHub/R_UF/ML/WWScan/working_data.xlsx", sheet = 3))
df_to_join <- unique(covid1)
covid3 <- inner_join(covid, df_to_join, by = "county_names")
covid_final <- inner_join(covid2, covid3, by = "key_sewershed")









