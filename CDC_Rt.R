library(EpiEstim)
library(ggplot2)
library(readxl)
## install.packages("writexl")
library(writexl)

## load data
## data1 <- read_excel("D:/GitHub/R_UF/covid_29019_cc.xlsx")
## data_cc <- read_excel("~/Documents/GitHub/R_UF/paper_data.xlsx")
# data_cvs <- read.csv("~/Documents/GitHub/R_UF/cdc-nwss-restricted-dataset-final.csv")
data_cc <- read_excel("~/Documents/GitHub/R_UF/Covid_SantaCruz_cc.xlsx")  ## After Dec 2021
# data_cvs <- read.csv("~/Documents/GitHub/R_UF/us_confirmed.csv")
# data_cc <- read_excel("~/Documents/GitHub/R_UF/Covid_kauai_cc.xlsx")
# data_ww <- read_excel("~/Documents/GitHub/R_UF/covid_29019_cc.xlsx")
## data_ww <- read_excel("~/Documents/GitHub/R_UF/covid_264_ww.xlsx")
# data_cc <- read_excel("~/Documents/GitHub/R_UF/covid_29019_cc_omicron.xlsx") ## Before Dec 2021
# data_ww <- read_excel("~/Documents/GitHub/R_UF/covid_29019_ww_omicron.xlsx")

## view data
## View(data["dates"])
## View(data["I"])
## View(data)

## data(Flu2009)

date_only = as.Date(as.character(as.POSIXct(data_cc$dates)))
data_cc$dates = date_only

# date_only1 = as.Date(as.character(as.POSIXct(data_ww$dates)))
# data_ww$dates = date_only1
# 
# write_xlsx(data, "~/Documents/GitHub/R_UF/covid_concentration1.xlsx")

# Flu2009$incidence$dates = data$dates
# Flu2009$incidence$I = data$I

## use the incidence R package to easily plot the daily incidence data:

# library(incidence)
# plot(as.incidence(data1$I, dates = data1$dates))

## Estimating R with a non parametric serial interval distribution
# res_non_parametric_si <- estimate_R(data1,
#                                     method="non_parametric_si",
#                                     config = make_config(list(
#                                     si_distr = Flu2009$si_distr))
# )
# plot(res_non_parametric_si, "R")

# Estimating R on sliding weekly windows, with a parametric serial interval
# to specify t_start and t_end in config, e.g. to have biweekly sliding
# windows
# t_start <- seq(2, nrow(data_cc)-13)
# t_end <- t_start + 13
# res_parametric_si <- estimate_R(data1,
#                                 method="parametric_si",
#                                 config = make_config(list(
#                                   mean_si = 2.6,
#                                   std_si = 1.5, 
#                                   t_start = t_start, 
#                                   t_end = t_end))
# )
# plot(res_parametric_si, legend = FALSE)

res_parametric_si_cc <- estimate_R(data_cc,
                                method="parametric_si",
                                config = make_config(list(
                                mean_si = 4.7,
                                std_si = 2.9))
)
plot(res_parametric_si_cc, legend = FALSE, "R") ## "R" only plot Estimated Rt

# res_parametric_si_ww <- estimate_R(data_ww,
#                                 method="parametric_si",
#                                 config = make_config(list(
#                                 mean_si = 4.8,
#                                 std_si = 2.3))
# )
# plot(res_parametric_si_ww, legend = FALSE, "R") ## "R" only plot Estimated Rt
# 
# ## visualize R estimates on the same plot
# estimate_R_plots(list(res_parametric_si_cc, res_parametric_si_ww), what = "R",
#                  options_R = list(col = c("blue", "red")), legend = TRUE)


## wrtie value of estimated R to excel file
write_xlsx(res_parametric_si_cc[["R"]],"~/Documents/GitHub/R_UF/covid_Rt_cc.xlsx")
# write_xlsx(res_parametric_si_ww[["R"]],"~/Documents/GitHub/R_UF/covid_29019_Rt_ww.xlsx")


## Estimating R accounting for uncertainty on the serial interval distribution
# config <- make_config(list(mean_si = 2.6, std_mean_si = 1,
#                            min_mean_si = 1, max_mean_si = 4.2,
#                            std_si = 1.5, std_std_si = 0.5,
#                            min_std_si = 0.5, max_std_si = 2.5))
# res_uncertain_si <- estimate_R(data1,
#                                method = "uncertain_si",
#                                config = config)
# 
# plot(res_uncertain_si, legend = FALSE) ## , add=TRUE to plot 2 graph in 1 plot

## Estimating R and the serial interval using data on pairs infector/infected
# MCMC_seed <- 1
# overall_seed <- 2
# mcmc_control <- make_mcmc_control(seed = MCMC_seed, 
#                                   burnin = 1000)
# dist <- "G" # fitting a Gamma dsitribution for the SI
# config <- make_config(list(si_parametric_distr = dist,
#                            mcmc_control = mcmc_control,
#                            seed = overall_seed, 
#                            n1 = 50, 
#                            n2 = 50))
# res_si_from_data <- estimate_R(data,
#                                method = "si_from_data",
#                                si_data = Flu2009$si_data,
#                                config = config)
# 
# plot(res_si_from_data, legend = FALSE)
# incid <- data1
# dt <- 7L
# weekly_incid <- aggregate_inc(incid, dt)
# #> Incidence aggregated up to day 105 of 107
# si_distr <- Flu2009$si_distr
# 
# # estimate Rt using the default parameters (method "non_parametric_si")
# method <- "non_parametric_si"
# config <- make_config(list(si_distr = si_distr))
# res_weekly <- estimate_R_agg(incid = weekly_incid, 
#                              dt = 7L, # aggregation window of the data
#                              dt_out = 7L, # desired sliding window length
#                              iter = 10L,
#                              config = config,
#                              method = method,
#                              grid = list(precision = 0.001, min = -1, max = 1))
# 
# plot(res_weekly)