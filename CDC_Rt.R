library(EpiEstim)
library(ggplot2)

## load data
data(Flu2009)
## incidence:
head(Flu2009$incidence)

library(incidence)
plot(as.incidence(Flu2009$incidence$I, dates = Flu2009$incidence$dates))
res_parametric_si <- estimate_R(Flu2009$incidence, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 2.6, 
                                  std_si = 1.5))
)
plot(res_parametric_si, legend = FALSE)
