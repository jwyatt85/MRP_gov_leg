
#### Senate/House/Gov MRP Estimates

library(readr)
library(dplyr)
library(mrpExport)

### Gov estimates ------------------------------------------------------------------------------------------------------------------------
setwd("~/Desktop")
tsdat <- read_rds("tsdat_updated.rds")

tsdat_test2 <- tsdat %>% 
  filter(poll >= 160101)

leg_vars <- grep('^LEG1', names(tsdat_test2), value = TRUE) %>% 
  .[!grepl('^LEG1$|^LEG1NET$', .)]

for (q in leg_vars) {
  temp <- tsdat_test2[[q]]
  temp[is.na(temp)] <- 0
  tsdat_test2[[paste0(q, '_fixed')]] <- temp
}

fixed_vars <- grep('fixed$', names(tsdat_test2), value = TRUE)

tsdat_test2 <- tsdat_test2 %>% 
  mutate(
    LEG1_combined = tsdat_test2[, fixed_vars] %>% 
      rowSums
  )

gov_test <- mrpInput(
  formula        = as.formula("LEG1_combined ~ age + sex + race + education + stname"),
  update_formula = as.formula(.~. + obama12 + unemployment_rate + obama_approve + percent_gdp_increase), 
  polling_data   = tsdat_test2,
  agg_level      = 'stname',
  weights = FALSE
) %>% 
  estimate(disagg=TRUE)

gov_test_final <- gov_test %>%
  transmute(
    Demographic,
    combined = as.numeric(`1`+`2`),
    N
  )

write_csv(gov_test_final, "gov_approval.csv")


#### Congrestional level estimates -------------------------------------------------------------------------
setwd("~/Desktop")
tsdat <- read_rds("tsdat_updated.rds")

tsdat_test2 <- tsdat %>% 
  filter(poll >= 160101)

cong_test <- mrpInput(
  formula        = as.formula("LEG4NET ~ age + sex + education + cd"),
  update_formula = as.formula(.~. + obama12 + median_log + percenthisp + percentblack), 
  polling_data   = tsdat_test2,
  agg_level      = 'cd'
) %>% 
  estimate(disagg=TRUE)

gov_test_final <- gov_test %>%
  transmute(
    Demographic,
    combined = as.numeric(`1`+`2`),
    N
  )


