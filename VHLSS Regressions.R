
##########################
# PROBABILITY OF WORKING #
##########################

etable(list(
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + marst + tinh,
        subset(vhlss14, south == 0 & age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + marst + tinh,
        subset(vhlss14, south == 1 & age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + marst + tinh,
        subset(vhlss16, south == 0 & age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth),
  feols(work ~ as.factor(female) + i(as.factor(female), tot_bmr_prov_std) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + marst + tinh,
        subset(vhlss16, south == 1 & age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth)), tex = T)

etable(list(
  feols(work ~ as.factor(widowed) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + tinh,
        subset(vhlss14, south == 0 & female == 1 & age > (2014-(1975-15)) & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth),
  feols(work ~ as.factor(widowed) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + tinh,
        subset(vhlss14, south == 1 & female == 1 & age > (2014-(1975-15)) & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth),
  feols(work ~ as.factor(widowed) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + tinh,
        subset(vhlss14, south == 0 & female == 1 & age > (2016-(1975-15)) & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth),
  feols(work ~ as.factor(widowed) + age + age^2 + educ + hhsize + as.factor(migrant) | ethnicity + urban + tinh,
        subset(vhlss14, south == 0 & female == 1 & age > (2016-(1975-15)) & age < 65),
        weights = ~wt45,
        vcov = ~prov_birth)), tex = T)


