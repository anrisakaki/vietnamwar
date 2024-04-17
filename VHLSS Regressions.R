######################################
# VHLSS REGRESSIONS - DISTRICT LEVEL #
######################################

etable(list(
  feols(work ~ as.factor(wartime)/as.factor(female)/log(tot_bmr_prov),
        vhlss02,
        weights = ~wt75,
        vcov = ~tinh),
  feols(work ~ as.factor(wartime)/as.factor(female)/log(tot_bmr_prov),
        vhlss04,
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(wartime)/as.factor(female)/log(tot_bmr_prov),
        vhlss06,
        weights = ~wt45,
        vcov = ~tinh)  
))

etable(list(
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02, age > 42 & age < 65),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04, age > 44 & age < 65),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06, age > 46 & age < 65),
        weights = ~wt45,
        vcov = ~tinh+huyen)  
))

etable(list(
  feols(selfemp ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02, age > 15 & age < 65),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04, age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06, age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~tinh+huyen)  
))

etable(list(
  feols(wagework ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02, age > 15 & age < 65),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(wagework ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04, age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(wagework ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06, age > 15 & age < 65),
        weights = ~wt45,
        vcov = ~tinh+huyen)  
))
