######################################
# VHLSS REGRESSIONS - PROVINCE LEVEL #
######################################

etable(list(
  feols(workerratio ~ log(tot_bmr_prov),
        prov02_vhlss,
        vcov = ~tinh02),
  feols(workerratio ~ log(tot_bmr_prov),
        prov04_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        prov06_vhlss,
        vcov = ~tinh)  
))

etable(list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov02_vhlss, south == 1),
        vcov = ~tinh02),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov04_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov06_vhlss, south == 1),
        vcov = ~tinh)  
))

etable(list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov02_vhlss, south == 0),
        vcov = ~tinh02),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov04_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov06_vhlss, south == 0),
        vcov = ~tinh)  
))

etable(list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov02_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov04_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov06_vhlss,
        vcov = ~tinh)  
))

########################################
# VHLSS REGRESSIONS - INDIVIDUAL LEVEL #
########################################

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
        subset(vhlss02, age > 15 & age < 65 & work == 1),
        weights = ~wt75,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04, age > 15 & age < 65 & work == 1),
        weights = ~wt45,
        vcov = ~tinh+huyen),
  feols(selfemp ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06, age > 15 & age < 65 & work == 1),
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
