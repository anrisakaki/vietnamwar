######################################
# VHLSS REGRESSIONS - PROVINCE LEVEL #
######################################

# Ratio of workers 

vhlss_prov <- list(
  feols(workerratio ~ log(tot_bmr_prov),
        prov02_vhlss,
        vcov = ~tinh02),
  feols(workerratio ~ log(tot_bmr_prov),
        prov04_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        prov06_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        prov10_vhlss,
        vcov = ~tinh)
)

vhlss_prov_s <-  list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov02_vhlss, south == 1),
        vcov = ~tinh02),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov04_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov06_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov10_vhlss, south == 1),
        vcov = ~tinh)  
)

vhlss_prov_n <-  list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov02_vhlss, south == 0),
        vcov = ~tinh02),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov04_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov06_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(prov10_vhlss, south == 0),
        vcov = ~tinh)  
)

vhlss_prov_coef_n <- lapply(vhlss_prov_n, tidy)
vhlss_prov_coef_s <- lapply(vhlss_prov_s, tidy)
vhlss_prov_coef_n <- do.call(rbind, vhlss_prov_coef_n) %>% filter(term != "(Intercept)")
vhlss_prov_coef_s <- do.call(rbind, vhlss_prov_coef_s) %>% filter(term != "(Intercept)")
vhlss_prov_coef_n$group <- "North"
vhlss_prov_coef_s$group <- "South"
vhlss_prov_coef_n$year <- rep(seq(2001, 2009, by = 2), length.out = nrow(vhlss_prov_coef_n))
vhlss_prov_coef_n <- vhlss_prov_coef_n %>% mutate(year = ifelse(year == 2007, 2009, year))
vhlss_prov_coef_s$year <- rep(seq(2001, 2009, by = 2), length.out = nrow(vhlss_prov_coef_s)) 
vhlss_prov_coef_s <-  vhlss_prov_coef_s %>% mutate(year = ifelse(year == 2007, 2009, year))
vhlss_prov_coef_ns <- rbind(vhlss_prov_coef_n, vhlss_prov_coef_s)

etable(list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov02_vhlss,
        vcov = ~tinh02),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov04_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov06_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        prov10_vhlss,
        vcov = ~tinh)
))

vhlss_selfemp_prov_s <- list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov02_vhlss, south == 1),
        vcov = ~tinh02),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov04_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov06_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov10_vhlss, south == 1),
        vcov = ~tinh)  
)

vhlss_selfemp_prov_n <-list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov02_vhlss, south == 0),
        vcov = ~tinh02),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov04_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov06_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov),
        subset(prov10_vhlss, south == 0),
        vcov = ~tinh)  
)

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
