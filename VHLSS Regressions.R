vhlss <- c("vhlss02.Rda", "vhlss04.Rda", "vhlss06.Rda", "vhlss08.Rda", "vhlss10.Rda", "vhlss12.Rda")

for (i in vhlss) {
  load(i)
}

prov_vhlss <- c("prov02_vhlss.Rda", "prov04_vhlss.Rda", "prov06_vhlss.Rda", "prov08_vhlss.Rda", "prov10_vhlss.Rda", "prov12_vhlss.Rda")

for (i in prov_vhlss) {
  load(i)
}

######################################
# VHLSS REGRESSIONS - PROVINCE LEVEL #
######################################

# Ratio of workers 
vhlss_prov <- list(
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov02_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov04_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov06_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov08_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov10_vhlss,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov12_vhlss,
        vcov = ~tinh)
)

vhlss_prov_coef <- lapply(vhlss_prov, tidy)
vhlss_prov_coef <- do.call(rbind, vhlss_prov_coef) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_prov_coef$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_prov_coef))

vhlss_prov_s <-  list(
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov02_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov04_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov06_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov10_vhlss, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov12_vhlss, south == 1),
        vcov = ~tinh)
)

vhlss_prov_n <-  list(
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov02_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov04_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov06_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov10_vhlss, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov12_vhlss, south == 0),
        vcov = ~tinh)
)

vhlss_prov_coef_n <- lapply(vhlss_prov_n, tidy)
vhlss_prov_coef_s <- lapply(vhlss_prov_s, tidy)
vhlss_prov_coef_n <- do.call(rbind, vhlss_prov_coef_n) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_prov_coef_s <- do.call(rbind, vhlss_prov_coef_s) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_prov_coef_n$group <- "North"
vhlss_prov_coef_s$group <- "South"
vhlss_prov_coef_n$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_prov_coef_n))
vhlss_prov_coef_s$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_prov_coef_s)) 
vhlss_prov_coef_ns <- rbind(vhlss_prov_coef_n, vhlss_prov_coef_s)

# Ratio of formal workers 

vhlss_formal_prov <- list(
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov02_vhlss,
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov04_vhlss,
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov06_vhlss,
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov08_vhlss,
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov10_vhlss,
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov12_vhlss,
        vcov = ~tinh)
)

vhlss_formal_prov_coef <- lapply(vhlss_formal_prov, tidy)
vhlss_formal_prov_coef <- do.call(rbind, vhlss_formal_prov_coef) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_formal_prov_coef$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_formal_prov_coef))

vhlss_formal_prov_s <-  list(
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov02_vhlss, south == 1),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov04_vhlss, south == 1),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov06_vhlss, south == 1),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 1),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov10_vhlss, south == 1),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov12_vhlss, south == 1),
        vcov = ~tinh)
)

vhlss_formal_prov_n <-  list(
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov02_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov04_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov06_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov10_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov12_vhlss, south == 0),
        vcov = ~tinh)
)

vhlss_formal_prov_coef_n <- lapply(vhlss_formal_prov_n, tidy)
vhlss_formal_prov_coef_s <- lapply(vhlss_formal_prov_s, tidy)
vhlss_formal_prov_coef_n <- do.call(rbind, vhlss_formal_prov_coef_n) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_formal_prov_coef_s <- do.call(rbind, vhlss_formal_prov_coef_s) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_formal_prov_coef_n$group <- "North"
vhlss_formal_prov_coef_s$group <- "South"
vhlss_formal_prov_coef_n$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_formal_prov_coef_n))
vhlss_formal_prov_coef_s$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_formal_prov_coef_s)) 
vhlss_formal_prov_coef_ns <- rbind(vhlss_formal_prov_coef_n, vhlss_formal_prov_coef_s)

# Self employed 

vhlss_selfemp_prov <- list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov02_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov04_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov06_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov08_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov10_vhlss,
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        prov12_vhlss,
        vcov = ~tinh)
)

vhlss_selfemp_prov_coef <- lapply(vhlss_selfemp_prov, tidy)
vhlss_selfemp_prov_coef <- do.call(rbind, vhlss_selfemp_prov_coef) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_selfemp_prov_coef$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_selfemp_prov_coef))

vhlss_selfemp_prov_s <-  list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov02_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov04_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov06_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov10_vhlss, south == 1),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov12_vhlss, south == 1),
        vcov = ~tinh)
)

vhlss_selfemp_prov_n <-  list(
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov02_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov04_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov06_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov08_vhlss, south == 0),
        vcov = ~tinh),
  feols(selfemp_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov10_vhlss, south == 0),
        vcov = ~tinh),
  feols(wagework_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + dist_nearest_base_prov + dist_nearest_hochi_prov,
        subset(prov12_vhlss, south == 0),
        vcov = ~tinh)
)

vhlss_selfemp_prov_coef_n <- lapply(vhlss_selfemp_prov_n, tidy)
vhlss_selfemp_prov_coef_s <- lapply(vhlss_selfemp_prov_s, tidy)
vhlss_selfemp_prov_coef_n <- do.call(rbind, vhlss_selfemp_prov_coef_n) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_selfemp_prov_coef_s <- do.call(rbind, vhlss_selfemp_prov_coef_s) %>% filter(term == "log(tot_bmr_prov_ppn)")
vhlss_selfemp_prov_coef_n$group <- "North"
vhlss_selfemp_prov_coef_s$group <- "South"
vhlss_selfemp_prov_coef_n$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_selfemp_prov_coef_n))
vhlss_selfemp_prov_coef_s$year <- rep(seq(2001, 2011, by = 2), length.out = nrow(vhlss_selfemp_prov_coef_s)) 
vhlss_selfemp_prov_coef_ns <- rbind(vhlss_selfemp_prov_coef_n, vhlss_selfemp_prov_coef_s)

########################################
# VHLSS REGRESSIONS - INDIVIDUAL LEVEL #
########################################

# Probability of working 

etable(list(
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02),
        weights = ~wt75,
        vcov = ~tinh02),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss10),
        weights = ~wt9,
        vcov = ~tinh)
))

etable(list(
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02, south == 1),
        weights = ~wt75,
        vcov = ~tinh02),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04, south == 1),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06, south == 1),
        weights = ~wt45,
        vcov = ~tinh)  
))

etable(list(
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02, south == 0),
        weights = ~wt75,
        vcov = ~tinh02),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04, south == 0),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06, south == 0),
        weights = ~wt45,
        vcov = ~tinh)  
))

# Probability of being a manager of HH business 

etable(list(
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss02),
        weights = ~wt75,
        vcov = ~tinh02),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss04),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss06),
        weights = ~wt45,
        vcov = ~tinh),
  feols(work ~ as.factor(female)/log(tot_bmr_prov) + age + age^2 + educ,
        subset(vhlss10),
        weights = ~wt9,
        vcov = ~tinh)
))