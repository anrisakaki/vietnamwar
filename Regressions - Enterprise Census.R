dn <- c("dn_prov.Rda", "dn_dist.Rda")

for (i in dn) {
  load(i)
}

bmr <- c("province_bmr_sum.Rda", "district_bmr_sum.Rda")

for (i in bmr) {
  load(i)
}

#############
# Firm level#
#############

# OLS

dn_ols_prov <- (list(
  feols(workerratio ~ log(tot_bmr_prov),
        dn01,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn02,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn03,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn04,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn05,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn06,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn07,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn08,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn09,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn10,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn11,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn12,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn13,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn14,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn15,
        vcov = ~tinh)))

dn_ols_coef_prov<- lapply(dn_ols_prov, tidy)
dn_ols_coef_prov <- do.call(rbind, dn_ols_coef_prov) %>% filter(term != "(Intercept)")
dn_ols_coef_prov$year <- rep(seq(2001, 2015), each = nrow(dn_ols_coef_prov) / length(seq(2001, 2015)))

## By south 
dn_ols_prov_n <- (list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn01, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn02, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn03, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn04, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn05, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn06, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn07, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn08, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn09, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn10, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn11, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn12, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn13, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn14, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn15, south == 0),
        vcov = ~tinh)))

dn_ols_prov_s <- (list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn01, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn02, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn03, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn04, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn05, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn06, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn07, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn08, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn09, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn10, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn11, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn12, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn13, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn14, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn15, south == 1),
        vcov = ~tinh)))

dn_ols_prov_coef_n <- lapply(dn_ols_prov_n, tidy)
dn_ols_prov_coef_s <- lapply(dn_ols_prov_s, tidy)
dn_ols_prov_coef_n <- do.call(rbind, dn_ols_prov_coef_n) %>% filter(term != "(Intercept)")
dn_ols_prov_coef_s <- do.call(rbind, dn_ols_prov_coef_s) %>% filter(term != "(Intercept)")
dn_ols_prov_coef_n$year <- rep(seq(2001, 2015), each = nrow(dn_ols_prov_coef_n) / length(seq(2001, 2015)))
dn_ols_prov_coef_s$year <- rep(seq(2001, 2015), each = nrow(dn_ols_prov_coef_s) / length(seq(2001, 2015)))

dn_ols_prov_coef_n$group <- "North"
dn_ols_prov_coef_s$group <- "South"
dn_ols_prov_coef_ns <- rbind(dn_ols_prov_coef_n, dn_ols_prov_coef_s)

# Industry Fixed effects 
dn_indfe_prov <- (list(
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn01,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn02,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn03,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn04,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn05,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn06,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn07,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn08,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn09,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn10,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn11,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn12,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn13,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn14,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        dn15,
        vcov = ~tinh)))

dn_indfe_prov_coef <- lapply(dn_indfe_prov, tidy) 
dn_indfe_prov_coef <- do.call(rbind, dn_indfe_prov_coef) %>% filter(term != "(Intercept)")
dn_indfe_prov_coef$year <- rep(seq(2001, 2015), each = nrow(dn_indfe_prov_coef) / length(seq(2001, 2015)))

## By north/south 
dn_indfe_prov_n <- (list(
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn01, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn02, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn03, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn04, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn05, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn06, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn07, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn08, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn09, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn10, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn11, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn12, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn13, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn14, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn15, south == 0),
        vcov = ~tinh)))

dn_indfe_prov_s <- (list(
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn01, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn02, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn03, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn04, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn05, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn06, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn07, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn08, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn09, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn10, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn11, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn12, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn13, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn14, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov) | nganh_kd,
        subset(dn15, south == 1),
        vcov = ~tinh)))

dn_indfe_prov_coef_n <- lapply(dn_indfe_prov_n, tidy)
dn_indfe_prov_coef_s <- lapply(dn_indfe_prov_s, tidy)
dn_indfe_prov_coef_n <- do.call(rbind, dn_indfe_prov_coef_n) %>% filter(term != "(Intercept)")
dn_indfe_prov_coef_s <- do.call(rbind, dn_indfe_prov_coef_s) %>% filter(term != "(Intercept)")
dn_indfe_prov_coef_n$year <- rep(seq(2001, 2015), each = nrow(dn_indfe_prov_coef_n) / length(seq(2001, 2015)))
dn_indfe_prov_coef_s$year <- rep(seq(2001, 2015), each = nrow(dn_indfe_prov_coef_s) / length(seq(2001, 2015)))

dn_indfe_prov_coef_n$group <- "North"
dn_indfe_prov_coef_s$group <- "South"
dn_indfe_prov_coef_ns <- rbind(dn_indfe_prov_coef_n, dn_indfe_prov_coef_s)

#################
# Province level#
#################

# OLS

dn_prov_ols_models <- list(
  feols(workerratio ~ log(tot_bmr_prov),
        dn01_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn02_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn03_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn04_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn05_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn06_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn07_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn08_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn09_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn10_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn11_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn12_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn13_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn14_prov,
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        dn15_prov,
        vcov = ~tinh)
)

dn_prov_ols_coef <- lapply(dn_prov_ols_models, tidy)
dn_prov_ols_coef <- do.call(rbind, dn_prov_ols_coef) %>% filter(term != "(Intercept)")
dn_prov_ols_coef$year <- rep(seq(2001, 2015), each = nrow(dn_prov_ols_coef) / length(seq(2001, 2015)))

# By South 

dn_prov_ols_s <- list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn01_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn02_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn03_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn04_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn05_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn06_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn07_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn08_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn09_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn10_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn11_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn12_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn13_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn14_prov, south == 1),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn15_prov, south == 1),
        vcov = ~tinh)
)

dn_prov_ols_n <- list(
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn01_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn02_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn03_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn04_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn05_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn06_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn07_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn08_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn09_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn10_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn11_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn12_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn13_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn14_prov, south == 0),
        vcov = ~tinh),
  feols(workerratio ~ log(tot_bmr_prov),
        subset(dn15_prov, south == 0),
        vcov = ~tinh)
)

dn_prov_ols_coef_s <- lapply(dn_prov_ols_s, tidy)
dn_prov_ols_coef_s <- do.call(rbind, dn_prov_ols_coef_s) %>% filter(term != "(Intercept)")
dn_prov_ols_coef_s$year <- rep(seq(2001, 2015), each = nrow(dn_prov_ols_coef_s) / length(seq(2001, 2015)))

dn_prov_ols_coef_n <- lapply(dn_prov_ols_n, tidy)
dn_prov_ols_coef_n <- do.call(rbind, dn_prov_ols_coef_n) %>% filter(term != "(Intercept)")
dn_prov_ols_coef_n$year <- rep(seq(2001, 2015), each = nrow(dn_prov_ols_coef_n) / length(seq(2001, 2015)))

dn_prov_ols_coef_n$group <- "North"
dn_prov_ols_coef_s$group <- "South"
dn_prov_ols_coef_ns <- rbind(dn_prov_ols_coef_n, dn_prov_ols_coef_s)
