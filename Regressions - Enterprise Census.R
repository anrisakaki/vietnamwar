###############################
# Firm level FE - ALL WORKERS #
###############################

## By north/south 
dn_indfe_prov_n <- (list(
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + log(popdensity_02) | nganh_kd + lhdn,
        subset(dn02, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + log(popdensity_03) | nganh_kd + lhdn,
        subset(dn03, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio05 + log(popdensity_04)| nganh_kd + lhdn,
        subset(dn04, south == 0),
        vcov = ~tinh), 
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio05 + log(popdensity_05)| nganh_kd + lhdn,
        subset(dn05, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio05 + log(popdensity_06)| nganh_kd + lhdn,
        subset(dn06, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio07 + log(popdensity_07)| nganh_kd + lhdn,
        subset(dn07, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio08 + log(popdensity_08)| nganh_kd + lhdn,
        subset(dn08, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio09 + log(popdensity_09)| nganh_kd + lhdn,
        subset(dn09, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio10 + log(popdensity_10)| nganh_kd + lhdn,
        subset(dn10, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio11 + log(popdensity_11)| nganh_kd + lhdn,
        subset(dn11, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio12 + log(popdensity_12)| nganh_kd + lhdn,
        subset(dn12, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio13 + log(popdensity_13)| nganh_kd + lhdn,
        subset(dn13, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio14 + log(popdensity_14)| nganh_kd + lhdn,
        subset(dn14, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio15 + log(popdensity_15)| nganh_kd + lhdn,
        subset(dn15, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16)| nganh_kd + lhdn,
        subset(dn16, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio17 + log(popdensity_17)| nganh_kd + lhdn,
        subset(dn17, south == 0),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio18 + log(popdensity_18)| nganh_kd + lhdn,
        subset(dn18, south == 0),
        vcov = ~tinh)))

dn_indfe_prov_s <- (list(
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + log(popdensity_02) | nganh_kd + lhdn,
        subset(dn02, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sex_ratio + log(popdensity_03) | nganh_kd + lhdn,
        subset(dn03, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio05 + log(popdensity_04) | nganh_kd + lhdn,
        subset(dn04, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio05 + log(popdensity_05) | nganh_kd + lhdn,
        subset(dn05, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio05 + log(popdensity_06) | nganh_kd + lhdn,
        subset(dn06, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio07 + log(popdensity_07) | nganh_kd + lhdn,
        subset(dn07, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio08 + log(popdensity_08) | nganh_kd + lhdn,
        subset(dn08, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio09 + log(popdensity_09) | nganh_kd + lhdn,
        subset(dn09, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio10 + log(popdensity_10) | nganh_kd + lhdn,
        subset(dn10, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio11 + log(popdensity_11) | nganh_kd + lhdn,
        subset(dn11, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio12 + log(popdensity_12) | nganh_kd + lhdn,
        subset(dn12, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio13 + log(popdensity_13) | nganh_kd + lhdn,
        subset(dn13, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio14 + log(popdensity_14) | nganh_kd + lhdn,
        subset(dn14, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio15 + log(popdensity_15) | nganh_kd + lhdn,
        subset(dn15, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) | nganh_kd + lhdn,
        subset(dn16, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio17 + log(popdensity_17) | nganh_kd + lhdn,
        subset(dn17, south == 1),
        vcov = ~tinh),
  feols(tot_workerratio ~ log(tot_bmr_prov_ppn) + sexratio18 + log(popdensity_18) | nganh_kd + lhdn,
        subset(dn18, south == 1),
        vcov = ~tinh)))

dn_indfe_prov_coef_n <- lapply(dn_indfe_prov_n, tidy)
dn_indfe_prov_coef_s <- lapply(dn_indfe_prov_s, tidy)
dn_indfe_prov_coef_n <- do.call(rbind, dn_indfe_prov_coef_n) %>% filter(term == "log(tot_bmr_prov_ppn)")
dn_indfe_prov_coef_s <- do.call(rbind, dn_indfe_prov_coef_s) %>% filter(term == "log(tot_bmr_prov_ppn)")
dn_indfe_prov_coef_n$year <- rep(seq(2002, 2018), each = nrow(dn_indfe_prov_coef_n) / length(seq(2002, 2018)))
dn_indfe_prov_coef_s$year <- rep(seq(2002, 2018), each = nrow(dn_indfe_prov_coef_s) / length(seq(2002, 2018)))

dn_indfe_prov_coef_n$group <- "North"
dn_indfe_prov_coef_s$group <- "South"
dn_indfe_prov_coef_ns <- rbind(dn_indfe_prov_coef_n, dn_indfe_prov_coef_s)

#################################
# Casualties - FE - ALL WORKERS #
#################################

dn_indfe_prov_cas <- (list(
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sex_ratio + log(popdensity_02) | nganh_kd + lhdn,
        dn02,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sex_ratio + log(popdensity_03) | nganh_kd + lhdn,
        dn03,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio05 + log(popdensity_04) | nganh_kd + lhdn,
        dn04,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio05 + log(popdensity_05) | nganh_kd + lhdn,
        dn05,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio05 + log(popdensity_06) | nganh_kd + lhdn,
        dn06,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio07 + log(popdensity_07) | nganh_kd + lhdn,
        dn07,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio08 + log(popdensity_08) | nganh_kd + lhdn,
        dn08,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio09 + log(popdensity_09) | nganh_kd + lhdn,
        dn09,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio10 + log(popdensity_10) | nganh_kd + lhdn,
        dn10,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio11 + log(popdensity_11) | nganh_kd + lhdn,
        dn11,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio12 + log(popdensity_12) | nganh_kd + lhdn,
        dn12,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio13 + log(popdensity_13) | nganh_kd + lhdn,
        dn13,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio14 + log(popdensity_14) | nganh_kd + lhdn,
        dn14,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio15 + log(popdensity_15) | nganh_kd + lhdn,
        dn15,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio16 + log(popdensity_16) | nganh_kd + lhdn,
        dn16,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio17 + log(popdensity_17) | nganh_kd + lhdn,
        dn17,
        vcov = ~tinh),
  feols(tot_workerratio ~ log(killed_tot_prov_ppn) + sexratio18 + log(popdensity_18) | nganh_kd + lhdn,
        dn18,
        vcov = ~tinh)))

dn_indfe_prov_coef_cas <- lapply(dn_indfe_prov_cas, tidy) 
dn_indfe_prov_coef_cas <- do.call(rbind, dn_indfe_prov_coef_cas) %>% filter(term == "log(killed_tot_prov_ppn)")
dn_indfe_prov_coef_cas$year <- rep(seq(2002, 2018), each = nrow(dn_indfe_prov_coef_cas) / length(seq(2002, 2018)))

###################
# FEMALE DIRECTOR #
###################

etable(list(
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | lhdn + nganh_kd,
        subset(dn16, south == 0),
        vcov = ~tinh),
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | lhdn + nganh_kd,
             subset(dn16, south == 1),
             vcov = ~tinh),
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | lhdn + nganh_kd,
        subset(dn16, south == 0 & tinh != 1),
        vcov = ~tinh),
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | lhdn + nganh_kd,
        subset(dn16, south == 1 & tinh != 79 & tinh != 48))), tex = T)


etable(list(
  # Agriculture 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 0 & nganh_kd2 < 5),
        vcov = ~tinh),
  # Manufacturing 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 0 & manu == 1),
        vcov = ~tinh),
  # Construction 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 0 & nganh_kd2 >= 41 & nganh_kd2 < 45),
        vcov = ~tinh),
  # Wholesale and retail 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 0 & nganh_kd2 >= 45 & nganh_kd2 < 49),
        vcov = ~tinh),
  # Hospitality   
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 0 & nganh_kd2 >= 55 & nganh_kd2 < 58),
        vcov = ~tinh)
), tex = T)

etable(list(
  # Agriculture 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 1 & nganh_kd2 < 5),
        vcov = ~tinh),
  # Manufacturing 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 1 & manu == 1),
        vcov = ~tinh),
  # Construction 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 1 & nganh_kd2 >= 41 & nganh_kd2 < 45),
        vcov = ~tinh),
  # Wholesale and retail 
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 1 & nganh_kd2 >= 45 & nganh_kd2 < 49),
        vcov = ~tinh),
  # Hospitality   
  feols(female_dir ~ log(tot_bmr_prov_ppn) + sexratio16 + log(popdensity_16) + dist_nearest_base_prov + dist_nearest_hochi_prov + as.factor(dir_ethnicity) + dir_yob | nganh_kd + lhdn,
        subset(dn16, south == 1 & nganh_kd2 >= 55 & nganh_kd2 < 58),
        vcov = ~tinh)
), tex = T)
