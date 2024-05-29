##############################
# PROBABILITY OF WORK BY AGE #
##############################

# 2002 

## Female 

work_agexbmr_02_s <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss02, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~wt75,
                           vcov = ~tinh + age)) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
work_agexbmr_02_n <- tidy(feols(work ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss02, south == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~wt75,
                           vcov = ~tinh + age)) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))

selfagri_agexbmr_coef_02_s$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_02_s) / length(seq(16, 64)))
selfagri_agexbmr_coef_02_n$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_02_n) / length(seq(16, 64)))

selfagri_agexbmr_coef_02_n$group <- "North"
selfagri_agexbmr_coef_02_s$group <- "South"
selfagri_agexbmr_coef_02_ns <- bind_rows(selfagri_agexbmr_coef_02_n, selfagri_agexbmr_coef_02_s)
