############################
# PROBABILITY OF SELF AGRI #
############################

# 2002 

## Female 

selfagri_agexbmr_02_s <- feols(selfagri ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss02, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~wt75,
                           vcov = ~tinh + age)
selfagri_agexbmr_02_n <- feols(selfagri ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss02, south == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~wt75,
                           vcov = ~tinh + age)

selfagri_agexbmr_coef_02_s <- tidy(selfagri_agexbmr_02_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
selfagri_agexbmr_coef_02_n <- tidy(selfagri_agexbmr_02_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
selfagri_agexbmr_coef_02_s$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_02_s) / length(seq(16, 64)))
selfagri_agexbmr_coef_02_n$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_02_n) / length(seq(16, 64)))

selfagri_agexbmr_coef_02_n$group <- "North"
selfagri_agexbmr_coef_02_s$group <- "South"
selfagri_agexbmr_coef_02_ns <- bind_rows(selfagri_agexbmr_coef_02_n, selfagri_agexbmr_coef_02_s)

# 2004 

selfagri_agexbmr_04_s <- feols(selfagri ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss04, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~wt45,
                           vcov = ~tinh + age)
selfagri_agexbmr_04_n <- feols(selfagri ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss04, south == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~wt45,
                           vcov = ~tinh + age)

selfagri_agexbmr_coef_04_s <- tidy(selfagri_agexbmr_04_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
selfagri_agexbmr_coef_04_n <- tidy(selfagri_agexbmr_04_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
selfagri_agexbmr_coef_04_s$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_04_s) / length(seq(16, 64)))
selfagri_agexbmr_coef_04_n$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_04_n) / length(seq(16, 64)))

selfagri_agexbmr_coef_04_n$group <- "North"
selfagri_agexbmr_coef_04_s$group <- "South"
selfagri_agexbmr_coef_04_ns <- bind_rows(selfagri_agexbmr_coef_04_n, selfagri_agexbmr_coef_04_s)

# 2006 

selfagri_agexbmr_06_s <- feols(selfagri ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss06, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~wt45,
                           vcov = ~tinh + age)
selfagri_agexbmr_06_n <- feols(selfagri ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss06, south == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~wt45,
                           vcov = ~tinh + age)

selfagri_agexbmr_coef_06_s <- tidy(selfagri_agexbmr_06_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
selfagri_agexbmr_coef_06_n <- tidy(selfagri_agexbmr_06_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
selfagri_agexbmr_coef_06_s$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_06_s) / length(seq(16, 64)))
selfagri_agexbmr_coef_06_n$age <- rep(seq(16, 64), each = nrow(selfagri_agexbmr_coef_06_n) / length(seq(16, 64)))

selfagri_agexbmr_coef_06_n$group <- "North"
selfagri_agexbmr_coef_06_s$group <- "South"
selfagri_agexbmr_coef_06_ns <- bind_rows(selfagri_agexbmr_coef_06_n, selfagri_agexbmr_coef_06_s)

##############################
# PROBABILITY OF MANUFACTURE #
##############################

manu_agexbmr_02_s <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                               subset(vhlss02, south == 1 & female == 1 & age < 65 & age > 15),
                               weights = ~wt75,
                               vcov = ~tinh + age)
manu_agexbmr_02_n <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                               subset(vhlss02, south == 0 & female == 1 & age < 65 & age > 15),
                               weights = ~wt75,
                               vcov = ~tinh + age)

manu_agexbmr_coef_02_s <- tidy(manu_agexbmr_02_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_02_n <- tidy(manu_agexbmr_02_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_02_s$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_02_s) / length(seq(16, 64)))
manu_agexbmr_coef_02_n$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_02_n) / length(seq(16, 64)))

manu_agexbmr_coef_02_n$group <- "North"
manu_agexbmr_coef_02_s$group <- "South"
manu_agexbmr_coef_02_ns <- bind_rows(manu_agexbmr_coef_02_n, manu_agexbmr_coef_02_s)

# 2004 

manu_agexbmr_04_s <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                               subset(vhlss04, south == 1 & female == 1 & age < 65 & age > 15),
                               weights = ~wt45,
                               vcov = ~tinh + age)
manu_agexbmr_04_n <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                               subset(vhlss04, south == 0 & female == 1 & age < 65 & age > 15),
                               weights = ~wt45,
                               vcov = ~tinh + age)

manu_agexbmr_coef_04_s <- tidy(manu_agexbmr_04_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_04_n <- tidy(manu_agexbmr_04_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_04_s$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_04_s) / length(seq(16, 64)))
manu_agexbmr_coef_04_n$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_04_n) / length(seq(16, 64)))

manu_agexbmr_coef_04_n$group <- "North"
manu_agexbmr_coef_04_s$group <- "South"
manu_agexbmr_coef_04_ns <- bind_rows(manu_agexbmr_coef_04_n, manu_agexbmr_coef_04_s)

# 2006 

manu_agexbmr_06_s <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                               subset(vhlss06, south == 1 & female == 1 & age < 65 & age > 15),
                               weights = ~wt45,
                               vcov = ~tinh + age)
manu_agexbmr_06_n <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                               subset(vhlss06, south == 0 & female == 1 & age < 65 & age > 15),
                               weights = ~wt45,
                               vcov = ~tinh + age)

manu_agexbmr_coef_06_s <- tidy(manu_agexbmr_06_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_06_n <- tidy(manu_agexbmr_06_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_06_s$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_06_s) / length(seq(16, 64)))
manu_agexbmr_coef_06_n$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_06_n) / length(seq(16, 64)))

manu_agexbmr_coef_06_n$group <- "North"
manu_agexbmr_coef_06_s$group <- "South"
manu_agexbmr_coef_06_ns <- bind_rows(manu_agexbmr_coef_06_n, manu_agexbmr_coef_06_s)

# 2008

manu_agexbmr_08_s <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss08, south == 1 & female == 1 & age < 65 & age > 15),
                           weights = ~wt9,
                           vcov = ~tinh + age)
manu_agexbmr_08_n <- feols(manu ~ i(as.factor(age), log(tot_bmr_prov_ppn)) + educ + dist_nearest_base_prov + dist_nearest_hochi_prov | urban + age + minority,
                           subset(vhlss08, south == 0 & female == 1 & age < 65 & age > 15),
                           weights = ~wt9,
                           vcov = ~tinh + age)

manu_agexbmr_coef_08_s <- tidy(manu_agexbmr_08_s) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_08_n <- tidy(manu_agexbmr_08_n) %>% filter(grepl("log\\(tot_bmr_prov_ppn\\)", term))
manu_agexbmr_coef_08_s$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_08_s) / length(seq(16, 64)))
manu_agexbmr_coef_08_n$age <- rep(seq(16, 64), each = nrow(manu_agexbmr_coef_08_n) / length(seq(16, 64)))

manu_agexbmr_coef_08_n$group <- "North"
manu_agexbmr_coef_08_s$group <- "South"
manu_agexbmr_coef_08_ns <- bind_rows(manu_agexbmr_coef_08_n, manu_agexbmr_coef_08_s)
