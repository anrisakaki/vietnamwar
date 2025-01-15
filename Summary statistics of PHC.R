phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda", "phc19.Rda", "phc_all.Rda")

for (i in phc) {
  load(i)
}

phc_sum <- function(i){
  
  i %>%
    filter(age > 14 & age < 65) %>%
    summarise(
      n = sum(perwt, na.rm = TRUE),
      tot_f = sum(perwt[female == 1], na.rm = TRUE),
      tot_m = sum(perwt[female == 0], na.rm = TRUE),
      f_work = sum(perwt[female == 1 & work == 1], na.rm = TRUE),
      m_work = sum(perwt[female == 0 & work == 1], na.rm = TRUE),
      widowed_f = sum(perwt[female == 1 & widowed == 1], na.rm = TRUE),
      tot_f_migrants = sum(perwt[female == 1 & migration == 1], na.rm = TRUE),
      tot_m_migrants = sum(perwt[female == 0 & migration == 1], na.rm = TRUE),
      minority_n = sum(perwt[minority == 1], na.rm = TRUE),
      disabled_n = sum(perwt[disabled == 1], na.rm = TRUE),
      agri_f_n = sum(perwt[agri == 1 & female == 1], na.rm = TRUE),
      manu_f_n = sum(perwt[manu == 1 & female == 1], na.rm = TRUE),
      n_primary = sum(perwt[edattain == 2], na.rm = TRUE),
      n_secondary = sum(perwt[edattain == 3], na.rm = TRUE),
      n_uni = sum(perwt[edattain == 4], na.rm = TRUE),
      tot_bmr_std = mean(tot_bmr_prov_std, na.rm = TRUE),
      dist_hochi = mean(dist_nearest_hochi_prov, na.rm = TRUE)
    ) %>%
    mutate(
      flfp = f_work / tot_f,
      sexratio = tot_m / tot_f,
      widow_share = widowed_f / tot_f,
      f_migrant_share = tot_f_migrants / tot_f,
      migrant_share = (tot_f_migrants + tot_m_migrants) / n,
      minority_share = minority_n / n,
      disabled_share = disabled_n / n,
      agri_f_share = agri_f_n / f_work,
      manu_f_share = manu_f_n / f_work,
      primary_share = n_primary / tot_f,
      secdonary_share = n_secondary / tot_f,
      uni_share = n_uni / tot_f
    )
}

dist_phc_sum <- function(i) {
  i %>%
    filter(age > 14 & age < 65) %>%
    summarise(
      n = sum(perwt, na.rm = TRUE),
      tot_f = sum(perwt[female == 1], na.rm = TRUE),
      tot_m = sum(perwt[female == 0], na.rm = TRUE),
      f_work = sum(perwt[female == 1 & work == 1], na.rm = TRUE),
      m_work = sum(perwt[female == 0 & work == 1], na.rm = TRUE),
      widowed_f = sum(perwt[female == 1 & widowed == 1], na.rm = TRUE),
      tot_f_migrants = sum(perwt[female == 1 & migration == 1], na.rm = TRUE),
      tot_m_migrants = sum(perwt[female == 0 & migration == 1], na.rm = TRUE),
      minority_n = sum(perwt[minority == 1], na.rm = TRUE),
      disabled_n = sum(perwt[disabled == 1], na.rm = TRUE),
      agri_f_n = sum(perwt[agri == 1 & female == 1], na.rm = TRUE),
      manu_f_n = sum(perwt[manu == 1 & female == 1], na.rm = TRUE),
      n_primary = sum(perwt[edattain == 2], na.rm = TRUE),
      n_secondary = sum(perwt[edattain == 3], na.rm = TRUE),
      n_uni = sum(perwt[edattain == 4], na.rm = TRUE),
      tot_bmr_std = mean(tot_bmr_std, na.rm = TRUE),
      dist_hochi = mean(dist_nearest_hochi_dist, na.rm = TRUE),
      popdensgeo2 = mean(popdensgeo2, na.rm = TRUE),
      geo1_vn2009 = mean(geo1_vn2009, na.rm = TRUE),
      geo1_vn2019 = mean(geo1_vn2019, na.rm = TRUE)
    ) %>%
    mutate(
      flfp = f_work / tot_f,
      sexratio = tot_m / tot_f,
      widow_share = widowed_f / tot_f,
      f_migrant_share = tot_f_migrants / tot_f,
      migrant_share = (tot_f_migrants + tot_m_migrants) / n,
      minority_share = minority_n / n,
      disabled_share = disabled_n / n,
      agri_f_share = agri_f_n / f_work,
      manu_f_share = manu_f_n / f_work,
      primary_share = n_primary / tot_f,
      secondary_share = n_secondary / tot_f,
      uni_share = n_uni / tot_f
    ) %>%
    mutate(south = ifelse(geo2_vn > 704044457, 1, 0))
}

# Calculating sex ratio by age, and by north and south 

sum89 <- phc89 %>% 
  group_by(geo1_vn1989) %>% 
  phc_sum() %>% 
  mutate(south = ifelse(geo1_vn1989 > 26, 1, 0))

sum99 <- phc99 %>% 
  group_by(geo1_vn1999) %>% 
  phc_sum() %>% 
  mutate(south = ifelse(geo1_vn1999 > 405, 1, 0)) 

sum09 <- phc09 %>% 
  group_by(geo1_vn2009) %>% 
  phc_sum() %>% 
  mutate(south = ifelse(geo1_vn2009 > 44, 1, 0))

sum19 <- phc19 %>% 
  group_by(geo1_vn2019) %>% 
  phc_sum() %>% 
  mutate(south = ifelse(geo1_vn2019 > 44, 1, 0))

sum_dist09 <- phc09 %>% 
  group_by(geo2_vn, geo2_vn2009) %>% 
  dist_phc_sum()

sum_dist19 <- phc19 %>% 
  group_by(geo2_vn, geo2_vn2019) %>% 
  dist_phc_sum()

save(sum89, file = "sum89.Rda")
save(sum99, file = "sum99.Rda")
save(sum09, file = "sum09.Rda")
save(sum_dist09, file = "sum_dist09.Rda")
save(sum_dist19, file = "sum_dist19.Rda")

sum_phc <- phc_all %>% 
  group_by(year, south) %>% 
  summarise(n = sum(perwt),
            tot_f = sum(perwt[female == 1], na.rm = T),
            tot_m = sum(perwt[female == 0], na.rm = T),
            tot_mlf = sum(perwt[female == 0 & age > 15 & age < 65], na.rm = T),
            tot_flf = sum(perwt[female == 1 & age > 15 & age < 65], na.rm = T),
            f_work = sum(perwt[female == 1 & work == 1], na.rm = T),
            m_work = sum(perwt[female == 0 & work == 1], na.rm = T)) %>% 
  mutate(flfp = f_work/tot_flf,
         mlfp = m_work/tot_mlf)

# Calculating male the female ratio in each industry 

female_n_fn <- function(i){
  i %>% 
    filter(work == 1) %>% 
    group_by(female, south) %>% 
    count(female, south, wt = perwt) %>% 
    pivot_wider(names_from = female, values_from = n) %>% 
    rename(total_m_workers = 2,
           total_f_workers = 3)  
}

indgen_m_fn <- function(i){
  i %>% 
    filter(female == 0) %>% 
    group_by(south, indgen) %>% 
    summarise(N_m = sum(perwt)) %>% 
    filter(indgen > 0)
}

indgen_f_fn <- function(i){
  i %>% 
    filter(female == 1) %>% 
    group_by(south, indgen) %>% 
    summarise(N_f = sum(perwt)) %>% 
    filter(indgen > 0)
}

femalen <- phc_all %>% 
  filter(work == 1) %>% 
  group_by(year, south, female) %>%
  summarise(count = sum(perwt)) %>% 
  pivot_wider(names_from = female, values_from = count) %>% 
  rename(total_m = 3,
         total_f = 4)

indgen89_m <- phc89 %>% 
  indgen_m_fn()
indgen89_f <- phc89 %>% 
  indgen_f_fn()
indgen89 <- merge(indgen89_m, indgen89_f, by = c("south", "indgen")) %>% mutate(year = 1989)

indgen99_m <- phc99 %>% 
  indgen_m_fn()
indgen99_f <- phc99 %>% 
  indgen_f_fn()
indgen99 <- merge(indgen99_m, indgen99_f, by = c("south", "indgen")) %>% mutate(year = 1999)

indgen09_m <- phc09 %>% 
  indgen_m_fn()
indgen09_f <- phc09 %>% 
  indgen_f_fn()
indgen09 <- merge(indgen09_m, indgen09_f, by = c("south", "indgen")) %>% mutate(year = 2009)

indgen19_m <- phc19 %>% 
  indgen_m_fn()
indgen19_f <- phc19 %>% 
  indgen_f_fn()
indgen19 <- merge(indgen19_m, indgen19_f, by = c("south", "indgen")) %>% mutate(year = 2019)

indgen <- bind_rows(indgen89, indgen99, indgen09, indgen19) %>% 
  left_join(femalen, by = c("year", "south")) %>% 
  mutate(workerratio = N_m/N_f,
         Industry = case_when(indgen == 10 ~ 'Agriculture',
                              indgen == 20 ~ 'Mining and extraction',
                              indgen == 30 ~ 'Manufacturing',
                              indgen == 40 ~ 'Electricity, gas, water and waste management',
                              indgen == 50 ~ 'Construction',
                              indgen == 60 ~ 'Wholesale and retail trade',
                              indgen == 70 ~ 'Hotels and restaurants',
                              indgen == 80 ~ 'Transportation',
                              indgen == 90 ~ 'Financial services and insurance',
                              indgen == 100 ~ 'Public administration and defense',
                              indgen == 110 ~ 'Services',
                              indgen == 111 ~ 'Business services and real estate',
                              indgen == 112 ~ 'Education',
                              indgen == 113 ~ 'Health and social work',
                              indgen == 114 ~ 'Other services',
                              indgen == 120 ~ 'Private household services',
                              indgen == 130 ~ 'Other industry',
                              TRUE ~ NA_character_)) %>%
  mutate(f_comp = round((N_f/total_f)*100, 2),
         m_comp = round((N_m/total_m)*100, 2))

indgen_s <- indgen %>% filter(south == 1)
indgen_n <- indgen %>% filter(south == 0)

# Calculating the sex ratio and FLFP by age cohort in 1989, 1999 and 2009

agecohort_sum <- phc %>% 
  group_by(year, age_cohort, female) %>%
  summarise(tot = sum(perwt)) %>% 
  group_by(year, age_cohort) %>% 
  pivot_wider(names_from = female, values_from = tot) %>% 
  filter(!is.na(age_cohort)) %>% 
  rename(n_male = 3,
         n_female = 4) %>% 
  mutate(sex_ratio = (n_male / n_female) * 100,
         sex_ratio = round(sex_ratio, 2),
         prov_ppn = n_male + n_female,
         group89 = "Born before war",
         group99 = case_when(year == 1999 & age_cohort %in% c("10-14", "15-19", "20-24") ~ "Born after war",
                             TRUE ~ "Born before war"),
         group09 = case_when(year == 2009 & age_cohort %in% c("10-14", "15-19", "20-24", "25-29", "30-34") ~ "Born after war",
                             TRUE ~ "Born before war"))

# Summary statistics of migration patterns 

phc_all %>% 
  group_by(year) %>% 
  summarise(n = sum(perwt),
            # Share of people in same major, same minor admin unit 
            non_mig = sum(perwt[migrate5 == 11], na.rm = T),
            
            # Share of people in same major, different minor admin unit
            mig_district = sum(perwt[migrate5 == 12], na.rm = T),
            
            # Share of people in different major admin unit 
            mig_province = sum(perwt[migrate5 == 20], na.rm = T)) %>% 
  mutate(share1 = mig_district/n,
         share2 = mig_province/n,
         share3 = non_mig/n)

# Summary statistics of migrants 

migrant_sum_survey <- function(i) {
    # Filter the survey design object for females aged 15–64
    j <- subset(i, female == 1 & age > 14 & age < 65)
    
    j_age <- subset(j, !is.na(age))
    j_yrschool <- subset(j, !is.na(yrschool))
    j_work <- subset(j, !is.na(work))
    
    results <- list(
      # Age
      mean_age_migrant = svymean(~age, subset(j_age, migration == 1))[[1]],
      mean_age_nonmigrant = svymean(~age, subset(j_age, migration == 0))[[1]],
      sd_age_migrant = sqrt(svyvar(~age, subset(j_age, migration == 1))[[1]]),
      sd_age_nonmigrant = sqrt(svyvar(~age, subset(j_age, migration == 0))[[1]]),
      diff_mean_age = svymean(~age, subset(j_age, migration == 1))[[1]] -
        svymean(~age, subset(j_age, migration == 0))[[1]],
      
      # Years of Schooling
      mean_yrschool_migrant = svymean(~yrschool, subset(j_yrschool, migration == 1))[[1]],
      mean_yrschool_nonmigrant = svymean(~yrschool, subset(j_yrschool, migration == 0))[[1]],
      sd_yrschool_migrant = sqrt(svyvar(~yrschool, subset(j_yrschool, migration == 1))[[1]]),
      sd_yrschool_nonmigrant = sqrt(svyvar(~yrschool, subset(j_yrschool, migration == 0))[[1]]),
      diff_mean_yrschool = svymean(~yrschool, subset(j_yrschool, migration == 1))[[1]] -
        svymean(~yrschool, subset(j_yrschool, migration == 0))[[1]],
      
      # Work
      mean_work_migrant = svymean(~work, subset(j_work, migration == 1))[[1]],
      mean_work_nonmigrant = svymean(~work, subset(j_work, migration == 0))[[1]],
      sd_work_migrant = sqrt(svyvar(~work, subset(j_work, migration == 1))[[1]]),
      sd_work_nonmigrant = sqrt(svyvar(~work, subset(j_work, migration == 0))[[1]]),
      diff_mean_work = svymean(~work, subset(j_work, migration == 1))[[1]] -
        svymean(~work, subset(j_work, migration == 0))[[1]]
    )
    
    return(as.data.frame(results))
  }

svy89 <- svydesign(ids = ~1, weights = ~perwt, data = phc89)
svy99 <- svydesign(ids = ~1, weights = ~perwt, data = phc99)
svy09 <- svydesign(ids = ~1, weights = ~perwt, data = phc09)
svy19 <- svydesign(ids = ~1, weights = ~perwt, data = phc19)

summary_1989 <- migrant_sum_survey(svy89)
summary_1999 <- migrant_sum_survey(svy99)
summary_2009 <- migrant_sum_survey(svy09)
summary_2019 <- migrant_sum_survey(svy19)

feols(age ~ migration, subset(phc89, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(yrschool ~ migration, subset(phc89, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(work ~ migration, subset(phc89, female == 1 & age > 14 & age < 65), weights = ~perwt)

feols(age ~ migration, subset(phc99, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(yrschool ~ migration, subset(phc99, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(work ~ migration, subset(phc99, female == 1 & age > 14 & age < 65), weights = ~perwt)

feols(age ~ migration, subset(phc09, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(yrschool ~ migration, subset(phc09, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(work ~ migration, subset(phc09, female == 1 & age > 14 & age < 65), weights = ~perwt)

feols(age ~ migration, subset(phc19, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(yrschool ~ migration, subset(phc19, female == 1 & age > 14 & age < 65), weights = ~perwt)
feols(work ~ migration, subset(phc19, female == 1 & age > 14 & age < 65), weights = ~perwt)
