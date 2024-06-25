bombs_prov <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda")

for (i in bombs_prov) {
  load(i)
}

phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

phc_sum <- function(i){
  
  i %>% 
    summarise(tot_f = sum(perwt[female == 1], na.rm = T),
              tot_m = sum(perwt[female == 0], na.rm = T),
              tot_mlf = sum(perwt[female == 0 & age > 15 & age < 65], na.rm = T),
              tot_flf = sum(perwt[female == 1 & age > 15 & age < 65], na.rm = T),
              f_work = sum(perwt[female == 1 & work == 1], na.rm = T),
              m_work = sum(perwt[female == 0 & work == 1], na.rm = T),
              widowed_f = sum(perwt[female == 1 & widowed == 1], na.rm = T),
              tot_bmr_prov = mean(tot_bmr_prov),
              tot_bmr_prov_ppn = mean(tot_bmr_prov_ppn),
              killed_tot_prov_ppn = mean(killed_tot_prov_ppn)) %>% 
    mutate(flfp = f_work/tot_flf,
           sexratio = tot_m/tot_f,
           widow_share = widowed_f/tot_f)
  
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

sum_dist09 <- phc09 %>% 
  group_by(geo2_vn2009) %>% 
  summarise(tot_f = sum(perwt[female == 1], na.rm = T),
            tot_m = sum(perwt[female == 0], na.rm = T),
            tot_mlf = sum(perwt[female == 0 & age > 15 & age < 65], na.rm = T),
            tot_flf = sum(perwt[female == 1 & age > 15 & age < 65], na.rm = T),
            f_work = sum(perwt[female == 1 & work == 1], na.rm = T),
            m_work = sum(perwt[female == 0 & work == 1], na.rm = T),
            widowed_f = sum(perwt[female == 1 & widowed == 1], na.rm = T),
            tot_bmr = mean(tot_bmr),
            tot_bmr_prov = mean(tot_bmr_prov),
            tot_bmr_prov_ppn = mean(tot_bmr_prov_ppn),
            killed_tot_prov_ppn = mean(killed_tot_prov_ppn)) %>% 
  mutate(flfp = f_work/tot_flf,
         sexratio = tot_m/tot_f,
         widow_share = widowed_f/tot_f) %>% 
  mutate(south = ifelse(geo2_vn2009 > 44000, 1, 0))

save(sum89, file = "sexratio_prov_89.Rda")
save(sum99, file = "sexratio_prov_99.Rda")
save(sum09, file = "sexratio_prov_09.Rda")

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

indgen <- bind_rows(indgen89, indgen99, indgen09) %>% 
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

## FLFP by age cohort 
agecohort_flfp_sum <- phc %>% 
  filter(female == 1 & work == 1) %>% 
  group_by(year, age_cohort) %>%
  summarise(work = sum(work * perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort_sum <- left_join(agecohort_sum, agecohort_flfp_sum, by = c("year", "age_cohort")) %>% 
  mutate(flfp = (work/n_female)*100) %>% 
  mutate(
    sex_ratio = round(sex_ratio, 2),
    flfp = round(flfp, 2)
  )  

# Sex ratio by age cohort, by province 
agecohort_sum_prov <- phc %>% 
  group_by(year, age_cohort, geo1_vn1989, geo1_vn1999, geo1_vn2009, female) %>%
  summarise(tot = sum(perwt)) %>% 
  filter(!is.na(age_cohort)) %>% 
  group_by(year, age_cohort) %>% 
  pivot_wider(names_from = female, values_from = tot) %>% 
  filter(!is.na(age_cohort)) %>% 
  rename(n_male = 6,
         n_female = 7) %>% 

  mutate(sex_ratio = (n_male / n_female) * 100)
agecohort_flfp_sum_prov <- phc %>% 
  filter(female == 1 & work == 1) %>% 
  group_by(year, age_cohort, geo1_vn1989, geo1_vn1999, geo1_vn2009) %>%
  summarise(work = sum(work * perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort_sum_prov <- left_join(agecohort_sum_prov, agecohort_flfp_sum_prov, by = c("year", "age_cohort", "geo1_vn1989", "geo1_vn1999", "geo1_vn2009")) %>% 
  mutate(flfp = work/n_female)

agecohort_sum_prov <- left_join(agecohort_sum_prov, provbombs_sum, by = c("year", "geo1_vn1989", "geo1_vn1999", "geo1_vn2009"))
