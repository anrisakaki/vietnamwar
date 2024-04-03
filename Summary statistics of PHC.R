bombs_prov <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda")

for (i in bombs_prov) {
  load(i)
}

phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

# Calculating sex ratio by age, and by north and south 

bc_sexratio89 <- phc89 %>% 
  filter(!is.na(age)) %>% 
  group_by(age, female) %>% 
  summarise(total = sum(perwt)) %>% 
  pivot_wider(names_from = female, values_from = total) %>% 
  rename(m_total = 2,
         f_total = 3) %>% 
  mutate(sex_ratio = m_total/f_total)

bc_sexratio89_s <- phc89 %>% 
  filter(!is.na(age) & geo1_vn1989 > 26) %>% 
  group_by(age, female) %>% 
  summarise(total = sum(perwt)) %>% 
  pivot_wider(names_from = female, values_from = total) %>% 
  rename(m_south = 2,
         f_south = 3) %>% 
  mutate(sex_ratio_south = m_south/f_south)

bc_sexratio89_n <- phc89 %>% 
  filter(!is.na(age) & geo1_vn1989 <= 26) %>% 
  group_by(age, female) %>% 
  summarise(total = sum(perwt)) %>% 
  pivot_wider(names_from = female, values_from = total) %>% 
  rename(m_north = 2,
         f_north = 3) %>% 
  mutate(sex_ratio_north = m_north/f_north)

bc_sexratio89 <- list(bc_sexratio89, bc_sexratio89_n, bc_sexratio89_s) %>% 
  reduce(full_join, by = "age") %>% 
  select(age, sex_ratio, sex_ratio_north, sex_ratio_south)

# Calculating the sex ratio and LFP of men and women by age cohort in 1989 
prov_89_f <- phc89 %>% 
  filter(female == 1 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(total_f = sum(perwt),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt))

prov_89_m <- phc89 %>% 
  filter(female == 0 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(total_m = sum(perwt),
            work_m = sum(work * perwt))

prov89_age3064_f <- phc89 %>% 
  filter(age > 29 & age < 65 & female == 1) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(age3064_f = sum(perwt),
            age3064_widowed_f = sum(widowed * perwt) / sum(perwt),
            age3064_work_f = sum(work * perwt))

prov89_age3064_m <- phc89 %>% 
  filter(age > 29 & age < 65 & female == 0) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(age3064_m = sum(perwt),
            age3064_work_m = sum(work * perwt))

sexratio_prov_89 <- list(prov_89_f, prov_89_m, prov89_age3064_f, prov89_age3064_m, bombs_province89) %>% 
  reduce(full_join, by = "geo1_vn1989") %>% 
  mutate(sexratio = total_m/total_f,
         sexratio3064 = age3064_m/age3064_f,
         workratio = (work_m/work_f),
         workratio3064 = age3064_work_m/age3064_work_f)

# Calculating the sex ratio and LFP of men and women by age cohort in 1999 
prov_99_f <- phc99 %>% 
  filter(female == 1 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1999) %>% 
  summarise(total_f = sum(perwt),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt))

prov_99_m <- phc99 %>% 
  filter(female == 0 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1999) %>% 
  summarise(total_m = sum(perwt),
            work_m = sum(work * perwt))

prov99_age4064_f <- phc99 %>% 
  filter(age > 39 & age < 65 & female == 1) %>% 
  group_by(geo1_vn1999) %>% 
  summarise(age4064_f = sum(perwt),
            age4064_widowed_f = sum(widowed * perwt) / sum(perwt),
            age4064_work_f = sum(work * perwt))

prov99_age4064_m <- phc99 %>% 
  filter(age > 39 & age < 65 & female == 0) %>% 
  group_by(geo1_vn1999) %>% 
  summarise(age4064_m = sum(perwt),
            age4064_work_m = sum(work * perwt))

sexratio_prov_99 <- list(prov_99_f, prov_99_m, prov99_age4064_f, prov99_age4064_m, bombs_province99) %>% 
  reduce(full_join, by = "geo1_vn1999") %>% 
  mutate(sexratio = total_m/total_f,
         sexratio4064 = age4064_m/age4064_f,
         workratio = (work_m/work_f),
         workratio4064 = age4064_work_m/age4064_work_f)

# Calculating the sex ratio and LFP of men and women by age cohort in 2009
prov_09_f <- phc09 %>% 
  filter(female == 1 & age > 15 & age < 65) %>% 
  group_by(geo1_vn2009) %>% 
  summarise(total_f = sum(perwt),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt))

prov_09_m <- phc09 %>% 
  filter(female == 0 & age > 15 & age < 65) %>% 
  group_by(geo1_vn2009) %>% 
  summarise(total_m = sum(perwt),
            work_m = sum(work * perwt))

prov09_age5064_f <- phc09 %>% 
  filter(age > 49 & age < 65 & female == 1) %>% 
  group_by(geo1_vn2009) %>% 
  summarise(age5064_f = sum(perwt),
            age5064_widowed_f = sum(widowed * perwt) / sum(perwt),
            age5064_work_f = sum(work * perwt))

prov09_age5064_m <- phc09 %>% 
  filter(age > 49 & age < 65 & female == 0) %>% 
  group_by(geo1_vn2009) %>% 
  summarise(age5064_m = sum(perwt),
            age5064_work_m = sum(work * perwt))

sexratio_prov_09 <- list(prov_09_f, prov_09_m, prov09_age5064_f, prov09_age5064_m, bombs_province09) %>% 
  reduce(full_join, by = "geo1_vn2009") %>% 
  mutate(sexratio = total_m/total_f,
         sexratio5064 = age5064_m/age5064_f,
         workratio = (work_m/work_f),
         workratio5064 = age5064_work_m/age5064_work_f)

# Saving data

save(sexratio_prov_89, file = "sexratio_prov_89.Rda")
save(sexratio_prov_99, file = "sexratio_prov_99.Rda")
save(sexratio_prov_09, file = "sexratio_prov_09.Rda")

# Calculating male the female ratio in each occupation

occ_m <- phc %>% 
  filter(female == 0 & !is.na(occisco)) %>% 
  select(year, occisco, perwt) %>% 
  group_by(year, occisco) %>% 
  count(occisco, year, wt = perwt) %>% 
  rename(m_workers = n)

occ_f <- phc %>% 
  filter(female == 1 & !is.na(occisco)) %>% 
  select(year, occisco, perwt) %>% 
  group_by(year, occisco) %>% 
  count(occisco, year, wt = perwt) %>% 
  rename(f_workers = n)

occisco_sum <- merge(occ_m, occ_f, by = c("year", "occisco")) %>% 
  mutate(workerratio = m_workers/f_workers,
         Occupation = case_when(occisco == 1 ~ 'Legislators, senior officials & managers',
                                occisco == 2 ~ 'Professionals',
                                occisco == 3 ~ 'Technicians & associate professionals',
                                occisco == 4 ~ 'Clerks',
                                occisco == 5 ~ 'Service workers',
                                occisco == 6 ~ 'Skilled agricultural & fishery workers',
                                occisco == 7 ~ 'Crafts & related trades workers',
                                occisco == 8 ~ 'Plant & machine operators',
                                occisco == 9 ~ 'Elementary occupations',
                                occisco == 10 ~ 'Armed forces',
                                occisco == 11 ~ 'Other',
                                TRUE ~ NA_character_))

## By province 

occisco_prov_m <- phc %>% 
  filter(year == 1999 & female == 0 & !is.na(occisco)) %>% 
  group_by(geo1_vn1999, occisco) %>% 
  count(occisco, geo1_vn1999, wt = perwt) %>% 
  rename(m_workers = n)

occisco_prov_f <- phc %>% 
  filter(year == 1999 & female == 1 & !is.na(occisco)) %>% 
  group_by(geo1_vn1999, occisco) %>% 
  count(occisco, geo1_vn1999, wt = perwt) %>% 
  rename(f_workers = n)

occisco_prov_sum <- merge(occisco_prov_m, occisco_prov_f, by = c("geo1_vn1999", "occisco")) %>% 
  mutate(workerratio = m_workers/f_workers,
         Occupation = case_when(occisco == 1 ~ 'Legislators, senior officials & managers',
                                occisco == 2 ~ 'Professionals',
                                occisco == 3 ~ 'Technicians & associate professionals',
                                occisco == 4 ~ 'Clerks',
                                occisco == 5 ~ 'Service workers',
                                occisco == 6 ~ 'Skilled agricultural & fishery workers',
                                occisco == 7 ~ 'Crafts & related trades workers',
                                occisco == 8 ~ 'Plant & machine operators',
                                occisco == 9 ~ 'Elementary occupations',
                                occisco == 10 ~ 'Armed forces',
                                occisco == 11 ~ 'Other',
                                TRUE ~ NA_character_))
occisco_prov_sum <- left_join(occisco_prov_sum, bombs_province99, by = "geo1_vn1999")

## By north/south 

occ_ratio_n <- phc %>% 
  filter(geo1_vn1989 <= 26 | geo1_vn1999 <= 408 | geo1_vn2009 <= 44) %>% 
  group_by(year, female, occisco) %>% 
  count(occisco, female, wt = perwt) %>% 
  pivot_wider(names_from = female, values_from = n) %>% 
  rename(north_m = 3,
         north_f = 4) %>% 
  mutate(workerratio_n = north_m/north_f) %>% 
  filter(!is.na(occisco))

occ_ratio_s <- phc %>% 
  filter(geo1_vn1989 > 26 | geo1_vn1999 > 408 | geo1_vn2009 > 44) %>% 
  group_by(year, female, occisco) %>% 
  count(occisco, female, wt = perwt) %>% 
  pivot_wider(names_from = female, values_from = n) %>% 
  rename(south_m = 3,
         south_f = 4) %>% 
  mutate(workerratio_s = south_m/south_f) %>% 
  filter(!is.na(occisco))

occisco_ns <- merge(occ_ratio_n, occ_ratio_s, by = c("year", "occisco")) %>% 
  mutate(Occupation = case_when(occisco == 1 ~ 'Legislators, senior officials & managers',
                                occisco == 2 ~ 'Professionals',
                                occisco == 3 ~ 'Technicians & associate professionals',
                                occisco == 4 ~ 'Clerks',
                                occisco == 5 ~ 'Service workers',
                                occisco == 6 ~ 'Skilled agricultural & fishery workers',
                                occisco == 7 ~ 'Crafts & related trades workers',
                                occisco == 8 ~ 'Plant & machine operators',
                                occisco == 9 ~ 'Elementary occupations',
                                occisco == 10 ~ 'Armed forces',
                                occisco == 11 ~ 'Other',
                                TRUE ~ NA_character_)) %>% 
  filter(occisco < 98) %>% 
  select(year, Occupation, workerratio_n, workerratio_s)

# Calculating male the female ratio in each industry 

female <- phc %>%
  filter(labforce == 2) %>% 
  group_by(year, female) %>% 
  count(female, year, wt = perwt) %>% 
  pivot_wider(names_from = female, values_from = n) %>% 
  rename(total_m_workers = 2,
         total_f_workers = 3)
  
ind_m <- phc %>% 
  filter(female == 0 & !is.na(indgen) & indgen > 0) %>% 
  select(year, indgen, perwt) %>% 
  group_by(year, indgen) %>% 
  count(indgen, year, wt = perwt) %>% 
  rename(m_workers = n)

ind_f <- phc %>% 
  filter(female == 1 & !is.na(indgen) & indgen > 0) %>% 
  select(year, indgen, perwt) %>% 
  group_by(year, indgen) %>% 
  count(indgen, year, wt = perwt) %>% 
  rename(f_workers = n)

indgen_sum <- merge(ind_m, ind_f, by = c("year", "indgen")) %>% 
  mutate(workerratio = m_workers/f_workers,
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
                              TRUE ~ NA_character_))

indgen_sum <- merge(indgen_sum, female, by = "year") %>% 
  mutate(f_comp = round((f_workers/total_f_workers)*100, 2),
         m_comp = round((m_workers/total_m_workers)*100, 2))

## by province 

ind_m_prov <- phc %>% 
  filter(year == 1989, female == 0 & !is.na(indgen) & indgen > 0) %>% 
  select(geo1_vn1989, indgen, perwt) %>% 
  group_by(geo1_vn1989, indgen) %>% 
  count(indgen, geo1_vn1989, wt = perwt) %>% 
  rename(m_workers = n)

ind_f_prov <- phc %>% 
  filter(year == 1989, female == 1 & !is.na(indgen) & indgen > 0) %>% 
  select(geo1_vn1989, indgen, perwt) %>% 
  group_by(geo1_vn1989, indgen) %>% 
  count(indgen, geo1_vn1989, wt = perwt) %>% 
  rename(f_workers = n)

indgen_prov_sum <- merge(ind_m_prov, ind_f_prov, by = c("geo1_vn1989", "indgen")) %>% 
  mutate(workerratio = m_workers/f_workers,
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
                              TRUE ~ NA_character_))

indgen_prov_sum <- left_join(indgen_prov_sum, bombs_province89, by = "geo1_vn1989")

## By north/south 
ind_ratio_ns 
ind_ratio_n <- phc %>% 
  filter(geo1_vn1989 <= 26 | geo1_vn1999 <= 408 | geo1_vn2009 <= 44) %>% 
  group_by(year, female, indgen) %>% 
  count(indgen, female, wt = perwt) %>% 
  pivot_wider(names_from = female, values_from = n) %>% 
  rename(north_m = 3,
         north_f = 4) %>% 
  mutate(workerratio_n = north_m/north_f) %>% 
  filter(indgen > 0 & !is.na(indgen))

ind_ratio_s <- phc %>% 
  filter(geo1_vn1989 > 26 | geo1_vn1999 > 408 | geo1_vn2009 > 44) %>% 
  group_by(year, female, indgen) %>% 
  count(indgen, female, wt = perwt) %>% 
  pivot_wider(names_from = female, values_from = n) %>% 
  rename(south_m = 3,
         south_f = 4) %>% 
  mutate(workerratio_s = south_m/south_f) %>% 
  filter(indgen > 0 & !is.na(indgen))

ind_ratio_ns <- merge(ind_ratio_n, ind_ratio_s, by = c("year", "indgen")) %>% 
  mutate(Industry = case_when(indgen == 10 ~ 'Agriculture',
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
  select(year, Industry, workerratio_n, workerratio_s)

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
