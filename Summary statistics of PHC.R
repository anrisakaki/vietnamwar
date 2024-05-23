bombs_prov <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda")

for (i in bombs_prov) {
  load(i)
}

phc <- c("phc.Rda", "phc89.Rda", "phc99.Rda", "phc09.Rda")

for (i in phc) {
  load(i)
}

sexratio_phc <- function(i){
  i %>% 
    filter(!is.na(age)) %>% 
    group_by(age, female) %>% 
    summarise(total = sum(perwt)) %>% 
    pivot_wider(names_from = female, values_from = total) %>% 
    rename(m_total = 2,
           f_total = 3) %>% 
    mutate(sex_ratio = m_total/f_total)
}

# Calculating sex ratio by age, and by north and south 

## 1989

bc_sexratio89 <- phc89 %>% 
  sexratio_phc()

bc_sexratio89_s <- phc89 %>% 
  filter(!is.na(age) & geo1_vn1989 > 26) %>% 
  sexratio_phc() %>% 
  mutate(sex_ratio_south = m_total/f_total)

bc_sexratio89_n <- phc89 %>% 
  filter(!is.na(age) & geo1_vn1989 <= 26) %>% 
  sexratio_phc() %>% 
  mutate(sex_ratio_north = m_total/f_total)

bc_sexratio89 <- list(bc_sexratio89, bc_sexratio89_n, bc_sexratio89_s) %>% 
  reduce(full_join, by = "age") %>% 
  select(age, sex_ratio, sex_ratio_north, sex_ratio_south)

bc_sexratio89_long <- bc_sexratio89 %>% 
  rename(North = sex_ratio_north,
         South = sex_ratio_south) %>% 
  pivot_longer(!age, names_to = "group",
               values_to = "sex_ratio") %>% 
  filter(group != "sex_ratio")

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
