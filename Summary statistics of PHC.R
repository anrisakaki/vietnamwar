bombs_prov <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda")

for (i in bombs_prov) {
  load(i)
}

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
