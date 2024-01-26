# Calculating the sex ratio and LFP of men and women by age cohort in 1989, 1999 and 2009

agecohort_sum <- phc %>% 
  group_by(year, age_cohort, female) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort75_sum <- phc %>% 
  group_by(year, age_cohort75, female) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort75))

# Calculating sex ratio and FLFP of men and women by age cohort and province 

agecohort_prov89 <- phc89 %>% 
  group_by(geo1_vn1989, age_cohort, female, tot_bomb_per) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort75_prov89 <- phc89 %>% 
  group_by(geo1_vn1989, age_cohort, female, tot_bomb_per) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort75_prov99 <- phc99 %>% 
  group_by(geo1_vn1999, age_cohort, female, tot_bomb_per) %>%
  filter(age_cohort == "0-5", "5-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40")
summarise(work = sum(work * perwt) / sum(perwt),
          widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort75_prov09 <- phc09 %>% 
  group_by(geo1_vn2009, age_cohort, female, tot_bomb_per) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort))
