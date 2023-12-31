# Cleaning Population and Housing Census 

phc <- phc %>%
  mutate(married = ifelse(marst == 2, 1, 0),
         widowed = ifelse(marst == 4, 1, 0),
         minority = ifelse(ethnicvn > 1, 1, 0),
         literate = ifelse(lit == 2, 1, 0),
         work = ifelse(empstat == 1, 1, 0),
         disabled = ifelse(disabled == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0),
         agri = ifelse(indgen == 10, 1, 0),
         age75 = 1975 - birthyr,
         migration = ifelse(migrate5 > 19, 1, 0),
         migration = ifelse(migrate5 == 0 | mig1_5_vn == 99, NA, migration)) %>% 
  filter(age != 999) %>% 
  mutate(
    age_cohort = case_when(
      age <= 10 ~ "0-10",
      age <= 15 ~ "11-15",
      age <= 20 ~ "16-20",
      age <= 25 ~ "21-25",
      age <= 30 ~ "26-30",
      age <= 35 ~ "31-35",
      age <= 40 ~ "36-40",
      age <= 45 ~ "41-45",
      age <= 50 ~ "46-50",
      age <= 55 ~ "51-55",
      age <= 60 ~ "56-60",
      age <= 65 ~ "61-65",
      age <= 70 ~ "65+"),
    age_cohort75 = case_when(
      age75 <= 10 ~ "0-10",
      age75 <= 15 ~ "11-15",
      age75 <= 20 ~ "16-20",
      age75 <= 25 ~ "21-25",
      age75 <= 30 ~ "26-30",
      age75 <= 35 ~ "31-35",
      age75 <= 40 ~ "36-40",
      age75 <= 45 ~ "41-45",
      age75 <= 50 ~ "46-50",
      age75 <= 55 ~ "51-55",
      age75 <= 60 ~ "56-60",
      age75 <= 65 ~ "61-65",
      age75 <= 70 ~ "65+"))  

# Separating by year 

phc89 <- phc %>% 
  filter(year == 1989) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1989, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5,
         age75)
phc89 <- merge(phc89, bombs_province89, by = "geo1_vn1989") %>% distinct()

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1999, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5,
         age75)
phc99 <- merge(phc99, bombs_province99, by = "geo1_vn1999") %>% distinct()

# Calculating the sex ratio and LFP of men and women by age cohort in 1989 

prov_89_f <- phc89 %>% 
  filter(female == 1) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(total_f = sum(perwt),
            log_tot_bomb = mean(log_tot_bomb),
            log_tot_bmr_per = mean(log_tot_bmr_per),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt) / sum(perwt))

prov_89_m <- phc89 %>% 
  filter(female == 0) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(total_m = sum(perwt))

sexratio_prov_89 <- merge(prov_89_f, prov_89_m, by = "geo1_vn1989") %>% 
  mutate(sexratio = total_m/total_f)

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

