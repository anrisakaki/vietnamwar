provbombs <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda", "provbombs_sum.Rda")

for (i in provbombs) {
  load(i)
}

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
         age3464 = ifelse(age > 35 & age < 65, 1, 0),
         migration = ifelse(migrate5 == 10 | migrate5 == 11 | migrate5 == 12, 0, 1),
         age = ifelse(age == 999, NA, age)) %>% 
  mutate(
    age_cohort = case_when(
      age <= 4 ~ "0-4",
      age <= 9 ~ "5-9",
      age <= 14 ~ "10-14",
      age <= 19 ~ "15-19",
      age <= 24 ~ "20-24",
      age <= 29 ~ "25-29",
      age <= 34 ~ "30-34",
      age <= 39 ~ "35-39",
      age <= 44 ~ "40-44",
      age <= 49 ~ "45-49",
      age <= 54 ~ "50-54",
      age <= 59 ~ "55-59",
      age <= 64 ~ "60-64",
      age <= 100 ~ "65+"))  

save(phc, file = "phc.Rda")

# Separating by year 

phc89 <- phc %>% 
  filter(year == 1989) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1989, regnvn, pernum, perwt, nchild, age, age3464, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5)
phc89 <- left_join(phc89, bombs_province89, by = c("year", "geo1_vn1989"))

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  # Ha Tay merged with Hanoi 
  mutate(geo1_vn1999 = ifelse(geo1_vn1999 == 105, 101, geo1_vn1999)) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1999, regnvn, pernum, perwt, nchild, age, age3464, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5)
phc99 <- left_join(phc99, bombs_province99, by = c("year", "geo1_vn1999"))

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn2009, regnvn, pernum, perwt, nchild, age, age3464, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5)
phc09 <- left_join(phc09, bombs_province09, by = c("year", "geo1_vn2009"))

# Calculating the sex ratio and LFP of men and women by age cohort in 1989 

prov_89_f <- phc89 %>% 
  filter(female == 1 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(total_f = sum(perwt),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt) / sum(perwt))

prov_89_m <- phc89 %>% 
  filter(female == 0 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(total_m = sum(perwt),
            work_m = sum(work * perwt) / sum(perwt))

prov89_age3464_f <- phc89 %>% 
  filter(age3464 == 1 & female == 1) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(age3464_f = sum(perwt),
            age3464_widowed_f = sum(widowed * perwt) / sum(perwt),
            age3464_work_f = sum(work * perwt) / sum(perwt))

prov89_age3464_m <- phc89 %>% 
  filter(age3464 == 1 & female == 0) %>% 
  group_by(geo1_vn1989) %>% 
  summarise(age3464_m = sum(perwt),
            age3464_work_m = sum(work * perwt) / sum(perwt))

sexratio_prov_89 <- list(prov_89_f, prov_89_m, prov89_age3464_f, prov89_age3464_m, bombs_province89) %>% 
  reduce(full_join, by = "geo1_vn1989") %>% 
  mutate(sexratio = total_m/total_f,
         sexratio3464 = age3464_m/age3464_f,
         workratio = (work_m/work_f),
         workratio3464 = age3464_work_m/age3464_work_f)

# Calculating the sex ratio and LFP of men and women by age cohort in 1999 
prov_99_f <- phc99 %>% 
  filter(female == 1 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1999) %>% 
  summarise(total_f = sum(perwt),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt) / sum(perwt))

prov_99_m <- phc99 %>% 
  filter(female == 0 & age > 15 & age < 65) %>% 
  group_by(geo1_vn1999) %>% 
  summarise(total_m = sum(perwt),
            work_m = sum(work * perwt) / sum(perwt))

sexratio_prov_99 <- list(prov_99_f, prov_99_m, bombs_province99) %>% 
  reduce(full_join, by = "geo1_vn1999") %>% 
  mutate(sexratio = total_m/total_f,
         workratio = (work_m/work_f))

# Calculating the sex ratio and LFP of men and women by age cohort in 2009

prov_09_f <- phc09 %>% 
  filter(female == 1 & age > 15 & age < 65) %>% 
  group_by(geo1_vn2009) %>% 
  summarise(total_f = sum(perwt),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt) / sum(perwt))

prov_09_m <- phc09 %>% 
  filter(female == 0 & age > 15 & age < 65) %>% 
  group_by(geo1_vn2009) %>% 
  summarise(total_m = sum(perwt))

sexratio_prov_09 <- list(prov_09_f, prov_09_m, bombs_province09) %>% 
  reduce(full_join, by = "geo1_vn2009") %>% 
  mutate(sexratio = total_m/total_f)

# Saving data

save(phc89, file = "phc89.Rda")
save(phc99, file = "phc99.Rda")
save(phc09, file = "phc09.Rda")

save(sexratio_prov_89, file = "sexratio_prov_89.Rda")
save(sexratio_prov_99, file = "sexratio_prov_99.Rda")
save(sexratio_prov_09, file = "sexratio_prov_09.Rda")
