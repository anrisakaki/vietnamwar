provbombs <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda", "provbombs_sum.Rda")

for (i in provbombs) {
  load(i)
}

# Cleaning Population and Housing Census 

phc <- phc %>%
  mutate(married = ifelse(marst == 2, 1, 0),
         widowed = ifelse(marst == 4, 1, 0),
         single = ifelse(marst == 1, 1, 0),
         minority = ifelse(ethnicvn > 1, 1, 0),
         literate = ifelse(lit == 2, 1, 0),
         work = ifelse(empstat == 1, 1, 0),
         disabled = ifelse(disabled == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0),
         agri = ifelse(indgen == 10, 1, 0),
         housework = ifelse(empstatd == 310, 1, 0),
         manu = ifelse(indgen == 30, 1, 0),
         self = ifelse(empsect == 22, 1, 0),
         migration = ifelse(migrate5 == 10 | migrate5 == 11 | migrate5 == 12, 0, 1),
         age = ifelse(age == 999, NA, age),
         work = ifelse(age < 17 | age > 64, NA, work),
         agri = ifelse(age < 17 | age > 64, NA, agri),
         manu = ifelse(age < 17 | age > 64, NA, manu),
         self = ifelse(age < 17 | age > 64, NA, self),
         housework = ifelse(age < 17 | age > 64, NA, housework),
         urban = ifelse(urban == 1, 1, 0)) %>% 
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
  mutate(geo1_vn1989 = ifelse(geo1_vn1989 == 16, 13, geo1_vn1989)) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1989, regnvn, pernum, perwt, nchild, relate, famsize, age, age_cohort, female, marst, married, widowed, single,
         birthyr, minority, migration, literate, work, edattain, educvn, yrschool, labforce, occisco, indgen, isco88a, agri, manu, housework, empsect, self, ind, geomig1_5, urban, popdensgeo1) %>% 
  left_join(bombs_province89, by = "geo1_vn1989") %>% 
  mutate(south = ifelse(geo1_vn1989 > 26 | geo1_vn1989 == 2, 1, 0))  %>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 38 & age < 69), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  mutate(geo1_vn1999 = ifelse(geo1_vn1999 == 105, 101, geo1_vn1999),
         geo1_vn1999 = ifelse(geo1_vn1999 == 303, 301, geo1_vn1999)) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1999, regnvn, pernum, perwt, nchild, relate, age, age_cohort, female, marst, married, widowed, single,
         birthyr, minority, migration, literate, work, edattain, educvn, yrschool, labforce, occisco, indgen, isco88a, agri, manu, housework, empsect, self, ind,classwk, geomig1_5, urban, popdensgeo1) %>% 
  left_join(bombs_province99, by = "geo1_vn1999") %>% 
  mutate(south = ifelse(geo1_vn1999 > 405, 1, 0))  %>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 48 & age < 79), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  mutate(geo1_vn2009 = ifelse(geo1_vn2009 == 11, 12, geo1_vn2009),
         geo1_vn2009 = ifelse(geo1_vn2009 == 14, 12, geo1_vn2009)) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn2009, regnvn, pernum, perwt, nchild, relate, age, age_cohort, female, marst, married, widowed, single,
         birthyr, minority, migration, literate, work, edattain, educvn, yrschool, labforce, occisco, indgen, isco88a, agri, manu, housework, empsect, self, ind, geomig1_5, urban, popdensgeo1) %>% 
  left_join(bombs_province09, by = "geo1_vn2009") %>% 
  mutate(south = ifelse(geo1_vn2009 > 44, 1, 0))%>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 58 & age < 89), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

save(phc89, file = "phc89.Rda")
save(phc99, file = "phc99.Rda")
save(phc09, file = "phc09.Rda")