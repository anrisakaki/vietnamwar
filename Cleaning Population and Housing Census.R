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
  select(year, serial, hhwt, geo1_vn, geo1_vn1989, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, labforce, occisco, indgen, agri, empsect, ind, geomig1_5)
phc89 <- left_join(phc89, bombs_province89, by = c("year", "geo1_vn1989"))

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  # Ha Tay merged with Hanoi 
  mutate(geo1_vn1999 = ifelse(geo1_vn1999 == 105, 101, geo1_vn1999)) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1999, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, labforce, occisco, indgen, agri, empsect, ind, geomig1_5)
phc99 <- left_join(phc99, bombs_province99, by = c("year", "geo1_vn1999"))

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn2009, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, labforce, occisco, indgen, agri, empsect, ind, geomig1_5)
phc09 <- left_join(phc09, bombs_province09, by = c("year", "geo1_vn2009"))

save(phc89, file = "phc89.Rda")
save(phc99, file = "phc99.Rda")
save(phc09, file = "phc09.Rda")
