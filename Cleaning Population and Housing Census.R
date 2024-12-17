bombs <- c("bombs_province89.Rda", "bombs_province99.Rda", "bombs_province09.Rda", "district_bmr_phc.Rda")

for (i in bombs) {
  load(i)
}

# Cleaning Population and Housing Census 

phc <- phc %>%
  mutate(yrschool = ifelse(yrschool > 19, NA, yrschool),
         married = ifelse(marst == 2, 1, 0),
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
         urban = ifelse(urban == 1, 1, 0),
         south = ifelse(geo1_vn1989 > 26 & year == 1989 | geo1_vn1989 == 2 & year == 1989 , 1, 0),
         south = ifelse(geo1_vn1999 > 405 & year == 1999, 1, 0),
         south = ifelse(geo1_vn2009 > 44, 1, 0),
         south = ifelse(geo1_vn2019 > 44, 1, 0),
         geo2_vn2009 = as.double(geo2_vn2009),
         geo2_vn2019 = as.double(geo2_vn2019),
         geo2_vn = as.double(geo2_vn))

# Separating by year 

phc89 <- phc %>% 
  filter(year == 1989) %>% 
  left_join(bombs_province89, by = "geo1_vn1989") %>% 
  mutate(south = ifelse(geo1_vn1989 > 26 | geo1_vn1989 == 2, 1, 0))  %>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 38 & age < 69), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  mutate(geo1_vn1999 = ifelse(geo1_vn1999 == 105, 101, geo1_vn1999)) %>% 
  left_join(bombs_province99, by = "geo1_vn1999") %>% 
  mutate(south = ifelse(geo1_vn1999 > 405, 1, 0))  %>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 48 & age < 79), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  left_join(bombs_province09, by = "geo1_vn2009") %>% 
  left_join(district_bmr_phc, by = "geo2_vn") %>% 
  mutate(south = ifelse(geo1_vn2009 > 44, 1, 0))%>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 58 & age < 89), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc19 <- phc %>% 
  filter(year == 2019) %>% 
  left_join(bombs_province09, by = c("geo1_vn2019" = "geo1_vn2009")) %>% 
  left_join(district_bmr_phc, by = "geo2_vn") %>% 
  mutate(south = ifelse(geo1_vn2019 > 44, 1, 0))%>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 68), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc_all <- bind_rows(phc89, phc99, phc09, phc19)

save(phc89, file = "phc89.Rda")
save(phc99, file = "phc99.Rda")
save(phc09, file = "phc09.Rda")
save(phc19, file = "phc19.Rda")
save(phc_all, file = "phc_all.Rda")

# Widows in 1989

widowed_89 <- phc89 %>%
  filter(female == 1 & age >15) %>%
  group_by(age, south) %>%
  summarise(
    total_females = sum(perwt, na.rm = TRUE),
    total_widows = sum(perwt[widowed == 1], na.rm = TRUE)
  ) %>%
  mutate(share = (total_widows / total_females) * 100) 
  