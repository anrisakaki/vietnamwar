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
         urban = ifelse(urban == 1, 1, 0),
         south = ifelse(geo1_vn1989 > 26 & year == 1989 | geo1_vn1989 == 2 & year == 1989 , 1, 0),
         south = ifelse(geo1_vn1999 > 405 & year == 1999, 1, 0),
         south = ifelse(geo1_vn2009 > 44, 1, 0))

save(phc, file = "phc.Rda")

# Separating by year 

phc89 <- phc %>% 
  filter(year == 1989) %>% 
  mutate(geo1_vn1989 = ifelse(geo1_vn1989 == 16, 13, geo1_vn1989)) %>% 
  left_join(bombs_province89, by = "geo1_vn1989") %>% 
  mutate(south = ifelse(geo1_vn1989 > 26 | geo1_vn1989 == 2, 1, 0))  %>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 38 & age < 69), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  mutate(geo1_vn1999 = ifelse(geo1_vn1999 == 105, 101, geo1_vn1999),
         geo1_vn1999 = ifelse(geo1_vn1999 == 303, 301, geo1_vn1999)) %>% 
  left_join(bombs_province99, by = "geo1_vn1999") %>% 
  mutate(south = ifelse(geo1_vn1999 > 405, 1, 0))  %>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 48 & age < 79), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  mutate(huyen = as.numeric(substr(geo2_vn2009, 3, 5)),
         geo1_vn2009 = ifelse(geo1_vn2009 == 11, 12, geo1_vn2009),
         geo1_vn2009 = ifelse(geo1_vn2009 == 14, 12, geo1_vn2009)) %>%
  left_join(bombs_province09, by = "geo1_vn2009") %>% 
  mutate(south = ifelse(geo1_vn2009 > 44, 1, 0))%>% 
  group_by(serial) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1 & age > 58 & age < 89), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

phc_all <- bind_rows(phc89, phc99, phc09)

save(phc89, file = "phc89.Rda")
save(phc99, file = "phc99.Rda")
save(phc09, file = "phc09.Rda")

