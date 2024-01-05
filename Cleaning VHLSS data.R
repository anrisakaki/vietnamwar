#######################
# cleaning VHLSS data #
#######################

m1_06 <- m1_06 %>% 
  rename(birth_year = m1ac4b,
         hk = m1ac8) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         child = ifelse(m1ac3 == 3, 1, 0),
         parent = ifelse(m1ac3 == 1| m1ac3 == 2, 1, 0)) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, birth_year, female, parent, child, hk)

m2a_06 <- m2a_06 %>%
  rename(educ = m2ac1) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, educ)

m3_06 <- m3_06 %>% 
  rename(wage_work = m3c1a,
         agri_hh = m3c1b, 
         nonagri_hh = m3c1c, 
         work = m3c2,
         work_for = m3c8) %>% 
  mutate(wage_work = ifelse(wage_work == 1, 1, 0),
         agri_hh = ifelse(agri_hh == 1, 1, 0),
         nonagri_hh = ifelse(nonagri_hh == 1, 1, 0),
         work = ifelse(work == 1, 1, 0)) %>% 
  select(tinh02, huyen02, xa02, hoso02, matv02, wage_work, nonagri_hh, agri_hh, work, work_for) %>% 
  rename_with(~ str_replace(.x, "02", ""), everything())

m4a_06 <- m4a_06 %>% 
  rename(work = m4ac2) %>%
  mutate(income = m4ac11 +m4ac12f) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, work, income)

m4ho_06 <- m4ho_06 %>% 
  rename(ethnicity = ch_dantoc) %>%
  select(tinh, xa, hoso, ethnicity) %>%
  mutate(
    tinh = as.numeric(substr(xa, 1, 3)),
    huyen = as.numeric(substr(xa, 4, 5)),
    xa = as.numeric(substr(xa, 6, 7)),
    hoso = as.numeric(substr(hoso, nchar(hoso) - 1, nchar(hoso)))
  ) %>% 
  select(tinh, huyen, xa, hoso, ethnicity)

weights_06 <- weights_vhlss %>%
  select(tinh, huyen, xa, wt45) %>% 
  rename("hhwt" = wt45) %>% 
  distinct()

vhlss06 <- list(m1_06, m2a_06, m4a_06) %>% 
  reduce(full_join, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv")) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(tinh, huyen, xa, diaban, hoso, matv) %>%  
  mutate(ivid = cur_group_id()) %>% 
  mutate(age = 2006 - birth_year)

vhlss06 <- merge(vhlss06, m4ho_06, by = c("tinh", "huyen", "xa", "hoso"))

# Adding weights 

vhlss06 <- left_join(vhlss06, weights_06, by = c("tinh", "huyen", "xa"))

# Matching with Miguel bombing data 
provcode <-mccaig_boundaries %>% 
  select(prov2002, provname2002) %>% 
  distinct() %>% filter(!is.na(prov2002)) %>% 
  rename(province = prov2002)

bombs_province <- left_join(bombs_province, provcode, by = "province")

bombs_prov <- bombs_province %>%
  select(province, provname2002, tot_bmr, tot_bmr_per, area_251_500m, area_501_1000m, area_over_1000m, log_popdensity6061, south) %>% 
  rename(tot_bmr_prov = tot_bmr,
         tinh = province,
         tot_bmr_per_prov = tot_bmr_per)

vhlss06_bombs <- list(vhlss06, bombs_prov) %>% 
  reduce(merge, by = "tinh") %>% 
  mutate(war_time = ifelse(birth_year > 1965 & birth_year < 1976, 1, 0),
         exposed = ifelse(birth_year < 1959, 1, 0),
         work = ifelse(work == 1, 1, 0))  

hhinc06_bombs <- list(hhinc06, bombs_prov) %>% 
  reduce(merge, by = "tinh")
