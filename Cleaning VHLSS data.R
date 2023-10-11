#######################
# cleaning VHLSS data #
#######################

m1_06 <- m1_06 %>% 
  rename(birth_year = m1ac4b,
         hk = m1ac8) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0)) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, birth_year, female, hk)

m2a_06 <- m2a_06 %>%
  rename(educ = m2ac1) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, educ)

m4a_06 <- m4a_06 %>% 
  rename(work = m4ac2) %>%
  mutate(income = m4ac11 +m4ac12f) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, work, income)

weights_06 <- weights_vhlss %>%
  select(tinh, huyen, xa, wt45) %>% 
  rename("hhwt" = wt45) %>% 
  distinct()

vhlss06 <- list(m1_06, m2a_06, m4a_06) %>% 
  reduce(full_join, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv")) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(tinh, huyen, xa, diaban, hoso, matv) %>%  
  mutate(ivid = cur_group_id())

# Adding weights 

vhlss06 <- left_join(vhlss06, weights_06, by = c("tinh", "huyen", "xa"))

# Creating district variable

vhlss06 <- vhlss06 %>%
  mutate(xa = sprintf("%02d", xa),
          district = paste0(as.character(tinh), xa))

hhinc06 <- weights_vhlss %>% 
  mutate(xa = sprintf("%02d", xa),
         district = paste0(as.character(tinh), xa),
         tot_hhinc = rlincomepc * hhsize)  

# Matching with Miguel and THOR bombing data 

bombs_dist <- bombs_district %>% select(district, north_lat, east_long, south, tot_bmr, tot_bmr_per, districtname, provincename, urban) %>% 
  mutate(district = as.character(district))
bombs_prov <- bombs_province %>% select(province, tot_bmr) %>% 
  rename(tot_bmr_prov = tot_bmr,
         tinh = province)

vhlss06_bombs <- list(vhlss06, bombs_dist, thor_dist) %>% 
  reduce(merge, by = "district") %>% 
  mutate(tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         tot_bombs = ifelse(is.na(tot_bombs), 0, tot_bmr))

vhlss06_bombs <- left_join(vhlss06_bombs, bombs_prov, by = "tinh")

hhinc06_bombs <- list(hhinc06, bombs_dist, thor_dist) %>% 
  reduce(merge, by = "district") %>%   
  select(-"urban.y") %>% 
  rename(urban = urban.x)

hhinc06_bombs <- left_join(hhinc06_bombs, bombs_prov, by = "tinh")

vhlss06_bombs <- vhlss06_bombs %>% mutate(war_time = ifelse(birth_year > 1960 & birth_year < 1975, 1, 0))


