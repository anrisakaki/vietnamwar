load("district_bmr_sum.Rda")

district_bmr_sum <- district_bmr_sum %>% 
  rename(distname = distname2018,
         provname = provname2018)

dn04 <- ec_list[[5]] %>% 
  rename(prov04 = tinh,
         dist04 = huyen,
         ward04 = xa) %>% 
  mutate(across(c(prov04, dist04, ward04), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov04, dist04, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[3]], by = c("prov04", "dist04")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(south, by = "provname") %>% 
  mutate(year = 2004,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(prov04, dist04) %>% 
  mutate(distid = cur_group_id())

dn05 <- ec_list[[6]] %>% 
  rename(prov2005 = tinh,
         dist2005 = huyen,
         ward2005 = xa) %>% 
  mutate(across(c(prov2005, dist2005, ward2005), as.double)) %>% 
  left_join(district_codes, by = c("prov2005", "dist2005", "ward2005")) %>% 
  filter(!is.na(prov2018)) %>%
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov2005, dist2005, ward2005, provname2018, distname2018, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  left_join(south, by = "provname2018") %>% 
  mutate(year = 2005,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(provname2018, distname2018) %>% 
  mutate(distid = cur_group_id())
