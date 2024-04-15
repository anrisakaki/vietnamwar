dn00 <- ec_list[[1]] %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(ld612 = ifelse(is.na(ld612), 0, ld612),
         workerratio = (ld611-ld612)/ld612,
         workerratio_eoy = (ld613-ld614)/ld614,
         workerratio = ifelse(ld611 < 1, NA, workerratio),
         share_f = ld612/ld611,
         share_f = ifelse(ld612 == 0, 0, share_f),
         share_f = ifelse(ld611 < 1, NA, share_f)) %>% 
  select(prov2002, dist2002, xa02, provname2018, distname2018, nganh_kd, ld611, ld612, share_f, workerratio, workerratio_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  left_join(south, by = "provname2018") %>% 
  mutate(year = 2000,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(provname2018, distname2018) %>% 
  mutate(distid = cur_group_id())

dn01 <- ec_list[[2]] %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov2002, dist2002, xa02, provname2018, distname2018, nganh_kd, ld11, ld12, share_f, workerratio, workerratio_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  left_join(south, by = "provname2018") %>% 
  mutate(year = 2001,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(provname2018, distname2018) %>% 
  mutate(distid = cur_group_id())

dn02 <- ec_list[[3]] %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov2002, dist2002, xa02, provname2018, distname2018, nganh_kd, workerratio, workerratio_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  left_join(south, by = "provname2018") %>% 
  mutate(year = 2002,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(provname2018, distname2018) %>% 
  mutate(distid = cur_group_id())

dn03 <- ec_list[[4]] %>% 
  rename(prov2003 = tinh,
         dist2003 = huyen,
         xa03 = xa) %>% 
  left_join(district_codes, by = c("prov2003", "dist2003", "xa03")) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov2003, dist2003, xa03, provname2018, distname2018, nganh_kd, workerratio, workerratio_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  left_join(south, by = "provname2018") %>% 
  mutate(year = 2003,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(provname2018, distname2018) %>% 
  mutate(distid = cur_group_id())

dn04 <- ec_list[[5]] %>% 
  rename(prov2005 = tinh,
         dist2005 = huyen,
         xa05 = xa) %>% 
  mutate(across(c(prov2004, dist2004, xa04), as.double)) %>% 
  left_join(district_codes, by = c("prov2004", "dist2004", "xa04")) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov2003, dist2003, xa03, provname2018, distname2018, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  left_join(south, by = "provname2018") %>% 
  mutate(year = 2004,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr)) %>% 
  group_by(provname2018, distname2018) %>% 
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
