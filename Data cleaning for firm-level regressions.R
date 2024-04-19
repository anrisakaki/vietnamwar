load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")

province_bmr_sum <- province_bmr_sum %>% 
  rename(provname = name_1) %>% 
  select(-c(varname_1, provincename)) %>% 
  rename_with(~paste0(., "_prov"), -matches("provname|south")) %>% 
  select(provname, everything())

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
  left_join(province_bmr_sum, by = "provname") %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  mutate(year = 2004,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov04 > 43, 1, 0)) %>% 
  group_by(prov04, dist04) %>% 
  mutate(distid = cur_group_id())

dn05 <- ec_list[[6]] %>% 
  rename(prov05 = tinh,
         dist05 = huyen,
         ward05 = xa) %>% 
  mutate(across(c(prov05, dist05, ward05), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov05, dist05, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[4]], by = c("prov05", "dist05")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>%   
  mutate(year = 2005,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov05 > 43, 1, 0)) %>% 
  group_by(prov05, dist05) %>% 
  mutate(distid = cur_group_id())

dn06 <- ec_list[[7]] %>% 
  rename(prov06 = tinh,
         dist06 = huyen) %>% 
  mutate(across(c(prov06, dist06), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov06, dist06, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[5]], by = c("prov06", "dist06")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>%   
  mutate(year = 2006,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov06 > 43, 1, 0)) %>% 
  group_by(prov06, dist06) %>% 
  mutate(distid = cur_group_id())

dn07 <- ec_list[[8]] %>% 
  rename(prov07 = tinh,
         dist07 = huyen) %>% 
  mutate(across(c(prov07, dist07), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov07, dist07, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[6]], by = c("prov07", "dist07")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>%   
  mutate(year = 2007,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov07 > 43, 1, 0)) %>% 
  group_by(prov07, dist07) %>% 
  mutate(distid = cur_group_id())

dn08 <- ec_list[[9]] %>% 
  rename(prov08 = tinh,
         dist08 = huyen) %>% 
  mutate(across(c(prov08, dist08), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov08, dist08, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[7]], by = c("prov08", "dist08")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2008,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov08 > 43, 1, 0)) %>% 
  group_by(prov08, dist08) %>% 
  mutate(distid = cur_group_id())

dn09 <- ec_list[[10]] %>% 
  rename(prov09 = tinh,
         dist09 = huyen) %>% 
  mutate(across(c(prov09, dist09), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov09, dist09, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[8]], by = c("prov09", "dist09")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2009,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov09 > 43, 1, 0)) %>% 
  group_by(prov09, dist09) %>% 
  mutate(distid = cur_group_id())

dn10 <- ec_list[[11]] %>% 
  rename(prov10 = tinh,
         dist10 = huyen) %>% 
  mutate(across(c(prov10, dist10), as.double)) %>% 
  mutate(ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov10, dist10, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[9]], by = c("prov10", "dist10")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2010,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov10 > 43, 1, 0)) %>% 
  group_by(prov10, dist10) %>% 
  mutate(distid = cur_group_id())

dn11 <- ec_list[[12]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov11 = tinh,
         dist11 = huyen) %>% 
  mutate(across(c(prov11, dist11), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov11, dist11, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[10]], by = c("prov11", "dist11")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2011,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov11 > 43, 1, 0)) %>% 
  group_by(prov11, dist11) %>% 
  mutate(distid = cur_group_id())

dn12 <- ec_list[[13]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov12 = tinh,
         dist12 = huyen) %>% 
  mutate(across(c(prov12, dist12), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov12, dist12, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[11]], by = c("prov12", "dist12")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2012,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov12 > 43, 1, 0)) %>% 
  group_by(prov12, dist12) %>% 
  mutate(distid = cur_group_id())

dn13 <- ec_list[[14]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov13 = tinh,
         dist13 = huyen) %>% 
  mutate(across(c(prov13, dist13), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov13, dist13, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[12]], by = c("prov13", "dist13")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2013,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov13 > 43, 1, 0)) %>% 
  group_by(prov13, dist13) %>% 
  mutate(distid = cur_group_id())

dn14 <- ec_list[[15]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov14 = tinh,
         dist14 = huyen) %>% 
  mutate(across(c(prov14, dist14), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov14, dist14, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[13]], by = c("prov14", "dist14")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2014,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov14 > 43, 1, 0)) %>% 
  group_by(prov14, dist14) %>% 
  mutate(distid = cur_group_id())

dn15 <- ec_list[[16]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov15 = tinh,
         dist15 = huyen) %>% 
  mutate(across(c(prov15, dist15), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov15, dist15, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[14]], by = c("prov15", "dist15")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2015,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov15 > 43, 1, 0)) %>% 
  group_by(prov15, dist15) %>% 
  mutate(distid = cur_group_id())

dn16 <- ec_list[[17]] %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov16 = tinh,
         dist16 = huyen,
         director = gioitinh) %>% 
  mutate(across(c(prov16, dist16), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f),
         female_dir = ifelse(director == 2, 1, 0)) %>% 
  select(prov16, dist16, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[15]], by = c("prov16", "dist16")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2016,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov16 > 43, 1, 0)) %>% 
  group_by(prov16, dist16) %>% 
  mutate(distid = cur_group_id())

dn17 <- ec_list[[18]] %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov17 = tinh,
         dist17 = huyen) %>% 
  mutate(across(c(prov17, dist17), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov17, dist17, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[16]], by = c("prov17", "dist17")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2017,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov17 > 43, 1, 0)) %>% 
  group_by(prov17, dist17) %>% 
  mutate(distid = cur_group_id())

dn18 <- ec_list[[19]] %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov18 = tinh,
         dist18 = huyen) %>% 
  mutate(across(c(prov18, dist18), as.double),
         ld12 = ifelse(is.na(ld12), 0, ld12),
         workerratio = (ld11-ld12)/ld12,
         workerratio_eoy = (ld13-ld14)/ld14,
         workerratio = ifelse(ld11 < 1, NA, workerratio),
         share_f = ld12/ld11,
         share_f = ifelse(ld12 == 0, 0, share_f),
         share_f = ifelse(ld11 < 1, NA, share_f)) %>% 
  select(prov18, dist18, nganh_kd, share_f, workerratio, workerratio_eoy) %>% 
  left_join(geoid_district[[17]], by = c("prov18", "dist18")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>%   
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  left_join(province_bmr_sum, by = "provname") %>% 
  mutate(year = 2018,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr),
         south = ifelse(prov18 > 43, 1, 0 )) %>% 
  group_by(prov18, dist18) %>% 
  mutate(distid = cur_group_id())
