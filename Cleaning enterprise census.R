district_codes <- district %>% 
  mutate(xa02 = as.numeric(substr(ward2002, nchar(ward2002) - 1, nchar(ward2002))),
         xa03 = as.numeric(substr(ward2003, nchar(ward2003) - 1, nchar(ward2003))),
         xa04 = as.numeric(substr(ward2004, nchar(ward2004) - 1, nchar(ward2004)))) %>% 
  select(prov2002, dist2002, xa02, prov2003, dist2003, xa03, prov2004, dist2004, xa04,
         prov2005, dist2005, ward2005, prov2006, dist2006, ward2006,
         prov2007, dist2007, ward2007, prov2007, dist2007, ward2007,
         prov2008, dist2008, ward2008, prov2009, dist2009, ward2009,
         prov2010, dist2010, ward2010, prov2011, dist2011, ward2011,
         prov2012, dist2012, ward2012, prov2013, dist2013, ward2013,
         prov2014, dist2014, ward2014, prov2015, dist2015, ward2015,
         prov2016, dist2016, ward2016, prov2017, dist2017, ward2017,
         prov2018, dist2018, ward2018)

dn00_dist <- ec_list[[1]] %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld611, na.rm = T),
            fworkers = sum(ld612 , na.rm = T),
            nworkers_eoy = sum(ld613, na.rm = T),
            fworkers_eoy = sum(ld614, na.rm = T)) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy)

dn01_dist <- ec_list[[2]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14) %>% 
  group_by(tinh, huyen, xa) %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn02 <- ec_list[[3]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14) %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn03 <- ec_list[[4]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn04 <- ec_list[[5]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn05 <- ec_list[[6]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn06 <- ec_list[[7]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn07 <- ec_list[[8]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn08 <- ec_list[[9]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn09 <- ec_list[[10]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn10 <- ec_list[[11]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, ld11, ld12, ld13, ld14)

dn11 <- ec_list[[12]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld12) %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu)

dn12 <- ec_list[[13]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld12) %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu)

dn13 <- ec_list[[14]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld12) %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu)

dn14 <- ec_list[[15]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld12) %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu)

dn15 <- ec_list[[16]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld12) %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu)

dn16 <- ec_list[[17]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, gioitinh, namsinh, tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         director = gioitinh, 
         dir_yob = namsinh) %>% 
  mutate(female_dir = ifelse(director == 2, 1, 0))

dn17 <- ec_list[[18]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd,  tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu)

dn18 <- ec_list[[19]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu)
