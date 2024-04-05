# District level analysis 

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

dn02_dist <- ec_list[[3]] %>% 
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

dn03_dist <- ec_list[[4]] %>% 
  rename(prov2003 = tinh,
         dist2003 = huyen,
         xa03 = xa) %>% 
  left_join(district_codes, by = c("prov2003", "dist2003", "xa03")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn04_dist <- ec_list[[5]] %>% 
  rename(prov2005 = tinh,
         dist2005 = huyen,
         ward2005 = xa) %>% 
  mutate(across(c(prov2005, dist2005, ward2005), as.double)) %>% 
  left_join(district_codes, by = c("prov2005", "dist2005", "ward2005")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn05_dist <- ec_list[[6]] %>% 
  rename(prov2005 = tinh,
         dist2005 = huyen,
         ward2005 = xa) %>% 
  mutate(across(c(prov2005, dist2005, ward2005), as.double)) %>% 
  left_join(district_codes, by = c("prov2005", "dist2005", "ward2005")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn06_dist <- ec_list[[7]] %>% 
  rename(prov2006 = tinh,
         dist2006 = huyen,
         ward2006 = xa) %>% 
  mutate(across(c(prov2006, dist2006, ward2006), as.double),
         ward2006 = ifelse(dist2006 == 755, 75500, ward2006)) %>% 
  left_join(district_codes, by = c("prov2006", "dist2006", "ward2006")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn07 <- ec_list[[8]] %>% 
  rename(prov2007 = tinh,
         dist2007 = huyen,
         ward2007 = xa) %>% 
  mutate(across(c(prov2007, dist2007, ward2007), as.double)) %>% 
  left_join(district_codes, by = c("prov2007", "dist2007", "ward2007")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn08 <- ec_list[[9]] %>% 
  rename(prov2008 = tinh,
         dist2008 = huyen,
         ward2008 = xa) %>% 
  mutate(across(c(prov2008, dist2008, ward2008), as.double)) %>% 
  left_join(district_codes, by = c("prov2008", "dist2008", "ward2008")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn09 <- ec_list[[10]] %>% 
  rename(prov2009 = tinh,
         dist2009 = huyen,
         ward2009 = xa) %>% 
  mutate(across(c(prov2009, dist2009, ward2009), as.double)) %>% 
  left_join(district_codes, by = c("prov2009", "dist2009", "ward2009")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn10 <- ec_list[[11]] %>% 
  rename(prov2010 = tinh,
         dist2010 = huyen,
         ward2010 = xa) %>% 
  mutate(across(c(prov2010, dist2010, ward2010), as.double)) %>% 
  left_join(district_codes, by = c("prov2010", "dist2010", "ward2010")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn11 <- ec_list[[12]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2011 = tinh,
         dist2011 = huyen,
         ward2011 = xa) %>% 
  mutate(across(c(prov2011, dist2011, ward2011), as.double)) %>% 
  left_join(district_codes, by = c("prov2011", "dist2011", "ward2011")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn12 <- ec_list[[13]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2012 = tinh,
         dist2012 = huyen,
         ward2012 = xa) %>% 
  mutate(across(c(prov2012, dist2012, ward2012), as.double)) %>% 
  left_join(district_codes, by = c("prov2012", "dist2012", "ward2012")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn13 <- ec_list[[14]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2013 = tinh,
         dist2013 = huyen,
         ward2013 = xa) %>% 
  mutate(across(c(prov2013, dist2013, ward2013), as.double)) %>% 
  left_join(district_codes, by = c("prov2013", "dist2013", "ward2013")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn14 <- ec_list[[15]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2014 = tinh,
         dist2014 = huyen,
         ward2014 = xa) %>% 
  mutate(across(c(prov2014, dist2014, ward2014), as.double)) %>% 
  left_join(district_codes, by = c("prov2014", "dist2014", "ward2014")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn15 <- ec_list[[16]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2015 = tinh,
         dist2015 = huyen,
         ward2015 = xa) %>% 
  mutate(across(c(prov2015, dist2015, ward2015), as.double)) %>% 
  left_join(district_codes, by = c("prov2015", "dist2015", "ward2015")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn16 <- ec_list[[17]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, gioitinh, namsinh, tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         director = gioitinh, 
         prov2016 = tinh,
         dist2016 = huyen,
         ward2016 = xa) %>% 
  mutate(female_dir = ifelse(director == 2, 1, 0),
         male_dir = ifelse(director == 1, 1, 0),
         across(c(prov2016, dist2016, ward2016), as.double)) %>% 
  left_join(district_codes, by = c("prov2016", "dist2016", "ward2016")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T),
            female_dir = sum(female_dir == 1, na.rm = T),
            male_dir = sum(male_dir == 1, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy,
         dirratio = male_dir/female_dir) %>% 
  filter(!is.na(prov2018))

dn17 <- ec_list[[18]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd,  tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2017 = tinh,
         dist2017 = huyen,
         ward2017 = xa) %>% 
  mutate(across(c(prov2017, dist2017, ward2017), as.double)) %>% 
  left_join(district_codes, by = c("prov2017", "dist2017", "ward2017")) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))

dn18 <- ec_list[[19]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2018 = tinh,
         dist2018 = huyen,
         ward2018 = xa) %>% 
  group_by(prov2018, dist2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018))
