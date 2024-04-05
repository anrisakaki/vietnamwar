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
         prov2018, dist2018, ward2018, provname2018, distname2018) %>% 
  mutate(provname2018 = str_replace(provname2018, "(Tỉnh|Thành phố) ", ""),
         distname2018 = case_when(
           distname2018 == 'Huyện  Vũng Liêm' ~ 'Huyện Vũng Liêm',
           TRUE ~ distname2018
  ))

dn00_dist <- ec_list[[1]] %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld611, na.rm = T),
            fworkers = sum(ld612 , na.rm = T),
            nworkers_eoy = sum(ld613, na.rm = T),
            fworkers_eoy = sum(ld614, na.rm = T)) %>% 
  filter(!is.na(prov2018)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2000)

dn01_dist <- ec_list[[2]] %>% 
  group_by(tinh, huyen, xa) %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2001)

dn02_dist <- ec_list[[3]] %>% 
  rename(prov2002 = tinh,
         dist2002 = huyen,
         xa02 = xa) %>% 
  left_join(district_codes, by = c("prov2002", "dist2002", "xa02")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2002)

dn03_dist <- ec_list[[4]] %>% 
  rename(prov2003 = tinh,
         dist2003 = huyen,
         xa03 = xa) %>% 
  left_join(district_codes, by = c("prov2003", "dist2003", "xa03")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2003)

dn04_dist <- ec_list[[5]] %>% 
  rename(prov2005 = tinh,
         dist2005 = huyen,
         ward2005 = xa) %>% 
  mutate(across(c(prov2005, dist2005, ward2005), as.double)) %>% 
  left_join(district_codes, by = c("prov2005", "dist2005", "ward2005")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2004)

dn05_dist <- ec_list[[6]] %>% 
  rename(prov2005 = tinh,
         dist2005 = huyen,
         ward2005 = xa) %>% 
  mutate(across(c(prov2005, dist2005, ward2005), as.double)) %>% 
  left_join(district_codes, by = c("prov2005", "dist2005", "ward2005")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2005)

dn06_dist <- ec_list[[7]] %>% 
  rename(prov2006 = tinh,
         dist2006 = huyen,
         ward2006 = xa) %>% 
  mutate(across(c(prov2006, dist2006, ward2006), as.double),
         ward2006 = ifelse(dist2006 == 755, 75500, ward2006)) %>% 
  left_join(district_codes, by = c("prov2006", "dist2006", "ward2006")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2006)

dn07_dist <- ec_list[[8]] %>% 
  rename(prov2007 = tinh,
         dist2007 = huyen,
         ward2007 = xa) %>% 
  mutate(across(c(prov2007, dist2007, ward2007), as.double)) %>% 
  left_join(district_codes, by = c("prov2007", "dist2007", "ward2007")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2007)
  
dn08_dist <- ec_list[[9]] %>% 
  rename(prov2008 = tinh,
         dist2008 = huyen,
         ward2008 = xa) %>% 
  mutate(across(c(prov2008, dist2008, ward2008), as.double)) %>% 
  left_join(district_codes, by = c("prov2008", "dist2008", "ward2008")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2008)

dn09_dist <- ec_list[[10]] %>% 
  rename(prov2009 = tinh,
         dist2009 = huyen,
         ward2009 = xa) %>% 
  mutate(across(c(prov2009, dist2009, ward2009), as.double)) %>% 
  left_join(district_codes, by = c("prov2009", "dist2009", "ward2009")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2009)

dn10_dist <- ec_list[[11]] %>% 
  rename(prov2010 = tinh,
         dist2010 = huyen,
         ward2010 = xa) %>% 
  mutate(across(c(prov2010, dist2010, ward2010), as.double)) %>% 
  left_join(district_codes, by = c("prov2010", "dist2010", "ward2010")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2010)

dn11_dist <- ec_list[[12]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2011 = tinh,
         dist2011 = huyen,
         ward2011 = xa) %>% 
  mutate(across(c(prov2011, dist2011, ward2011), as.double)) %>% 
  left_join(district_codes, by = c("prov2011", "dist2011", "ward2011")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2011)

dn12_dist <- ec_list[[13]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2012 = tinh,
         dist2012 = huyen,
         ward2012 = xa) %>% 
  mutate(across(c(prov2012, dist2012, ward2012), as.double)) %>% 
  left_join(district_codes, by = c("prov2012", "dist2012", "ward2012")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2012)

dn13_dist <- ec_list[[14]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2013 = tinh,
         dist2013 = huyen,
         ward2013 = xa) %>% 
  mutate(across(c(prov2013, dist2013, ward2013), as.double)) %>% 
  left_join(district_codes, by = c("prov2013", "dist2013", "ward2013")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2013)

dn14_dist <- ec_list[[15]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2014 = tinh,
         dist2014 = huyen,
         ward2014 = xa) %>% 
  mutate(across(c(prov2014, dist2014, ward2014), as.double)) %>% 
  left_join(district_codes, by = c("prov2014", "dist2014", "ward2014")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2014)

dn15_dist <- ec_list[[16]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2015 = tinh,
         dist2015 = huyen,
         ward2015 = xa) %>% 
  mutate(across(c(prov2015, dist2015, ward2015), as.double)) %>% 
  left_join(district_codes, by = c("prov2015", "dist2015", "ward2015")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2015)

dn16_dist <- ec_list[[17]] %>% 
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
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T),
            female_dir = sum(female_dir == 1, na.rm = T),
            male_dir = sum(male_dir == 1, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy,
         dirratio = male_dir/female_dir) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2016)

dn17_dist <- ec_list[[18]] %>% 
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
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2017)

dn18_dist <- ec_list[[19]] %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov2018 = tinh,
         dist2018 = huyen,
         ward2018 = xa) %>% 
  mutate(across(c(prov2018, dist2018, ward2018), as.double)) %>% 
  left_join(district_codes, by = c("prov2018", "dist2018", "ward2018")) %>% 
  group_by(prov2018, dist2018, provname2018, distname2018) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  filter(!is.na(prov2018)) %>% 
  left_join(district_bmr_sum, by = c("provname2018", "distname2018")) %>% 
  mutate(year = 2018)

dn_dist <- bind_rows(dn00_dist, dn01_dist, dn02_dist, dn03_dist, dn04_dist, dn05_dist, dn06_dist,
                     dn07_dist, dn08_dist, dn09_dist, dn10_dist, dn11_dist, dn12_dist,
                     dn13_dist, dn14_dist, dn15_dist, dn16_dist, dn17_dist, dn18_dist)
save(dn_dist, file = "dn_dist.Rda")

# Province 

dn_prov_fn <- function(i) {
  i %>%
    group_by(prov2018, provname2018) %>%
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      nworkers_eoy = sum(nworkers_eoy, na.rm = TRUE),
      fworkers_eoy = sum(fworkers_eoy, na.rm = TRUE),
      tot_bmr = sum(tot_bmr, na.rm = TRUE)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
      workerratio_eoy = (nworkers_eoy - fworkers_eoy) / fworkers_eoy
    )
}

dn00_prov <- dn_prov_fn(dn00_dist) %>% mutate(year = 2000)
dn01_prov <- dn_prov_fn(dn01_dist) %>% mutate(year = 2001)
dn02_prov <- dn_prov_fn(dn02_dist) %>% mutate(year = 2002)
dn03_prov <- dn_prov_fn(dn03_dist) %>% mutate(year = 2003)
dn04_prov <- dn_prov_fn(dn04_dist) %>% mutate(year = 2004)
dn05_prov <- dn_prov_fn(dn05_dist) %>% mutate(year = 2005)
dn06_prov <- dn_prov_fn(dn06_dist) %>% mutate(year = 2006)
dn07_prov <- dn_prov_fn(dn07_dist) %>% mutate(year = 2007)
dn08_prov <- dn_prov_fn(dn08_dist) %>% mutate(year = 2008)
dn09_prov <- dn_prov_fn(dn09_dist) %>% mutate(year = 2009)
dn10_prov <- dn_prov_fn(dn10_dist) %>% mutate(year = 2010)
dn11_prov <- dn_prov_fn(dn11_dist) %>% mutate(year = 2011)
dn12_prov <- dn_prov_fn(dn12_dist) %>% mutate(year = 2012)
dn13_prov <- dn_prov_fn(dn13_dist) %>% mutate(year = 2013)
dn14_prov <- dn_prov_fn(dn14_dist) %>% mutate(year = 2014)
dn15_prov <- dn_prov_fn(dn15_dist) %>% mutate(year = 2015)
dn16_prov <- dn_prov_fn(dn16_dist) %>% mutate(year = 2016)
dn17_prov <- dn_prov_fn(dn17_dist) %>% mutate(year = 2017)
dn18_prov <- dn_prov_fn(dn18_dist) %>% mutate(year = 2018)

dn_prov <- bind_rows(dn00_prov, dn01_prov, dn02_prov, dn03_prov, dn04_prov, dn05_prov, dn06_prov,
                     dn07_prov, dn08_prov, dn09_prov, dn10_prov, dn11_prov, dn12_prov,
                     dn13_prov, dn14_prov, dn15_prov, dn16_prov, dn17_prov, dn18_prov)
save(dn_prov, file = "dn_prov.Rda")
