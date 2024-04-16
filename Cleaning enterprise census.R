load("district_bmr_sum.Rda")

district_bmr_sum <- district_bmr_sum %>% 
  rename(distname = distname2018,
         provname = provname2018)

# District level analysis 

dn04_dist <- ec_list[[5]] %>% 
  rename(prov04 = tinh,
         dist04 = huyen,
         ward04 = xa) %>% 
  mutate(across(c(prov04, dist04, ward04), as.double)) %>% 
  group_by(prov04, dist04) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[3]], by = c("prov04", "dist04")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>% 
  mutate(year = 2004,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn05_dist <- ec_list[[6]] %>% 
  rename(prov05 = tinh,
         dist05 = huyen,
         ward05 = xa) %>% 
  mutate(across(c(prov05, dist05, ward05), as.double)) %>% 
  group_by(prov05, dist05) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[4]], by = c("prov05", "dist05")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>% 
  mutate(year = 2005,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn06_dist <- ec_list[[7]] %>% 
  rename(prov06 = tinh,
         dist06 = huyen) %>% 
  mutate(across(c(prov06, dist06), as.double)) %>% 
  group_by(prov06, dist06) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[5]], by = c("prov06", "dist06")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>% 
  mutate(year = 2006,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn07_dist <- ec_list[[8]] %>% 
  rename(prov07 = tinh,
         dist07 = huyen) %>% 
  mutate(across(c(prov07, dist07), as.double)) %>% 
  group_by(prov07, dist07) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[6]], by = c("prov07", "dist07")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>% 
  mutate(year = 2007,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn08_dist <- ec_list[[9]] %>% 
  rename(prov08 = tinh,
         dist08 = huyen) %>% 
  mutate(across(c(prov08, dist08), as.double)) %>% 
  group_by(prov08, dist08) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[7]], by = c("prov08", "dist08")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2008,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn09_dist <- ec_list[[10]] %>% 
  rename(prov09 = tinh,
         dist09 = huyen) %>% 
  mutate(across(c(prov09, dist09), as.double)) %>% 
  group_by(prov09, dist09) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[8]], by = c("prov09", "dist09")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2009,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn10_dist <- ec_list[[11]] %>% 
  rename(prov10 = tinh,
         dist10 = huyen) %>% 
  mutate(across(c(prov10, dist10), as.double)) %>% 
  group_by(prov10, dist10) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[9]], by = c("prov10", "dist10")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2010,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn11_dist <- ec_list[[12]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov11 = tinh,
         dist11 = huyen) %>% 
  mutate(across(c(prov11, dist11), as.double)) %>% 
  group_by(prov11, dist11) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[10]], by = c("prov11", "dist11")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2011,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn12_dist <- ec_list[[13]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov12 = tinh,
         dist12 = huyen) %>% 
  mutate(across(c(prov12, dist12), as.double)) %>% 
  group_by(prov12, dist12) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[11]], by = c("prov12", "dist12")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2012,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn13_dist <- ec_list[[14]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov13 = tinh,
         dist13 = huyen) %>% 
  mutate(across(c(prov13, dist13), as.double)) %>% 
  group_by(prov13, dist13) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[12]], by = c("prov13", "dist13")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2013,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn14_dist <- ec_list[[15]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov14 = tinh,
         dist14 = huyen) %>% 
  mutate(across(c(prov14, dist14), as.double)) %>% 
  group_by(prov14, dist14) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[13]], by = c("prov14", "dist14")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2014,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn15_dist <- ec_list[[16]] %>% 
  rename(ld13 = ld11,
         ld14 = ld12,
         ld11 = tsld,
         ld12 = tsldnu,
         prov15 = tinh,
         dist15 = huyen) %>% 
  mutate(across(c(prov15, dist15), as.double)) %>% 
  group_by(prov15, dist15) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[14]], by = c("prov15", "dist15")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2015,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn16_dist <- ec_list[[17]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd, gioitinh, namsinh, tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         director = gioitinh, 
         prov16 = tinh,
         dist16 = huyen) %>% 
  mutate(female_dir = ifelse(director == 2, 1, 0),
         male_dir = ifelse(director == 1, 1, 0),
         across(c(prov16, dist16), as.double)) %>% 
  group_by(prov16, dist16) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T),
            female_dir = sum(female_dir == 1, na.rm = T),
            male_dir = sum(male_dir == 1, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy,
         dirratio = male_dir/female_dir) %>% 
  left_join(geoid_district[[15]], by = c("prov16", "dist16")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2016,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn17_dist <- ec_list[[18]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd,  tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov17 = tinh,
         dist17 = huyen) %>% 
  mutate(across(c(prov17, dist17), as.double)) %>% 
  group_by(prov17, dist17) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[16]], by = c("prov17", "dist17")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2017,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

dn18_dist <- ec_list[[19]] %>% 
  select(ma_thue, tinh, huyen, xa, lhdn, nganh_kd,  tsld, tsldnu, ld11, ld21) %>% 
  rename(ld13 = ld11,
         ld14 = ld21,
         ld11 = tsld,
         ld12 = tsldnu,
         prov18 = tinh,
         dist18 = huyen) %>% 
  mutate(across(c(prov18, dist18), as.double)) %>% 
  group_by(prov18, dist18) %>% 
  summarise(nworkers = sum(ld11, na.rm = T),
            fworkers = sum(ld12, na.rm = T),
            nworkers_eoy = sum(ld13, na.rm = T),
            fworkers_eoy = sum(ld14, na.rm = T)) %>% 
  mutate(workerratio = (nworkers-fworkers)/fworkers,
         workerratio_eoy = (nworkers_eoy-fworkers_eoy)/fworkers_eoy) %>% 
  left_join(geoid_district[[17]], by = c("prov18", "dist18")) %>% 
  mutate(provname = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  left_join(district_bmr_sum, by = c("provname", "distname")) %>% 
  filter(!is.na(provname)) %>%
  mutate(year = 2018,
         tot_bmr = ifelse(is.na(tot_bmr), 0, tot_bmr))

south <- south %>% select(name_1, south) %>% rename(provname = name_1)

dn_dist <- bind_rows(dn04_dist, dn05_dist, dn06_dist,
                     dn07_dist, dn08_dist, dn09_dist, dn10_dist, dn11_dist, dn12_dist,
                     dn13_dist, dn14_dist, dn15_dist, dn16_dist, dn17_dist, dn18_dist) %>% 
  left_join(south, "provname")

save(dn_dist, file = "dn_dist.Rda")

# Province 

dn_prov_fn <- function(i) {
  i %>%
    group_by(provname) %>%
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

dn_prov <- bind_rows(dn04_prov, dn05_prov, dn06_prov,
                     dn07_prov, dn08_prov, dn09_prov, dn10_prov, dn11_prov, dn12_prov,
                     dn13_prov, dn14_prov, dn15_prov, dn16_prov, dn17_prov, dn18_prov) %>% 
  left_join(south, by = "provname")
save(dn_prov, file = "dn_prov.Rda")