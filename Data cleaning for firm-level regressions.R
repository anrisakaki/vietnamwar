load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")
load("province_bmr_sum02.Rda")
load("province_bmr_sum2.Rda")
load("prov02_vhlss.Rda")
load("prov04_vhlss.Rda")

ppn002 <- ppn0019 %>% 
  group_by(tinh02) %>% 
  summarise(area = sum(Area),
            pop_01 = sum(X2001)*10000,
            pop_02 = sum(X2002)*10000) %>% 
  mutate(popdensity_01 = pop_01/area,
         popdensity_02 = pop_02/area) %>% 
  rename(tinh = tinh02)

ppn03 <- ppn0019 %>% 
  group_by(tinh) %>% 
  summarise(area = sum(Area),
            pop_03 = sum(X2003)*10000) %>% 
  mutate(popdensity_03 = pop_03/area)

ppn0419 <- ppn0019 %>% 
  group_by(tinh_08) %>% 
  summarise(area = sum(Area),
            pop_04 = sum(X2004)*10000,
            pop_05 = sum(X2005)*10000,
            pop_06 = sum(X2006)*10000,
            pop_07 = sum(X2007)*10000,
            pop_08 = sum(X2008)*10000,
            pop_09 = sum(X2009)*10000,
            pop_10 = sum(X2010)*10000,
            pop_11 = sum(X2011)*10000,
            pop_12 = sum(X2012)*10000,
            pop_13 = sum(X2013)*10000,
            pop_14 = sum(X2014)*10000,
            pop_15 = sum(X2015)*10000,
            pop_16 = sum(X2016)*10000,
            pop_17 = sum(X2017)*10000,
            pop_18 = sum(X2018)*10000
  ) %>% 
  mutate(popdensity_04 = pop_04/area,
         popdensity_05 = pop_05/area,
         popdensity_06 = pop_06/area,
         popdensity_07 = pop_07/area,
         popdensity_08 = pop_08/area,
         popdensity_09 = pop_09/area,
         popdensity_10 = pop_10/area,
         popdensity_11 = pop_11/area,
         popdensity_12 = pop_12/area,
         popdensity_13 = pop_13/area,
         popdensity_14 = pop_14/area,
         popdensity_15 = pop_15/area,
         popdensity_16 = pop_16/area,
         popdensity_17 = pop_17/area,
         popdensity_18 = pop_18/area) %>% 
  rename(tinh = tinh_08)

sexratio01 <- prov02_vhlss %>% select(tinh, sex_ratio)
sexratio03 <- prov04_vhlss %>% select(tinh, sex_ratio)

dn_fn <- function(i) {
  i %>% 
    mutate(across(c(tinh, huyen, xa), as.numeric),
           nganh_kd = as.numeric(substr(nganh_kd, 1, 4)),
           nganh_kd2 = case_when(
             nchar(nganh_kd) > 3 ~ as.numeric(substr(nganh_kd, 1, 2)),
             nchar(nganh_kd) == 3 ~ as.numeric(substr(nganh_kd, 1, 1)),
             TRUE ~ as.numeric(nganh_kd)  
           ),
           workerratio = (nworkers-fworkers)/fworkers,
           share_f = tot_fworkers/tot_workers,
           tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
           workerratio = ifelse(nworkers == 0 | fworkers == 0, NA, workerratio),
           tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio),
           share_f = ifelse(fworkers == 0 | is.na(fworkers), 0, share_f)) %>% 
    select(tinh, ma_thue, nganh_kd, nganh_kd2, lhdn, tot_workers, tot_fworkers, nworkers, fworkers,
           workerratio, tot_workerratio, share_f) %>% 
    left_join(sexratios, by = "tinh") %>% 
    left_join(ppn0419, by = "tinh")
}

# Private = Private enterprise | JSC w no state capital 
# SOE = Enterprise with 100% state capital 
# FOE = 100% foreign capital 

# Private limited companies are predominantly classified as JSC w state capital in 2001 
dn02 <- ec_list[[3]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 3 | lhdn == 6 | lhdn == 7, 1, 0),
         collective = ifelse(lhdn == 3, 1, 0),
         private = ifelse(lhdn == 4, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 407, 1, 0),
         manu = ifelse(nganh_kd2 > 14 & nganh_kd2 < 38, 1, 0),
         year = 2002) %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  left_join(sexratio01, by = "tinh") %>% 
  left_join(ppn002, by = "tinh")

dn03 <- ec_list[[4]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>%  
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 3, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 407, 1, 0),
         manu = ifelse(nganh_kd2 > 14 & nganh_kd2 < 38, 1, 0),
         year = 2003) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  left_join(sexratio03, by = "tinh") %>% 
  left_join(ppn03, by = "tinh")

dn04 <- ec_list[[5]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 101000 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14,) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         south = ifelse(tinh > 44, 1, 0),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         manu = ifelse(nganh_kd2 > 14 & nganh_kd2 < 38, 1, 0),
         year = 2004) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn05 <- ec_list[[6]] %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 101000 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 14 & nganh_kd2 < 38, 1, 0),
         year = 2005) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn06 <- ec_list[[7]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>%  
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2006) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn07 <- ec_list[[8]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2007) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn08 <- ec_list[[9]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         year = 2008) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn09 <- ec_list[[10]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2009) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn10 <- ec_list[[11]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2010) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn11 <- ec_list[[12]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn < 5, 1, 0),
         collective = ifelse(lhdn == 6, 1, 0),
         private = ifelse(lhdn == 7 | lhdn == 10, 1, 0),
         priv_w_state = ifelse(lhdn == 9, 1, 0),
         foe = ifelse(lhdn == 12, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2011) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn12 <- ec_list[[13]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2012) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn13 <- ec_list[[14]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2013) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn14 <- ec_list[[15]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2014) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn15 <- ec_list[[16]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2015) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn16 <- ec_list[[17]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = as.numeric(nganh_kd),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           nganh_kd >= 10101 ~ as.numeric(substr(nganh_kd, 1, 4)),
           TRUE ~ as.numeric(nganh_kd)  
         ),
         nganh_kd2 = case_when(
           nchar(nganh_kd) > 3 ~ as.numeric(substr(nganh_kd, 1, 2)),
           nchar(nganh_kd) == 3 ~ as.numeric(substr(nganh_kd, 1, 1)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21,
         dir_yob = namsinh,
         dir_ethnicity = dantoc) %>% 
  mutate(nganh_kd = as.numeric(nganh_kd),
         tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio),
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_fworkers == 0 | is.na(tot_fworkers), 0, share_f),
         across(c(tinh, huyen, xa), as.numeric),
         female_dir = ifelse(gioitinh == 2, 1, 0),
         female_dir = ifelse(female_dir == 0 & gioitinh == 0 | quoctich != "VN", NA, female_dir)) %>% 
  select(tinh, ma_thue, nganh_kd, nganh_kd2, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f, gioitinh, quoctich, female_dir, dir_yob, dir_ethnicity) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2016) %>% 
  left_join(sexratios, by = "tinh") %>% 
  left_join(ppn0419, by = "tinh")

dn17 <- ec_list[[18]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = as.numeric(nganh_kd),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           nganh_kd >= 10101 ~ as.numeric(substr(nganh_kd, 1, 4)),
           TRUE ~ as.numeric(nganh_kd)  
         ),
         nganh_kd2 = case_when(
           nchar(nganh_kd) > 3 ~ as.numeric(substr(nganh_kd, 1, 2)),
           nchar(nganh_kd) == 3 ~ as.numeric(substr(nganh_kd, 1, 1)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio),
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_fworkers == 0 | is.na(tot_fworkers), 0, share_f),
         across(tinh, as.numeric)) %>% 
  select(tinh, ma_thue, nganh_kd, nganh_kd2, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>%
  mutate(soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2017) %>% 
  left_join(sexratios, by = "tinh") %>% 
  left_join(ppn0419, by = "tinh")

dn18 <- ec_list[[19]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         nganh_kd = as.numeric(nganh_kd),
         nganh_kd = case_when(
           nganh_kd < 10101 ~ as.numeric(substr(nganh_kd, 1, 3)),
           nganh_kd >= 10101 ~ as.numeric(substr(nganh_kd, 1, 4)),
           TRUE ~ as.numeric(nganh_kd)  
         ),
         nganh_kd2 = case_when(
           nchar(nganh_kd) > 3 ~ as.numeric(substr(nganh_kd, 1, 2)),
           nchar(nganh_kd) == 3 ~ as.numeric(substr(nganh_kd, 1, 1)),
           TRUE ~ as.numeric(nganh_kd)  
         )) %>%
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio),
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_fworkers == 0 | is.na(tot_fworkers), 0, share_f)) %>% 
  select(tinh, ma_thue, nganh_kd, nganh_kd2, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(soe = ifelse(lhdn == 1 | lhdn == 2 | lhdn == 4, 1, 0),
         collective = ifelse(lhdn == 5, 1, 0),
         private = ifelse(lhdn == 6 | lhdn == 9, 1, 0),
         priv_w_state = ifelse(lhdn == 8, 1, 0),
         foe = ifelse(lhdn == 11, 1, 0),
         south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd2 > 9 & nganh_kd2 < 35, 1, 0),
         year = 2018) %>% 
  left_join(sexratios, by = "tinh") %>% 
  left_join(ppn0419, by = "tinh")

fdir_sum <- dn16 %>% 
  group_by(south, female_dir) %>% 
  summarise(mean_nworkers = mean(tot_workers, na.rm = T),
            sd_nworkers = sd(tot_workers, na.rm = T),
            mean_sharef = mean(share_f, na.rm = T),
            sd_sharef = sd(share_f, na.rm = T),
            mean_workerratio = mean(tot_workerratio, na.rm = T),
            sd_workerratio = sd(tot_workerratio, na.rm = T)) %>% 
  filter(!is.na(female_dir))

fshare_indsum <- dn16 %>% 
  group_by(nganh_kd2) %>% 
  summarise(nfirms = n(),
            nfirms_f = sum(female_dir == 1, na.rm = T)) %>% 
  mutate(share_f = round((nfirms_f / nfirms) * 100, 2)) %>% 
  select(nganh_kd2, nfirms, share_f) %>% 
  mutate(nganh_kd2 = as.character(nganh_kd2))

dn16 %>% 
  filter(female_dir == 1) %>% 
  summarise(n = n())

share_fdir_indsum <- dn16 %>% 
  filter(female_dir == 1) %>% 
  group_by(nganh_kd2) %>% 
  summarise(n = n()) %>% 
  mutate(share = round((n/135751)*100, 2))

dn <- bind_rows(dn02, dn03, dn04, dn05, dn06, dn07, dn08, dn09, dn10, dn11, dn12, dn13, dn14, dn15, dn16, dn17, dn18)
