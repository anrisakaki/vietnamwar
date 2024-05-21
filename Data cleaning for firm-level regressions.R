load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")
load("province_bmr_sum02.Rda")
load("prov02_vhlss.Rda")
load("prov04_vhlss.Rda")

ppn0103 <- ppn0019 %>% 
  group_by(tinh02) %>% 
  summarise(area = sum(Area),
            pop_01 = sum(X2001)*10000,
            pop_02 = sum(X2002)*10000,
            pop_03 = sum(X2003)*10000) %>% 
  mutate(popdensity_01 = pop_01/area,
         popdensity_02 = pop_02/area,
         popdensity_03 = pop_03/area) %>% 
  rename(tinh = tinh02)

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

sexratio03 <- prov04_vhlss %>% select(tinh, sex_ratio)

dn_fn <- function(i) {
  i %>% 
    mutate(nganh_kd = as.numeric(substr(nganh_kd, 1, 4)),
           workerratio = (nworkers-fworkers)/fworkers,
           share_f = fworkers/nworkers,
           tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
           across(c(tinh, huyen, xa), as.numeric),
           workerratio = ifelse(nworkers == 0 | fworkers == 0, NA, workerratio),
           tot_workerratio = ifelse(tot_workers == 0, NA, tot_workerratio),
           tot_workerratio = ifelse(tot_fworkers == 0, tot_workers, tot_workerratio)) %>% 
    select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, nworkers, fworkers,
           workerratio, tot_workerratio, share_f) %>% 
    left_join(sexratios, by = "tinh") %>% 
    left_join(ppn0419, by = "tinh")
}

dn01 <- ec_list[[2]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 302, 301, tinh),
         ldc21 = ifelse(is.na(ldc21), 0, ldc21),
         ldc22 = ifelse(is.na(ldc22), 0, ldc22),
         nworkers = ldc11 - ldc21,
         fworkers = ldc12 - ldc22) %>% 
  rename(tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>%
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  left_join(province_bmr_sum02, by = "tinh") %>%
  mutate(south = ifelse(tinh > 407, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         year = 2001) %>% 
  left_join(sexratio01, by = "tinh") %>% 
  left_join(ppn0103, by = "tinh")

dn02 <- ec_list[[3]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         year = 2002) %>% 
  left_join(sexratio01, by = "tinh") %>% 
  left_join(ppn0103, by = "tinh")

dn03 <- ec_list[[4]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>%  
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         year = 2003) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  left_join(sexratio03, by = "tinh") %>% 
  left_join(ppn0103, by = "tinh")

dn04 <- ec_list[[5]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14,) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         year = 2004) %>% 
  left_join(province_bmr_sum2, by = "tinh") 

dn05 <- ec_list[[6]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         year = 2005) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn06 <- ec_list[[7]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         year = 2006) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn07 <- ec_list[[8]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 1429 & nganh_kd < 4010, 1, 0),
         yuear = 2007) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn08 <- ec_list[[9]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2008) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn09 <- ec_list[[10]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2009) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn10 <- ec_list[[11]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2010) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn11 <- ec_list[[12]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2011) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn12 <- ec_list[[13]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2012) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn13 <- ec_list[[14]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2013) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn14 <- ec_list[[15]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2014) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn15 <- ec_list[[16]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(formal_f = tot_fworkers/tot_workers,
         formal_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, formal_f)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2015) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn16 <- ec_list[[17]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21,
         dir_yob = namsinh,
         dir_ethnicity = dantoc) %>% 
  mutate(nganh_kd = as.numeric(nganh_kd),
         tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, share_f),
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0, NA, tot_workerratio),
         tot_workerratio = ifelse(tot_fworkers == 0, tot_workers, tot_workerratio),
         female_dir = ifelse(gioitinh == 2, 1, 0),
         female_dir = ifelse(female_dir == 0 & gioitinh == 0 | quoctich != "VN", NA, female_dir)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f, gioitinh, quoctich, female_dir, dir_yob, dir_ethnicity) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2016) %>% 
  left_join(sexratios, by = "tinh") %>% 
  left_join(ppn0419, by = "tinh")

dn17 <- ec_list[[18]] %>% 
  mutate(nganh_kd = as.numeric(nganh_kd),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, share_f),
         ttot_workerratio = ifelse(tot_workers == 0, NA, tot_workerratio),
         tot_workerratio = ifelse(tot_fworkers == 0, tot_workers, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>%
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2017) %>% 
  left_join(sexratios, by = "tinh") %>% 
  left_join(ppn0419, by = "tinh")

dn18 <- ec_list[[19]] %>% 
  mutate(nganh_kd = as.numeric(nganh_kd),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0, NA, tot_workerratio),
         tot_workerratio = ifelse(tot_fworkers == 0, tot_workers, tot_workerratio),
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, share_f)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2018) %>% 
  left_join(sexratios, by = "tinh") %>% 
  left_join(ppn0419, by = "tinh")

dn <- bind_rows(dn01, dn02, dn03, dn04, dn05, dn06, dn07, dn08, dn09, dn10, dn11, dn12, dn13, dn14, dn15, dn16, dn17, dn18)
