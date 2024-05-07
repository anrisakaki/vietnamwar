load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")
load("province_bmr_sum02.Rda")
load("prov02_vhlss.Rda")
load("prov04_vhlss.Rda")

sexratio01 <- prov02_vhlss %>% select(tinh, sex_ratio)
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
    left_join(sexratios, by = "tinh")
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
  left_join(sexratio01, by = "tinh")

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
  left_join(sexratio01, by = "tinh")

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
  left_join(province_bmr_sum02, by = "tinh") %>% 
  left_join(sexratio03, by = "tinh")

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

namsxkd16 <- ec_list[[18]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2, lhdn, namsxkd)

dn16 <- ec_list[[17]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21,
         dir_yob = namsinh,
         dir_ethnicity = dantoc) %>% 
  left_join(namsxkd16, by = c("tinh", "huyen", "xa", "ma_thue", "ma_thue2", "lhdn")) %>% 
  mutate(nganh_kd = as.numeric(nganh_kd),
         tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         share_f = tot_fworkers/tot_workers,
         share_f = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, share_f),
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0, NA, tot_workerratio),
         tot_workerratio = ifelse(tot_fworkers == 0, tot_workers, tot_workerratio),
         female_dir = ifelse(gioitinh == 2, 1, 0),
         female_dir = ifelse(female_dir == 0 & gioitinh == 0 | quoctich != "VN", NA, female_dir)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio, share_f, gioitinh, quoctich, female_dir, dir_yob, dir_ethnicity, namsxkd) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 44, 1, 0),
         manu = ifelse(nganh_kd > 323 & nganh_kd < 4100, 1, 0),
         year = 2016) %>% 
  left_join(sexratios, by = "tinh")

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
  left_join(sexratios, by = "tinh")

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
  left_join(sexratios, by = "tinh")

dn <- bind_rows(dn01, dn02, dn03, dn04, dn05, dn06, dn07, dn08, dn09, dn10, dn11, dn12, dn13, dn14, dn15, dn16, dn17, dn18)
