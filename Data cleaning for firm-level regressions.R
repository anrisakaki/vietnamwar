load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")

sexratio01 <- prov02_vhlss %>% select(tinh02, sex_ratio) %>% rename(tinh = tinh02)
sexratio03 <- prov04_vhlss %>% select(tinh, sex_ratio)

dn_fn <- function(i) {
  i %>% 
    mutate(workerratio = (nworkers-fworkers)/fworkers,
           share_f = fworkers/nworkers,
           tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
           across(c(tinh, huyen, xa), as.numeric),
           workerratio = ifelse(nworkers == 0 | fworkers == 0, NA, workerratio),
           tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
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
  left_join(province_bmr_sum02, by = "tinh") %>%
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
  left_join(sexratio01, by = "tinh")

dn02 <- ec_list[[3]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
  left_join(sexratio01, by = "tinh")

dn03 <- ec_list[[4]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>%  
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
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
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn05 <- ec_list[[6]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn06 <- ec_list[[7]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn07 <- ec_list[[8]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn08 <- ec_list[[9]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
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
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn10 <- ec_list[[11]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn11 <- ec_list[[12]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn12 <- ec_list[[13]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn13 <- ec_list[[14]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn14 <- ec_list[[15]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn15 <- ec_list[[16]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(province_bmr_sum2, by = "tinh")

dn16 <- ec_list[[17]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(sexratios, by = "tinh")

dn17 <- ec_list[[18]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>%
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(sexratios, by = "tinh")

dn18 <- ec_list[[19]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio) %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) %>% 
  left_join(sexratios, by = "tinh")
