load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")

dn_fn <- function(i) {
  i %>% 
    mutate(workerratio = (nworkers-fworkers)/fworkers,
           share_f = fworkers/nworkers,
           tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
           across(c(tinh, huyen, xa), as.numeric),
           workerratio = ifelse(nworkers == 0 | fworkers == 0, NA, workerratio),
           tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio))
}

dn01 <- ec_list[[2]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         ldc21 = ifelse(is.na(ldc21), 0, ldc21),
         ldc22 = ifelse(is.na(ldc22), 0, ldc22),
         nworkers = ldc11 - ldc21,
         fworkers = ldc12 - ldc22,
         south = ifelse(tinh > 407, 1, 0)) %>% 
  rename(tot_workers = ld13,
         tot_fworkers = ld14,
         namsxkd = namtl) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, namsxkd, nganh_kd, lhdn, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f) %>% 
  left_join(province_bmr_sum0002, by = "tinh") %>% 
  mutate( south = ifelse(tinh > 407, 1, 0)) 

dn02 <- ec_list[[3]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14,
         namsxkd = namkd) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, namsxkd, nganh_kd, lhdn, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f) %>%
  left_join(province_bmr_sum0002, by = "tinh") %>% 
  mutate( south = ifelse(tinh > 407, 1, 0)) 

dn03 <- ec_list[[4]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14,
         pretax_profit = kqkd7) %>%  
  dn_fn() %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f, pretax_profit) %>%
  left_join(province_bmr_sum, by = "tinh")

dn04 <- ec_list[[5]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         export = ifelse(xk == 1, 1, 0),
         export = ifelse(is.na(export), 0, export)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14,
         pretax_profit = kqkd8) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, export, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f, pretax_profit) %>%  
  left_join(province_bmr_sum0419, by = "tinh")

dn05 <- ec_list[[6]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         export = ifelse(co_xnk == 1, 1, 0),
         export = ifelse(is.na(export), 0, export)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34,
         tot_workers = ld13,
         tot_fworkers = ld14,
         namsxkd = nam_sxkd,
         pretax_profit = kqkd8) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, namsxkd, export, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f, pretax_profit) %>%  
  left_join(province_bmr_sum0419, by = "tinh")

dn06 <- ec_list[[7]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         export = ifelse(co_xnk == 1, 1, 0),
         export = ifelse(is.na(export), 0, export)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14,
         pretax_profit = kqkd8) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, export, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f, pretax_profit) %>%  
  left_join(province_bmr_sum0419, by = "tinh")

dn07 <- ec_list[[8]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         export = ifelse(co_xnk == 1, 1, 0),
         export = ifelse(is.na(export), 0, export)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14,
         pretax_profit = kqkd9) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, namsxkd, export, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f, pretax_profit) %>%  
  left_join(province_bmr_sum0419, by = "tinh")

dn08 <- ec_list[[9]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14,
         pretax_profit = kqkd9) %>% 
  dn_fn() %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, namsxkd, export, tot_workers, tot_fworkers, nworkers, fworkers,
         workerratio, tot_workerratio, share_f, pretax_profit) %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn09 <- ec_list[[10]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn10 <- ec_list[[11]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24,
         tot_workers = ld13,
         tot_fworkers = ld14) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn11 <- ec_list[[12]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn12 <- ec_list[[13]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn13 <- ec_list[[14]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn14 <- ec_list[[15]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn15 <- ec_list[[16]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22,
         tot_workers = ld11,
         tot_fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn16 <- ec_list[[17]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio) %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn17 <- ec_list[[18]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio) %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn18 <- ec_list[[19]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(tot_workers = ld11,
         tot_fworkers = ld21) %>% 
  mutate(tot_workerratio = (tot_workers - tot_fworkers) /tot_fworkers,
         across(c(tinh, huyen, xa), as.numeric),
         tot_workerratio = ifelse(tot_workers == 0 | tot_fworkers == 0, NA, tot_workerratio)) %>% 
  select(tinh, ma_thue, nganh_kd, lhdn, tot_workers, tot_fworkers, tot_workerratio) %>% 
  left_join(province_bmr_sum0419, by = "tinh")
