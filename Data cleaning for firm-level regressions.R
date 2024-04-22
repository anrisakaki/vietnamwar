load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")

dn0203_fn <- function(i) {
  i %>% 
    rename(nworkers = ld11,
           fworkers = ld12) %>% 
    mutate(tinh = ifelse(tinh == 105, 101, tinh),
           fworkers = ifelse(is.na(fworkers), 0, fworkers), 
           workerratio = (nworkers-fworkers)/nworkers,
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f),
           workerratio = ifelse(fworkers < 1, nworkers, workerratio), 
           across(c(tinh, huyen, xa), as.numeric)) %>% 
    filter(nworkers > 0) %>% 
    select(tinh, ma_thue, nganh_kd, lhdn, nworkers, fworkers, workerratio, share_f)
}

dn0410_fn <- function(i) {
  i %>% 
    rename(nworkers = ld11,
           fworkers = ld12) %>% 
    mutate(tinh = ifelse(tinh == 28, 1, tinh),
           fworkers = ifelse(is.na(fworkers), 0, fworkers), 
           workerratio = (nworkers-fworkers)/nworkers,
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f),
           workerratio = ifelse(fworkers < 1, nworkers, workerratio), 
           across(c(tinh, huyen, xa), as.numeric)) %>% 
    filter(nworkers > 0) %>% 
    select(tinh, ma_thue, nganh_kd, lhdn, nworkers, fworkers, workerratio, share_f) %>% 
    left_join(province_bmr_sum0419, by = "tinh")
}

dn00 <- ec_list[[1]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ld611,
         fworkers = ld612) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0003, by = "tinh")

dn01 <- ec_list[[2]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ldc11,
         fworkers = ldc12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0003, by = "tinh")

dn02 <- ec_list[[3]] %>% 
  dn0210_fn() %>% 
  left_join(province_bmr_sum0003, by = "tinh")

dn03 <- ec_list[[4]] %>% 
  dn0210_fn() %>% 
  left_join(province_bmr_sum, by = "tinh")

dn04 <- ec_list[[5]] %>% dn0410_fn()

dn05 <- ec_list[[6]] %>% dn0410_fn()

dn06 <- ec_list[[7]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld11,
         fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn07 <- ec_list[[8]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld11,
         fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn08 <- ec_list[[9]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld11,
         fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn09 <- ec_list[[10]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld11,
         fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn10 <- ec_list[[11]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld11,
         fworkers = ld12) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn11 <- ec_list[[12]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = tsld,
         fworkers = tsldnu) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn12 <- ec_list[[13]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = tsld,
         fworkers = tsldnu) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn13 <- ec_list[[14]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = tsld,
         fworkers = tsldnu) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn14 <- ec_list[[15]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = tsld,
         fworkers = tsldnu) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn15 <- ec_list[[16]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = tsld,
         fworkers = tsldnu) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")
