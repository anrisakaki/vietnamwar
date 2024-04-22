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
           across(c(tinh, huyen, xa), as.numeric),
           south = ifelse(tinh > 407, 1, 0)) %>% 
    filter(nworkers > 0) %>% 
    select(tinh, ma_thue, nganh_kd, lhdn, nworkers, fworkers, workerratio, share_f, south)
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

dn1118_fn <- function(i) {
  i %>% 
    rename(nworkers = tsld,
           fworkers = tsldnu) %>% 
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
  mutate( south = ifelse(tinh > 407, 1, 0)) %>% 
  left_join(province_bmr_sum0003, by = "tinh")

dn01 <- ec_list[[2]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ldc11,
         fworkers = ldc12) %>% 
  dn_fn() %>% 
  mutate( south = ifelse(tinh > 407, 1, 0)) %>% 
  left_join(province_bmr_sum0003, by = "tinh")

dn02 <- ec_list[[3]] %>% 
  dn0210_fn() %>% 
  left_join(province_bmr_sum0003, by = "tinh") %>% 
  mutate( south = ifelse(tinh > 407, 1, 0)) 

dn03 <- ec_list[[4]] %>% 
  dn0210_fn() %>% 
  left_join(province_bmr_sum, by = "tinh")

dn04 <- ec_list[[5]] %>% dn0410_fn()

dn05 <- ec_list[[6]] %>% dn0410_fn()

dn06 <- ec_list[[7]] %>% dn0410_fn()

dn07 <- ec_list[[8]] %>% dn0410_fn()

dn08 <- ec_list[[9]] %>% dn0410_fn()

dn09 <- ec_list[[10]] %>% dn0410_fn()

dn10 <- ec_list[[11]] %>% dn0410_fn()

dn11 <- ec_list[[12]] %>% dn1118_fn()

dn12 <- ec_list[[13]] %>% dn1118_fn()

dn13 <- ec_list[[14]] %>% dn1118_fn()

dn14 <- ec_list[[15]] %>% dn1118_fn()

dn15 <- ec_list[[16]] %>% dn1118_fn()

dn16 <- ec_list[[17]] %>% dn1118_fn()

dn17 <- ec_list[[18]] %>% dn1118_fn()

dn18 <- ec_list[[19]] %>% dn1118_fn()
