load("district_bmr_sum.Rda")
load("province_bmr_sum.Rda")

dn_fn <- function(i) {
  i %>% 
    mutate(fworkers = ifelse(is.na(fworkers), 0, fworkers), 
           workerratio = (nworkers-fworkers)/nworkers,
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f),
           workerratio = ifelse(fworkers < 1, nworkers, workerratio), 
           across(c(tinh, huyen, xa), as.numeric)) %>% 
    filter(nworkers > 0) %>% 
    select(tinh, ma_thue, nganh_kd, lhdn, nworkers, fworkers, workerratio, share_f)
}

dn01 <- ec_list[[2]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         ldc21 = ifelse(is.na(ldc21), 0, ldc21),
         ldc22 = ifelse(is.na(ldc22), 0, ldc22),
         nworkers = ldc11 - ldc21,
         fworkers = ldc12 - ldc22,
         south = ifelse(tinh > 407, 1, 0)) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0002, by = "tinh") %>% 
  mutate( south = ifelse(tinh > 407, 1, 0)) 

dn02 <- ec_list[[3]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0002, by = "tinh") %>% 
  mutate( south = ifelse(tinh > 407, 1, 0)) 

dn03 <- ec_list[[4]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>%  
  dn_fn() %>% 
  left_join(province_bmr_sum, by = "tinh")

dn04 <- ec_list[[5]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn05 <- ec_list[[6]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn06 <- ec_list[[7]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn07 <- ec_list[[8]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn08 <- ec_list[[9]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn09 <- ec_list[[10]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn10 <- ec_list[[11]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn11 <- ec_list[[12]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn12 <- ec_list[[13]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn13 <- ec_list[[14]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn14 <- ec_list[[15]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")

dn15 <- ec_list[[16]] %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  dn_fn() %>% 
  left_join(province_bmr_sum0419, by = "tinh")
