############################
# Province-level summaries #
############################

dn_prov_fn <- function(i) {
  i %>%
    group_by(tinh) %>%
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
      share_f = fworkers/nworkers,
    )
}

dn01_prov <- ec_list[[2]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         ldc21 = ifelse(is.na(ldc21), 0, ldc21),
         ldc22 = ifelse(is.na(ldc22), 0, ldc22),
         nworkers = ldc11 - ldc21,
         fworkers = ldc12 - ldc22) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
  mutate(year = 2001)

dn02_prov <- ec_list[[3]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
  mutate(year = 2002)

dn03_prov <- ec_list[[4]] %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0)) %>% 
  mutate(year = 2003)
  
dn04_prov <- ec_list[[5]] %>%
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2004)

dn05_prov <- ec_list[[6]] %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  mutate(year = 2005)

dn06_prov <- ec_list[[7]] %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2006)  

dn07_prov <- ec_list[[8]] %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2007) 

dn08_prov <- ec_list[[9]] %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2008) 

dn09_prov <- ec_list[[10]] %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2009) 

dn10_prov <- ec_list[[11]] %>% 
  rename(nworkers = ld23,
         fworkers = ld24) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2010) 

dn11_prov <- ec_list[[12]] %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2011) 

dn12_prov <- ec_list[[13]] %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         tinh = ifelse(tinh == 28, 1, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2012) 

dn13_prov <- ec_list[[14]] %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2013) 

dn14_prov <- ec_list[[15]] %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2014) 

dn15_prov <- ec_list[[16]] %>% 
  rename(nworkers = ld21,
         fworkers = ld22) %>% 
  mutate(tinh = as.numeric(tinh),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  dn_prov_fn() %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(year = 2015) 

dn_prov <- bind_rows(dn01_prov, dn02_prov, dn03_prov, dn04_prov, dn05_prov, dn06_prov,
                     dn07_prov, dn08_prov, dn09_prov, dn10_prov, dn11_prov, dn12_prov,
                     dn13_prov, dn14_prov, dn15_prov)
save(dn_prov, file = "dn_prov.Rda")
