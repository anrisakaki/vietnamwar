############################
# Province-level summaries #
############################

dn0002_prov_fn <- function(i) {
  i %>%
    group_by(tinh) %>%
    mutate(tinh = ifelse(tinh == 105, 101, tinh),
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
    filter(nworkers > 0) %>% 
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      share_f = mean(share_f, na.rm = T)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
      south = ifelse(tinh > 407, 1, 0)
    ) %>% 
    left_join(province_bmr_sum0002, by = "tinh")
}

dn03_prov_fn <- function(i) {
  i %>%
    group_by(tinh) %>%
    mutate(tinh = ifelse(tinh == 105, 101, tinh),
           share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      share_f = mean(share_f, na.rm = T)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
    ) %>% 
    left_join(province_bmr_sum, by = "tinh")
}

dn_prov_fn <- function(i) {
  i %>%
    mutate(across(tinh, as.double),
           tinh = ifelse(tinh == 28, 1, tinh)) %>% 
    rename(nworkers =  ld23,
           fworkers = ld24) %>% 
    mutate(share_f = fworkers/nworkers,
           share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
    group_by(tinh) %>%
    summarise(
      nworkers = sum(nworkers, na.rm = TRUE),
      fworkers = sum(fworkers, na.rm = TRUE),
      share_f = mean(share_f, na.rm = T)
    ) %>%
    mutate(
      workerratio = (nworkers - fworkers) / fworkers,
    ) %>% 
    left_join(province_bmr_sum0419, by = "tinh")
}

dn1115_prov_fn <- function(i) {
  i %>%
    mutate(across(tinh, as.double),
           tinh = ifelse(tinh == 28, 1, tinh)) %>% 
    rename(ld23 = ld21,
           ld24 = ld22)
}

dn01_prov <- ec_list[[2]] %>% 
  mutate(ldc21 = ifelse(is.na(ldc21), 0, ldc21),
         ldc22 = ifelse(is.na(ldc22), 0, ldc22),
         nworkers = ldc11 - ldc21,
         fworkers = ldc12 - ldc22) %>% 
  dn0002_prov_fn() %>% 
  mutate(year = 2001)

dn02_prov <- ec_list[[3]] %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn0002_prov_fn() %>% 
  mutate(year = 2002)

dn03_prov <- ec_list[[4]] %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  dn03_prov_fn() %>% 
  mutate(year = 2003)
  
dn04_prov <- ec_list[[5]] %>%
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  mutate(across(tinh, as.numeric),
         share_f = fworkers/nworkers,
         share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
  group_by(tinh) %>%
  summarise(
    nworkers = sum(nworkers, na.rm = TRUE),
    fworkers = sum(fworkers, na.rm = TRUE),
    share_f = mean(share_f, na.rm = T)
  ) %>%
  mutate(
    workerratio = (nworkers - fworkers) / fworkers,
  ) %>% 
  left_join(province_bmr_sum0419, by = "tinh") %>% 
  mutate(year = 2004)

dn05_prov <- ec_list[[6]] %>% 
  rename(nworkers = ld33,
         fworkers = ld34) %>% 
  mutate(across(tinh, as.numeric),
         share_f = fworkers/nworkers,
         share_f = ifelse(fworkers < 1, 0, share_f)) %>% 
  group_by(tinh) %>%
  summarise(
    nworkers = sum(nworkers, na.rm = TRUE),
    fworkers = sum(fworkers, na.rm = TRUE),
    share_f = mean(share_f, na.rm = T)
  ) %>%
  mutate(
    workerratio = (nworkers - fworkers) / fworkers,
  ) %>% 
  left_join(province_bmr_sum0419, by = "tinh") %>% 
  mutate(year = 2005)

dn06_prov <- dn_prov_fn(ec_list[[7]]) %>% mutate(year = 2006)
dn07_prov <- dn_prov_fn(ec_list[[8]]) %>% mutate(year = 2007)
dn08_prov <- dn_prov_fn(ec_list[[9]]) %>% mutate(year = 2008)
dn09_prov <- dn_prov_fn(ec_list[[10]]) %>% mutate(year = 2009)
dn10_prov <- dn_prov_fn(ec_list[[11]]) %>% mutate(year = 2010)
dn11_prov <- dn1115_prov_fn(ec_list[[12]]) %>% dn_prov_fn() %>% mutate(year = 2011)
dn12_prov <- dn1115_prov_fn(ec_list[[13]]) %>% dn_prov_fn() %>% mutate(year = 2012)
dn13_prov <- dn1115_prov_fn(ec_list[[14]]) %>% dn_prov_fn() %>% mutate(year = 2013)
dn14_prov <- dn1115_prov_fn(ec_list[[15]]) %>% dn_prov_fn() %>% mutate(year = 2014)
dn15_prov <- dn1115_prov_fn(ec_list[[16]]) %>% dn_prov_fn() %>% mutate(year = 2015)

dn_prov <- bind_rows(dn04_prov, dn05_prov, dn06_prov,
                     dn07_prov, dn08_prov, dn09_prov, dn10_prov, dn11_prov, dn12_prov,
                     dn13_prov, dn14_prov, dn15_prov, dn16_prov, dn17_prov, dn18_prov)
save(dn_prov, file = "dn_prov.Rda")
