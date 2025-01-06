load("province_bmr_sum.Rda")

ivid <- c("tinh", "huyen", "xa", "diaban", "hoso", "matv")
hhid <- c("tinh", "huyen", "xa", "diaban", "hoso")

ppn_density <- ppn_density %>% select(-Cities..provincies)

province_bmr_sum_vhlss <- province_bmr_sum %>%
  rename(prov_birth = tinh) %>% 
  ungroup() %>% 
  mutate(mean_tot_bmr_prov = mean(tot_bmr_prov, na.rm = T),
         sd_tot_bmr_prov = sd(tot_bmr_prov, na.rm = T),
         tot_bmr_prov_std = (tot_bmr_prov - mean_tot_bmr_prov)/sd_tot_bmr_prov) %>% 
  select(-c(mean_tot_bmr_prov, sd_tot_bmr_prov))

vhlss_fn <- function(i){
  
  i %>% 
    rename(sex = m1ac2,
           age = m1ac5,
           marst = m1ac8,
           prov_birth = m1ac11,
           educ = m2ac1,
           work = m4ac2,
           ethnicity = dantoc,
           hhsize = tsnguoi,
           urban = ttnt) %>% 
    group_by(tinh, huyen, xa, diaban, hoso) %>% 
    mutate(hhid = cur_group_id()) %>% 
    ungroup() %>% 
    mutate(female = ifelse(sex == 2, 1, 0),
           work = ifelse(work == 1, 1, 0),
           educ = as.numeric(educ),
           south = ifelse(tinh > 45, 1, 0),
           prewar = ifelse(m1ac4b < 1975, 1, 0),
           migrant = ifelse(tinh != prov_birth, 1, 0),
           widowed = ifelse(marst == 3, 1, 0)) %>% 
    select(hhid, tinh, huyen, xa, diaban, hoso, matv, prov_birth, female, age, prewar, marst, educ, work, ethnicity, hhsize, urban, migrant, widowed, wt45, south) %>% 
    left_join(province_bmr_sum_vhlss, by = "prov_birth") %>% 
    left_join(ppn_density, by = "tinh")
}

# 2014 

wt14 <- wt14 %>% select(-ttnt)

vhlss14 <- list(m1a_14, m2a_14, m4a_14) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_14, by = hhid) %>% 
  merge(wt14, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  vhlss_fn() %>% 
  mutate(year = 2014)

# 2016

m1a_16 <- m1a_16 %>% rename(matv = m1ama)
m2ab_16 <- m2ab_16 %>% rename(matv = m2ma)
m4a_16 <- m4a_16 %>% rename(matv = m4ama)
wt16 <- wt16 %>% select(-ttnt)

vhlss16 <- list(m1a_16, m2ab_16, m4a_16) %>% 
  reduce(merge, by = ivid) %>% 
  merge(ho1_16, by = hhid) %>% 
  merge(wt16, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  vhlss_fn() %>% 
  mutate(year = 2016)

vhlss <- bind_rows(vhlss14, vhlss16)

save(vhlss14, file = "vhlss14.Rda")
save(vhlss16, file = "vhlss16.Rda")

sum_vhlss <- vhlss %>% 
  group_by(year, south) %>% 
  summarise(n = sum(wt45),
            tot_f = sum(wt45[female == 1], na.rm = T),
            tot_m = sum(wt45[female == 0], na.rm = T),
            tot_mlf = sum(wt45[female == 0 & age > 15 & age < 65], na.rm = T),
            tot_flf = sum(wt45[female == 1 & age > 15 & age < 65], na.rm = T),
            f_work = sum(wt45[female == 1 & work == 1], na.rm = T),
            m_work = sum(wt45[female == 0 & work == 1], na.rm = T)) %>% 
  mutate(flfp = f_work/tot_flf,
         mlfp = m_work/tot_mlf)
