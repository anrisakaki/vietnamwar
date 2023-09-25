#######################
# cleaning VHLSS data #
#######################

m1_06 <- m1_06 %>% 
  rename(birth_year = m1ac4b,
         hk = m1ac8) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0)) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, birth_year, female, hk)

m2a_06 <- m2a_06 %>%
  rename(educ = m2ac1) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, educ)

m4a_06 <- m4a_06 %>% 
  rename(work = m4ac2) %>%
  mutate(income = m4ac11 +m4ac12f) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, work, income)

weights_06 <- weights_vhlss %>%
  select(tinh, huyen, xa, wt45) %>% 
  rename("hhwt" = wt45) %>% 
  distinct()

vhlss06 <- list(m1_06, m2a_06, m4a_06) %>% 
  reduce(full_join, by = c("tinh", "huyen", "xa", "diaban", "hoso", "matv")) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  group_by(tinh, huyen, xa, diaban, hoso, matv) %>%  
  mutate(ivid = cur_group_id())

# Adding weights 

vhlss06 <- left_join(vhlss06, weights_06, by = c("tinh", "huyen", "xa"))
