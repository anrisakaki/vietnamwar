dhs97 <- dhc_97 %>% 
  rename(age = v012,
         region = v024,
         educ = v133,
         hhsize = v136,
         nchildren = v201,
         ) %>% 
  mutate(female = ifelse(v151 == 2, 1, 0))