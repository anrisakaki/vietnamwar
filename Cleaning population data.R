# Sex ratio in 1979 and merging with bombing data 

ppn79 <- left_join(ppn79, bombs_provcodes, by = "provincename")

province79 <- ppn79 %>% select(Tinh79, provincename) %>% distinct()

bombs_province79 <- bombs_province %>% select(provincename, tot_bomb, area_sum)

bombs_province79 <- left_join(province79, bombs_province79, by = "provincename") %>% 
  group_by(Tinh79) %>% 
  summarise(tot_bomb = sum(tot_bomb),
            area_sum = sum(area_sum)) %>% 
  mutate(log_tot_bomb = log(tot_bomb),
         tot_bomb_per = tot_bomb/area_sum,
         log_tot_bmr_per = log(tot_bomb_per))

prov_ppn79 <- ppn79 %>% 
  mutate(m_population = ifelse(is.na(m_population), 0, m_population),
         f_population = ifelse(is.na(f_population), 0, f_population)) %>% 
  group_by(Tinh79) %>% 
  summarise(m_population = sum(m_population) * 1000,
            f_population= sum(f_population) * 1000) %>% 
  mutate(sexratio = m_population/f_population)

sexratio_prov79 <- merge(prov_ppn79, bombs_province79, by = "Tinh79")

# Merging 1979 sex ratio data with FLFP in 1989

prov_ppn79a <- ppn79 %>% 
  mutate(m_population = ifelse(is.na(m_population), 0, m_population),
         f_population = ifelse(is.na(f_population), 0, f_population)) %>% 
  group_by(geo1_vn) %>% 
  summarise(m_population = sum(m_population) * 1000,
            f_population= sum(f_population) * 1000) %>% 
  mutate(sexratio = m_population/f_population)
