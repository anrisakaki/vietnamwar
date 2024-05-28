load("dn.Rda")

####################
# INDUSTRY SUMMARY #
####################

dn_ind_sexratio <- dn %>% 
  mutate(mworkers = nworkers - fworkers) %>% 
  group_by(year, nganh_kd2) %>% 
  summarise(
    total_f = sum(fworkers, na.rm = T),
    total_m = sum(mworkers, na.rm = T)
  ) %>% 
  mutate(sexratio = total_m/total_f) %>% 
  filter(year < 2016)

ind01 <- dn_ind_sexratio %>% filter(year == 2001) %>% 
  mutate(Industry = case_when(nganh_kd2 == 15 ~ 'Manufacture of food and beverages',
                              nganh_kd2 == 17 ~ 'Manufacture of textiles',
                              nganh_kd2 == 18 ~ 'Manufacture of wearing apparel',
                              nganh_kd2 == 19 ~ 'Manufacture of leather products',
                              nganh_kd2 == 30 ~ 'Manufacture of office, accounting and computing machinery',
                              nganh_kd2 == 31 ~ 'Manufacture of electrical machinery and apparatus n.e.c.',
                              TRUE ~ NA_character_))

ind10 <- dn_ind_sexratio %>% filter(year == 2010) %>% 
  mutate(Industry = case_when(nganh_kd2 == 14 ~ 'Manufacture of wearing apparel',
                              nganh_kd2 == 15 ~ 'Manufacture of leather and related products',
                              nganh_kd2 == 26 ~ 'Manufacture of electronic, computer and optical products',
                              nganh_kd2 == 32 ~ 'Other processing and producing',
                              nganh_kd2 == 88 ~ 'Activities auxiliary to society (non-concentrated)',
                              TRUE ~ NA_character_))

################
# BY FIRM TYPE #
################

