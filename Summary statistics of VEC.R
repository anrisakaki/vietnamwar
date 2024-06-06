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

firmtype_sum <- dn %>% 
  group_by(year) %>% 
  summarise(total = n(),
            soe = sum(soe == 1, na.rm = T),
            private = sum(private == 1, na.rm = T),
            priv_w_state = sum(priv_w_state == 1, na.rm = T),
            collective = sum(collective == 1, na.rm = T),
            foe = sum(foe == 1, na.rm = T)) %>% 
  mutate(soe_share = soe/total,
         Private = private/total,
         Private_w_Share = priv_w_state/total,
         Collective = collective/total,
         FOE = foe/total)

firmtype_long <- firmtype_sum %>% 
  select(year, Private, Private_w_Share, Collective, FOE) %>% 
  pivot_longer(
    cols = -year,
    names_to = "FirmType",
    values_to = "Share") %>% 
  mutate(FirmType = ifelse(FirmType == "Private_w_Share", "Private w/ State Capital", FirmType))

####################################
# BMR VS SHARE OF FEMALE DIRECTORS #
####################################

bmr_fdir <- dn16 %>% 
  group_by(tinh, huyen, south) %>% 
  summarise(
    n_firms = n(),
    n_fdir = sum(female_dir == 1, na.rm = T),
    tot_bmr = mean(tot_bmr)
  ) %>% 
  mutate(share_fdir = (n_fdir/n_firms)*100)

bmr_fdir_prov <- dn16 %>% 
  group_by(tinh, south) %>% 
  summarise(
    n_firms = n(),
    n_fdir = sum(female_dir == 1, na.rm = T),
    tot_bmr = mean(tot_bmr_prov_ppn)
  ) %>% 
  mutate(share_fdir = (n_fdir/n_firms)*100)
