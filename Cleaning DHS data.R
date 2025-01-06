dhs97 <- dhs97 %>% 
  mutate(sprovin = ifelse(sprovin == 22, 1, sprovin)) %>% 
  left_join(bombs_province97, by = "sprovin")

dhs_summary <- dhs97 %>% 
  filter(v012 > (1997-(1975-15))) %>% 
  group_by(sprovin) %>% 
  summarise(firstmarriage = weighted.mean(v511, v005),
            tot_bmr_prov = mean(tot_bmr_prov))
