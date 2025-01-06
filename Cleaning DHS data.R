dhs97 <- dhs97 %>% 
  mutate(sprovin = ifelse(sprovin == 22, 1, sprovin)) %>% 
  left_join(bombs_province97, by = "sprovin")
