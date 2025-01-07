dhs97 <- dhs97 %>% 
  mutate(sprovin = ifelse(sprovin == 22, 1, sprovin),
         wife = ifelse(v150 == 2, 1, 0),
         spouse_age_diff = v152 - v012) %>% 
  left_join(bombs_province97, by = "sprovin")

