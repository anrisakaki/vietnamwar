dhs97 <- dhs97 %>% 
  mutate(sprovin = ifelse(sprovin == 22, 1, sprovin),
         wife = ifelse(v150 == 2, 1, 0),
         spouse_age_diff = v152 - v012,
         age_75 = (1975 - (1997-v012))) %>% 
  left_join(bombs_province97, by = "sprovin")

dhs02 <- dhs02 %>% 
  mutate(sprovin = ifelse(sprovin == 105, 101, sprovin),
         wife = ifelse(v150 == 2, 1, 0),
         spouse_age_diff = v152 - v012,
         age_75 = (1975 - (2002-v012))) %>% 
  left_join(bombs_province02, by = c("sprovin" = "geo1_vn1999"))

