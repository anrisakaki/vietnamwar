# Creating consistent district boundaries 

districts03 <- ec_list[[4]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh03 = tinh, huyen03 = huyen, xa03 = xa)
districts18 <- ec_list[[19]] %>% select(tinh, huyen, xa, ma_thue)

geo03018 <- left_join(districts03, districts18, by = "ma_thue") %>% 
  select(tinh03, huyen03, xa03, tinh, huyen, xa) %>% 
  group_by(tinh03, huyen03, xa03) %>% 
  mutate(geoid03 = cur_group_id()) %>% 
  group_by(tinh, huyen, xa) %>% 
  mutate(geoid18 = cur_group_id()) %>% 
  filter(!is.na(tinh) & !is.na(huyen03) & !is.na(xa03)) %>% 
  group_by(tinh03, huyen03, xa03, tinh, huyen, xa, geoid03, geoid18) %>%
  summarize(count = n()) %>%
  group_by(tinh03, huyen03, xa03, geoid03) %>%
  slice_max(order_by = count) %>% 
  ungroup() %>% 
  select(geoid03, geoid18, tinh03, huyen03, xa03, tinh, huyen, xa)

