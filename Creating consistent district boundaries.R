district <- bombs_district %>% 
  select(districtname, provincename, province, district) %>% 
  mutate(ward02 = as.numeric(str_sub(district, -2)))

geoid <- geoid_list %>% 
  reduce(full_join, by = c("provname", "distname", "wardname")) %>% 
  select(provname, distname, wardname, everything()) %>% 
  distinct()

geo0418a <- geoid %>%
  select(provname, distname, wardname, prov04, dist04, ward04, prov18, dist18, ward18)

geo0518 <- geoid %>%
  select(provname, distname, wardname, prov05, dist05, ward05, prov18, dist18, ward18) %>%
  mutate(samepd = ifelse(prov05 == prov18 & dist05 == dist18, 1, 0))

id18 <- geoid %>% 
  select(provname, distname, prov18, dist18) %>%
  distinct() %>% 
  mutate(provname2018 = str_replace(provname, "(Tỉnh|Thành phố) ", "")) %>% 
  full_join(vnmap2, by = c("provname2018" = "NAME_1", "distname" = "distname2018"))

# Creating consistent district boundaries 

districts01 <- ec_list[[2]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh01 = tinh, huyen01 = huyen, xa01 = xa)
districts02 <- ec_list[[3]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh02 = tinh, huyen02 = huyen, xa02 = xa)
districts03 <- ec_list[[4]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh03 = tinh, huyen03 = huyen, xa03 = xa)
districts04 <- ec_list[[5]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh04 = tinh, huyen04 = huyen, ward2004 = xa)
districts05 <- ec_list[[6]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh05 = tinh, huyen05 = huyen, ward2005 = xa)
districts06 <- ec_list[[7]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh06 = tinh, huyen06 = huyen, ward2006 = xa)
districts07 <- ec_list[[8]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh07 = tinh, huyen07 = huyen, ward2007 = xa)
districts08 <- ec_list[[9]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh08 = tinh, huyen08 = huyen, ward2008 = xa)
districts09 <- ec_list[[10]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh09 = tinh, huyen09 = huyen, ward2009 = xa)
districts10 <- ec_list[[11]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh10 = tinh, huyen10 = huyen, ward2010 = xa)
districts11 <- ec_list[[12]] %>% select(tinh, huyen, xa, ma_thue, ma_thue2) %>% 
  mutate(ma_thue = paste(ma_thue, ma_thue2, sep = "")) %>% 
  rename(tinh11 = tinh, huyen11 = huyen, ward2011 = xa)
districts12 <- ec_list[[13]] %>% select(tinh, huyen, xa, ma_thue) %>% 
  rename(tinh12 = tinh, huyen12 = huyen, ward2012 = xa)
districts13 <- ec_list[[14]] %>% select(tinh, huyen, xa, ma_thue) %>% 
  rename(tinh13 = tinh, huyen13 = huyen, ward2013 = xa)
districts14 <- ec_list[[15]] %>% select(tinh, huyen, xa, ma_thue) %>% 
  rename(tinh14 = tinh, huyen14 = huyen, ward2014 = xa)
districts15 <- ec_list[[16]] %>% select(tinh, huyen, xa, ma_thue) %>% 
  rename(tinh15 = tinh, huyen15 = huyen, ward2015 = xa)
districts16 <- ec_list[[17]] %>% select(tinh, huyen, xa, ma_thue) %>% 
  rename(tinh16 = tinh, huyen16 = huyen, ward2016 = xa)
districts17 <- ec_list[[18]] %>% select(tinh, huyen, xa, ma_thue) %>% 
  rename(tinh17 = tinh, huyen17 = huyen, ward2017 = xa)
districts18 <- ec_list[[19]] %>% select(tinh, huyen, xa, ma_thue)

geo0218 <- left_join(districts02, districts18, by = "ma_thue") %>% 
  select(tinh02, huyen02, xa02, tinh, huyen, xa) %>% 
  group_by(tinh02, huyen02, xa02) %>% 
  mutate(geoid02 = cur_group_id()) %>% 
  group_by(tinh, huyen, xa) %>% 
  mutate(geoid18 = cur_group_id()) %>% 
  filter(!is.na(tinh) & !is.na(huyen02) & !is.na(xa02)) %>% 
  group_by(tinh02, huyen02, xa02, tinh, huyen, xa, geoid02, geoid18) %>%
  summarize(count = n()) %>%
  group_by(tinh02, huyen02, xa02, geoid02) %>%
  slice_max(order_by = count) %>% 
  ungroup() %>% 
  select(geoid02, geoid18, tinh02, huyen02, xa02, tinh, huyen, xa) %>% 
  rename(prov2002 = tinh02, dist2002 = huyen02)

geo0318 <- left_join(districts03, districts18, by = "ma_thue") %>% 
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
  select(geoid03, geoid18, tinh03, huyen03, xa03, tinh, huyen, xa) %>% 
  rename(prov2003 = tinh03, dist2003 = huyen03)

geo0418 <- left_join(districts04, districts18, by = "ma_thue") %>% 
  select(tinh04, huyen04, ward2004, tinh, huyen, xa) %>% 
  group_by(tinh04, huyen04, ward2004) %>% 
  mutate(geoid04 = cur_group_id()) %>% 
  group_by(tinh, huyen, xa) %>% 
  mutate(geoid18 = cur_group_id()) %>% 
  group_by(tinh04, huyen04, ward2004, tinh, huyen, xa, geoid04, geoid18) %>%
  summarize(count = n()) %>%
  group_by(tinh04, huyen04, ward2004) %>%
  slice_max(order_by = count) %>% 
  ungroup() %>% 
  filter(!is.na(tinh)) %>% 
  select(geoid04, geoid18, tinh04, huyen04, ward2004, tinh, huyen, xa) %>% 
  mutate_all(as.numeric) %>% 
  mutate(samepd = ifelse(tinh04 == tinh & huyen04 == huyen, 1, 0))

geo0518 <- left_join(districts05, districts18, by = "ma_thue") %>% 
  select(tinh05, huyen05, ward2005, tinh, huyen, xa) %>% 
  group_by(tinh05, huyen05, ward2005) %>% 
  mutate(geoid05 = cur_group_id()) %>% 
  group_by(tinh, huyen, xa) %>% 
  mutate(geoid18 = cur_group_id()) %>% 
  group_by(tinh05, huyen05, ward2005, tinh, huyen, xa, geoid05, geoid18) %>%
  summarize(count = n()) %>%
  group_by(tinh05, huyen05, ward2005) %>%
  slice_max(order_by = count) %>% 
  ungroup() %>% 
  filter(!is.na(tinh)) %>% 
  select(geoid05, geoid18, tinh05, huyen05, ward2005, tinh, huyen, xa) %>% 
  mutate_all(as.numeric) %>% 
  mutate(samepd = ifelse(tinh05 == tinh & huyen05 == huyen, 1, 0))  

geo0618 <- left_join(districts06, districts18, by = "ma_thue") %>% 
  select(tinh06, huyen06, ward2006, tinh, huyen, xa) %>% 
  group_by(tinh06, huyen06, ward2006) %>% 
  mutate(geoid06 = cur_group_id()) %>% 
  group_by(tinh, huyen, xa) %>% 
  mutate(geoid18 = cur_group_id()) %>% 
  group_by(tinh06, huyen06, ward2006, tinh, huyen, xa, geoid06, geoid18) %>%
  summarize(count = n()) %>%
  group_by(tinh06, huyen06, ward2006) %>%
  slice_max(order_by = count) %>% 
  ungroup() %>% 
  select(geoid06, geoid18, tinh06, huyen06, ward2006, tinh, huyen, xa) 
