geoid_district <- lapply(geoid_list, function(i) {
  i %>%
    select(provname, distname, contains("prov"), contains("dist")) %>%
    distinct()
})

district0203 <- bombs_district %>% 
  select(districtname, provincename, province, district, tot_bmr, south_corrected) %>% 
  mutate(huyen = as.numeric(str_sub(district, -2))) %>% 
  rename(tinh = province) %>% 
  select(-district) %>% 
  filter(!is.na(tinh))

prov0203 <- bombs_province_miguel %>% 
  select(province, tot_bmr) %>% 
  rename(tinh = province,
         tot_bmr_prov = tot_bmr) %>% 
  filter(!is.na(tinh))
