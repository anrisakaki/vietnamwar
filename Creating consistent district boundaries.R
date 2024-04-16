district <- bombs_district %>% 
  select(districtname, provincename, province, district) %>% 
  mutate(ward02 = as.numeric(str_sub(district, -2)))

geoid_district <- lapply(geoid_list, function(i) {
  i %>%
    select(provname, distname, contains("prov"), contains("dist")) %>%
    distinct()
})
