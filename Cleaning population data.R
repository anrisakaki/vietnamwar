ppn_total <- ppn %>% 
  pivot_longer(!Tinh, names_to = "Year", values_to = "Total_ppn") %>% 
  mutate(Year = as.numeric(str_extract(Year, "\\d+")))  

male_prewarppn <- male_prewarppn %>% 
  select(-X) %>%
  pivot_longer(!Tinh, names_to = "Year", values_to = "Male_ppn") %>%
  mutate(Year = as.numeric(str_extract(Year, "\\d+")))
