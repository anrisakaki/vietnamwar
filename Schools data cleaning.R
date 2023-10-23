###########################
# Cleaning education data #
###########################

schools <- list(educ_65, educ_67, educ_72) %>% 
  reduce(full_join, by = "tinh_old") %>% 
  select(-c("tinh_old", "X")) %>% 
  rename(provname2002 = tinh)

schools_long <- schools %>% 
  rename(y_1965 = schools_65,
         y_1966 = schools_66,
         y_1967 = schools_67,
         y_1968 = schools_68,
         y_1972 = School_72) %>% 
  pivot_longer(!tinh, names_to = "year", values_to = "count") %>% 
  mutate(year = as.numeric(sub("y_", "", year))) %>%  
  filter(!grepl("students", schools_long$year))  

prov <- district %>% 
  select(prov2002, provname2002) %>% 
  distinct() %>% 
  filter(!is.na(prov2002)) %>% 
  mutate(provname2002 = str_remove(provname2002, "^(Thành phố|Tỉnh) ")) %>% 
  rename(tinh = prov2002)

schools <- left_join(schools, prov, by = "provname2002") %>% 
  mutate(tinh = ifelse(provname2002 == "Quảng Nam", 503, tinh),
         tinh = ifelse(provname2002 == "      Bình Phước", 707, tinh),
         diff = School_72 - schools_65) %>% 
  mutate(tinh = ifelse(students_72 == 6.6, 821, tinh))

# Matching schools data with bombing data 

schools <- list(schools, thor_prov, bombs_prov) %>% 
  reduce(full_join, by = "tinh")
