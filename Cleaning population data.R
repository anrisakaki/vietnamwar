prov6576 <- prov6306 %>% select(Tinh65, Tinh76) %>% distinct() %>% rename(Tinh63 = Tinh65)
prov7606 <- prov6306 %>% select(Tinh76, Tinh06) %>% distinct()

m_ppn5763 <- prov_m_ppn5763 %>% 
  select(-Tinh63) %>% 
  pivot_longer(!Tinh76, names_to = "Year", values_to = "Male_ppn") %>% 
  mutate(Year = as.numeric(str_extract(Year, "\\d+"))) %>% 
  mutate(Male_ppn = as.numeric(gsub(",", "", Male_ppn)))  %>% 
  filter(!is.na(Year),
         !is.na(Tinh76)) %>% 
  mutate(Male_ppn = ifelse(is.na(Male_ppn), 0, Male_ppn)) %>% 
  group_by(Tinh76, Year) %>% 
  summarise(Male_ppn = sum(Male_ppn)) %>% 
  mutate(Male_ppn = ifelse(Male_ppn == 0, NA, Male_ppn)) %>% 
  ungroup()

m_ppn7680 <- prov_m_ppn7680 %>% 
  rename(Tinh76 = Tinh) %>% 
  pivot_longer(!Tinh76, names_to = "Year", values_to = "Male_ppn") %>% 
  mutate(Year = as.numeric(str_extract(Year, "\\d+")),
         Male_ppn = as.numeric(gsub(",", "", Male_ppn))) %>% 
  filter(!is.na(Year),
         !Tinh76 == "Total")  

prov_ppn5776 <- prov_ppn5776 %>% 
  select(-Tinh63) %>% 
  pivot_longer(!Tinh76, names_to = "Year", values_to = "Total_ppn") %>% 
  mutate(Year = as.numeric(str_extract(Year, "\\d+"))) %>% 
  mutate(Total_ppn = as.numeric(gsub(",", "", Total_ppn)))  %>% 
  filter(!is.na(Year),
         !is.na(Tinh76)) %>% 
  mutate(Total_ppn = ifelse(is.na(Total_ppn), 0, Total_ppn)) %>% 
  group_by(Tinh76, Year) %>% 
  summarise(Total_ppn = sum(Total_ppn)) %>% 
  mutate(Total_ppn = ifelse(Total_ppn == 0, NA, Total_ppn)) %>% 
  ungroup()  

prov_ppn7680 <- prov_ppn7680 %>% 
  rename(Tinh76 = Tinh) %>% 
  pivot_longer(!Tinh76, names_to = "Year", values_to = "Total_ppn") %>% 
  mutate(Year = as.numeric(str_extract(Year, "\\d+")),
         Total_ppn = as.numeric(gsub(",", "", Total_ppn))) %>% 
  filter(!is.na(Year),
         !Tinh76 == "Total")   

m_ppn5780 <- bind_rows(m_ppn5763, m_ppn7680)
prov_ppn5780 <- bind_rows(prov_ppn5776, prov_ppn7680)

prov_ppn5780 <- full_join(prov_ppn5780, m_ppn5780, by = c("Tinh76", "Year")) %>% 
  arrange(Tinh76) %>% 
  mutate(mshare = Male_ppn/Total_ppn)

