# Cleaning Population and Housing Census 

phc <- phc %>%
  mutate(married = ifelse(marst == 2, 1, 0),
         widowed = ifelse(marst == 4, 1, 0),
         minority = ifelse(ethnicvn > 1, 1, 0),
         literate = ifelse(lit == 2, 1, 0),
         work = ifelse(empstat == 1, 1, 0),
         disabled = ifelse(disabled == 1, 1, 0),
         female = ifelse(sex == 2, 1, 0),
         agri = ifelse(indgen == 10, 1, 0),
         age75 = 1975 - birthyr,
         migration = ifelse(migrate5 > 19, 1, 0),
         migration = ifelse(migrate5 == 0 | mig1_5_vn == 99, NA, migration)) %>% 
  filter(age != 999) %>% 
  mutate(
    age_cohort = case_when(
      age <= 10 ~ "0-10",
      age <= 15 ~ "11-15",
      age <= 20 ~ "16-20",
      age <= 25 ~ "21-25",
      age <= 30 ~ "26-30",
      age <= 35 ~ "31-35",
      age <= 40 ~ "36-40",
      age <= 45 ~ "41-45",
      age <= 50 ~ "46-50",
      age <= 55 ~ "51-55",
      age <= 60 ~ "56-60",
      age <= 65 ~ "61-65",
      age <= 70 ~ "65+"),
    age_cohort75 = case_when(
      age75 <= 10 ~ "0-10",
      age75 <= 15 ~ "11-15",
      age75 <= 20 ~ "16-20",
      age75 <= 25 ~ "21-25",
      age75 <= 30 ~ "26-30",
      age75 <= 35 ~ "31-35",
      age75 <= 40 ~ "36-40",
      age75 <= 45 ~ "41-45",
      age75 <= 50 ~ "46-50",
      age75 <= 55 ~ "51-55",
      age75 <= 60 ~ "56-60",
      age75 <= 65 ~ "61-65",
      age75 <= 70 ~ "65+"))  

# merge with bombing data 

provcodes <- phc %>% 
  select(geo1_vn, geo1_vn1989, geo1_vn1999, geo1_vn2009) %>%
  distinct()

provcodes89 <- provcodes %>% select(geo1_vn, geo1_vn1989) %>% filter(!is.na(geo1_vn1989))
provcodes99 <- provcodes %>% select(geo1_vn, geo1_vn1999) %>% filter(!is.na(geo1_vn1999))
provcodes09 <- provcodes %>% select(geo1_vn, geo1_vn2009) %>% filter(!is.na(geo1_vn2009))

bombs_provcodes89 <- bombs_province %>%
  select(provincename) %>%
  distinct() %>%
  mutate(geo1_vn = recode(provincename,
                          'Ha Noi (City)' = 704001,
                          'Hai Phong (City)' = 704031,
                          'Ha Tay' = 704001,
                          'Hai Duong' = 704030,
                          'Hung Yen' = 704030,
                          'Nam Dinh' = 704035,
                          'Ha Nam' = 704035,
                          'Ninh Binh' = 704035,
                          'Thai Binh' = 704034,
                          'Ha Giang' = 704002,
                          'Cao Bang' = 704004,
                          'Lao Cai' = 704010,
                          'Bac Kan' = 704004,
                          'Lang Son' = 704020,
                          'Tuyen Quang' = 704002,
                          'Yen Bai' = 704010,
                          'Thai Nguyen' = 704004,
                          'Phu Tho' = 704001,
                          'Vinh Phuc' = 704001,
                          'Bac Giang' = 704024,
                          'Bac Ninh' = 704024,
                          'Quang Ninh' = 704022,
                          'Lai Chau' = 704010,
                          'Son La' = 704010,
                          'Hoa Binh' = 704001,
                          'Thanh Hoa' = 704038,
                          'Nghe An'= 704040,
                          'Ha Tinh' = 704040,
                          'Quang Binh' = 704044,
                          'Quang Tri' = 704045,
                          'Thuathien-Hue' = 704046,
                          'Da Nang (City)' = 704048,
                          'Quang Nam' = 704048,
                          'Quang Ngai' = 704051,
                          'Binh Dinh' = 704052,
                          'Phu Yen' = 704054,
                          'Khanh Hoa' = 704056,
                          'Kon Tum' = 704062,
                          'Gia Lai' = 704062,
                          'Dak Lak' = 704066,
                          'Ho Chi Minh (City)' = 704079,
                          'Lam Dong' = 704068,
                          'Ninh Thuan' = 704058,
                          'Binh Phuoc' = 704070,
                          'Tay Ninh' = 704072,
                          'Binh Duong' = 704070,
                          'Dong Nai' = 704075,
                          'Binh Thuan' = 704058,
                          'Ba Ria' = 704075,
                          'Long An' = 704080,
                          'Dong Thap' = 704087,
                          'An Giang' = 704089,
                          'Tien Giang' = 704082,
                          'Vinh Long' = 704084,
                          'Ben Tre' = 704083,
                          'Kien Giang' = 704091,
                          'Can Tho' = 704092,
                          'Tra Vinh' = 704084,
                          'Soc Trang' = 704092,
                          'Bac Lieu' = 704095,
                          'Ca Mau' = 704095,
                          .default = NA_real_)) 

bombs_province <- left_join(bombs_province, bombs_provcodes, by = "provincename") %>% distinct()

bombs_prov_geo1_vn <- bombs_province %>% 
  group_by(geo1_vn) %>% 
  summarise(tot_bomb = sum(tot_bomb),
            area_sum = sum(area_sum)) %>% 
  mutate(log_tot_bomb = log(tot_bomb),
         tot_bomb_per = tot_bomb/area_sum,
         log_tot_bmr_per = log(tot_bomb_per))

bombs_prov_geo1_vn1989 <- bombs_province %>% 
  group_by(geo1_) %>% 
  summarise(tot_bomb = sum(tot_bomb),
            area_sum = sum(area_sum)) %>% 
  mutate(log_tot_bomb = log(tot_bomb),
         tot_bomb_per = tot_bomb/area_sum,
         log_tot_bmr_per = log(tot_bomb_per))

phc <- left_join(phc, bombs_prov, by = "geo1_vn")

# Separating by year 

phc89 <- phc %>% 
  filter(year == 1989) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1989, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5,
         age75, log_tot_bomb, tot_bomb_per, log_tot_bmr_per)

phc99 <- phc %>% 
  filter(year == 1999) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn1999, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5,
         age75, log_tot_bomb, tot_bomb_per, log_tot_bmr_per)

phc09 <- phc %>% 
  filter(year == 2009) %>% 
  select(year, serial, hhwt, geo1_vn, geo1_vn2009, regnvn, pernum, perwt, nchild, age, age_cohort, age_cohort75, female, marst, married, widowed,
         birthyr, minority, migration, literate, work, edattain, yrschool, occ, indgen, agri, empsect, ind, geomig1_5,
         age75, log_tot_bomb, tot_bomb_per, log_tot_bmr_per)

# Calculating the sex ratio and LFP of men and women by age cohort in 1989 

prov_89_f <- phc89 %>% 
  filter(female == 1) %>% 
  group_by(geo1_vn) %>% 
  summarise(total_f = sum(perwt),
            log_tot_bomb = mean(log_tot_bomb),
            log_tot_bmr_per = mean(log_tot_bmr_per),
            widowed_f = sum(widowed * perwt) / sum(perwt),
            work_f = sum(work * perwt) / sum(perwt))

prov_89_m <- phc89 %>% 
  filter(female == 0) %>% 
  group_by(geo1_vn) %>% 
  summarise(total_m = sum(perwt))

sexratio_prov_89 <- merge(prov_89_f, prov_89_m, by = "geo1_vn") %>% 
  mutate(sexratio = total_m/total_f)

# Calculating the sex ratio and LFP of men and women by age cohort in 1989, 1999 and 2009

agecohort_sum <- phc %>% 
  group_by(year, age_cohort, female) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort))

agecohort75_sum <- phc %>% 
  group_by(year, age_cohort75, female) %>%
  summarise(work = sum(work * perwt) / sum(perwt),
            widowed = sum(widowed * perwt) / sum(perwt)) %>% 
  filter(!is.na(age_cohort75))

