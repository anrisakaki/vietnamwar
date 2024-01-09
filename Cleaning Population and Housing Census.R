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

bombs_provcodes_geo1_vn <- bombs_province %>%
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

bombs_provcodes89 <- bombs_province %>%
  select(provincename) %>%
  distinct() %>%
  mutate(geo1_vn1989 = recode(provincename,
                          'Ha Noi (City)' = 1,
                          'Hai Phong (City)' = 3,
                          'Ha Tay' = 11,
                          'Hai Duong' = 21,
                          'Hung Yen' = 21,
                          'Nam Dinh' = 23,
                          'Ha Nam' = 23,
                          'Ninh Binh' = 23,
                          'Thai Binh' = 22,
                          'Ha Giang' = 11,
                          'Cao Bang' = 10,
                          'Lao Cai' = 14,
                          'Bac Kan' = 15,
                          'Lang Son' = 12,
                          'Tuyen Quang' = 11,
                          'Yen Bai' = 14,
                          'Thai Nguyen' = 15,
                          'Phu Tho' = 17,
                          'Vinh Phuc' = 17,
                          'Bac Giang' = 18,
                          'Bac Ninh' = 18,
                          'Quang Ninh' = 19,
                          'Lai Chau' = 13,
                          'Son La' = 16,
                          'Hoa Binh' = 20,
                          'Thanh Hoa' = 24,
                          'Nghe An'= 25,
                          'Ha Tinh' = 25,
                          'Quang Binh' = 26,
                          'Quang Tri' = 49,
                          'Thuathien-Hue' = 50,
                          'Da Nang (City)' = 27,
                          'Quang Nam' = 27,
                          'Quang Ngai' = 47,
                          'Binh Dinh' = 28,
                          'Phu Yen' = 48,
                          'Khanh Hoa' = 29,
                          'Kon Tum' = 31,
                          'Gia Lai' = 31,
                          'Dak Lak' = 32,
                          'Ho Chi Minh (City)' = 2,
                          'Lam Dong' = 33,
                          'Ninh Thuan' = 30,
                          'Binh Phuoc' = 34,
                          'Tay Ninh' = 35,
                          'Binh Duong' = 34,
                          'Dong Nai' = 36,
                          'Binh Thuan' = 30,
                          'Ba Ria' = 46,
                          'Long An' = 37,
                          'Dong Thap' = 38,
                          'An Giang' = 39,
                          'Tien Giang' = 40,
                          'Vinh Long' = 42,
                          'Ben Tre' = 41,
                          'Kien Giang' = 44,
                          'Can Tho' = 43,
                          'Tra Vinh' = 42,
                          'Soc Trang' = 43,
                          'Bac Lieu' = 45,
                          'Ca Mau' = 45,
                          .default = NA_real_)) 

bombs_provcodes99 <- bombs_province %>%
  select(provincename) %>%
  distinct() %>%
  mutate(geo1_vn1999 = recode(provincename,
                              'Ha Noi (City)' = 101,
                              'Hai Phong (City)' = 103,
                              'Ha Tay' = 105,
                              'Hai Duong' = 107,
                              'Hung Yen' = 109,
                              'Nam Dinh' = 113,
                              'Ha Nam' = 111,
                              'Ninh Binh' = 117,
                              'Thai Binh' = 115,
                              'Ha Giang' = 201,
                              'Cao Bang' = 203,
                              'Lao Cai' = 205,
                              'Bac Kan' = 207,
                              'Lang Son' = 209,
                              'Tuyen Quang' = 211,
                              'Yen Bai' = 213,
                              'Thai Nguyen' = 215,
                              'Phu Tho' = 217,
                              'Vinh Phuc' = 219,
                              'Bac Giang' = 221,
                              'Bac Ninh' = 223,
                              'Quang Ninh' = 225,
                              'Lai Chau' = 301,
                              'Son La' = 303,
                              'Hoa Binh' = 305,
                              'Thanh Hoa' = 401,
                              'Nghe An'= 403,
                              'Ha Tinh' = 405,
                              'Quang Binh' = 407,
                              'Quang Tri' = 409,
                              'Thuathien-Hue' = 411,
                              'Da Nang (City)' = 501,
                              'Quang Nam' = 507,
                              'Quang Ngai' = 503,
                              'Binh Dinh' = 505,
                              'Phu Yen' = 509,
                              'Khanh Hoa' = 511,
                              'Kon Tum' = 601,
                              'Gia Lai' = 603,
                              'Dak Lak' = 605,
                              'Ho Chi Minh (City)' = 701,
                              'Lam Dong' = 703,
                              'Ninh Thuan' = 705,
                              'Binh Phuoc' = 707,
                              'Tay Ninh' = 709,
                              'Binh Duong' = 711,
                              'Dong Nai' = 713,
                              'Binh Thuan' = 715,
                              'Ba Ria' = 717,
                              'Long An' = 801,
                              'Dong Thap' = 803,
                              'An Giang' = 805,
                              'Tien Giang' = 807,
                              'Vinh Long' = 809,
                              'Ben Tre' = 811,
                              'Kien Giang' = 813,
                              'Can Tho' = 815,
                              'Tra Vinh' = 817,
                              'Soc Trang' = 819,
                              'Bac Lieu' = 821,
                              'Ca Mau' = 823,
                              .default = NA_real_)) 

bombs_provcodes09 <- bombs_province %>%
  select(provincename) %>%
  distinct() %>%
  mutate(geo1_vn2009 = recode(provincename,
                              'Ha Noi (City)' = 1,
                              'Hai Phong (City)' = 31,
                              'Ha Tay' = 1,
                              'Hai Duong' = 30,
                              'Hung Yen' = 33,
                              'Nam Dinh' = 36,
                              'Ha Nam' = 35,
                              'Ninh Binh' = 37,
                              'Thai Binh' = 34,
                              'Ha Giang' = 2,
                              'Cao Bang' = 4,
                              'Lao Cai' = 10,
                              'Bac Kan' = 6,
                              'Lang Son' = 20,
                              'Tuyen Quang' = 8,
                              'Yen Bai' = 15,
                              'Thai Nguyen' = 19,
                              'Phu Tho' = 25,
                              'Vinh Phuc' = 26,
                              'Bac Giang' = 24,
                              'Bac Ninh' = 27,
                              'Quang Ninh' = 22,
                              'Lai Chau' = 12,
                              'Son La' = 14,
                              'Hoa Binh' = 17,
                              'Thanh Hoa' = 38,
                              'Nghe An'= 40,
                              'Ha Tinh' = 42,
                              'Quang Binh' = 44,
                              'Quang Tri' = 45,
                              'Thuathien-Hue' = 46,
                              'Da Nang (City)' = 48,
                              'Quang Nam' = 49,
                              'Quang Ngai' = 51,
                              'Binh Dinh' = 52,
                              'Phu Yen' = 54,
                              'Khanh Hoa' = 56,
                              'Kon Tum' = 62,
                              'Gia Lai' = 64,
                              'Dak Lak' = 66,
                              'Ho Chi Minh (City)' = 79,
                              'Lam Dong' = 68,
                              'Ninh Thuan' = 58,
                              'Binh Phuoc' = 70,
                              'Tay Ninh' = 72,
                              'Binh Duong' = 74,
                              'Dong Nai' = 75,
                              'Binh Thuan' = 60,
                              'Ba Ria' = 77,
                              'Long An' = 80,
                              'Dong Thap' = 87,
                              'An Giang' = 89,
                              'Tien Giang' = 82,
                              'Vinh Long' = 86,
                              'Ben Tre' = 83,
                              'Kien Giang' = 91,
                              'Can Tho' = 92,
                              'Tra Vinh' = 84,
                              'Soc Trang' = 94,
                              'Bac Lieu' = 95,
                              'Ca Mau' = 96,
                              .default = NA_real_)) 

bombs_province <- left_join(bombs_province, bombs_provcodes, by = "provincename") %>%
  distinct() %>% 
  group_by(geo1_vn) %>% 
  summarise(tot_bomb = sum(tot_bomb),
            area_sum = sum(area_sum)) %>% 
  mutate(log_tot_bomb = log(tot_bomb),
         tot_bomb_per = tot_bomb/area_sum,
         log_tot_bmr_per = log(tot_bomb_per))

bombs_province89 <- left_join(bombs_province, bombs_provcodes89, by = "provincename") %>% 
  distinct() %>% 
  group_by(geo1_vn1989) %>% 
  summarise(tot_bomb = sum(tot_bomb),
            area_sum = sum(area_sum)) %>% 
  mutate(log_tot_bomb = log(tot_bomb),
         tot_bomb_per = tot_bomb/area_sum,
         log_tot_bmr_per = log(tot_bomb_per))

bombs_province99 <- left_join(bombs_province, bombs_provcodes99, by = "provincename") %>% 
  distinct() %>% 
  group_by(geo1_vn1999) %>% 
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

