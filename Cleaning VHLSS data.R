
ivid02 <- c("tinh","xa", "hoso", "matv", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02", "qui", "phieu")
ivid04 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv", "ky")
ivid06 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv")

hhid02 <- c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "qui", "phieu")
hhid04 <- c("tinh", "huyen", "xa", "hoso", "diaban", "ky")
hhid06 <- c("tinh", "huyen", "xa", "hoso")
hhid10 <- c("tinh", "huyen", "xa", "diaban", "hoso")

def02 <- inc_02 %>% select(tinh02, huyen02, xa02, diaban02, hoso02, qui, hhsize, urban, rcpi, mcpi, wt75)
def04 <- inc_04 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)
def06 <- inc_04 %>% select(tinh, huyen, xa, hoso, urban, rcpi, mcpi, wt45)

wt08 <- wt08 %>% select(tinh, huyen, xa, diaban, wt9)
wt10 <- wt10 %>% select(-quyen) %>% rename(urban = ttnt)
wt12 <- wt12 %>% select(tinh, huyen, xa, diaban, wt9)

########
# 2002 # 
########

vhlss02 <- list(m1_02, m2_02, m3_02) %>%
  map(~mutate(.x, matv02 = as.numeric(str_sub(as.character(matv02), -2)))) %>%
  reduce(full_join, by = ivid02) %>% 
  left_join(m5a_02, by = ivid02) %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0),
         married = ifelse(m1c6 == 2, 1, 0),
         hhhead = ifelse(m1c3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m3c1a == 1, 1, 0),
         work = ifelse(m3c2 == 1, 1, 0),
         selfagri = ifelse(m3c1b == 1, 1, 0),
         selfagri = ifelse(selfagri == 0 & work == 0, NA, selfagri),
         selfemp = ifelse(m3c1c == 1, 1, 0),
         inc = m5ac6 + m5ac7e,
         wartime = ifelse(m1c5 > 42 & m1c5 < 62, 1, 0),
         work = ifelse(work == 0 & m1c5 < 15 | work == 0 & m1c5 > 64, NA, work),
         tinh02 = ifelse(tinh02 == 105, 101, tinh02),
         tinh02 = ifelse(tinh02 == 303 | tinh02 == 302, 301, tinh02)) %>% 
  rename(age = m1c5,
         educ = m2c1,
         hours = m3c3,
         days = m3c10,
         industry = m3c7) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, qui, phieu) %>% 
  mutate(hhid = cur_group_id()) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, qui, phieu, hhhead, fhead, female, age, wartime, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days, hhid) %>% 
  left_join(def02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02","qui")) %>% 
  rename(tinh = tinh02) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0))

########
# 2004 # 
########

managers04 <- busid %>% 
  filter(!is.na(hhid04)) %>%
  select(hhid04, manager) %>% 
  rename(matv = manager,
         hh = hhid04) %>% 
  distinct() %>% 
  mutate(manager = 1)

hhbus04 <- m10a_04 %>% 
  filter() %>% 
  mutate(hh_lab = ifelse(m10c12 == 1, 1, 0),
         hh_lab = ifelse(is.na(hh_lab), 0, hh_lab)) %>% 
  mutate_all(~ifelse(. == -1, NA, .)) %>% 
  rename(bus_start = m10c7,
         num_lab = m10c6) %>% 
  select(tinh, huyen, xa, diaban, hoso, m10ama, ky, bus_start, hh_lab, num_lab) %>% 
  full_join(ho1_04, by = c("tinh", "huyen", "xa", "diaban", "hoso", "ky")) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  rename(hhsize = tsnguoi, 
         inc_hh = tthu_4,
         agri_rev = m4b1t/rcpi/mcpi,
         agri_inc = m4b1tn/rcpi/mcpi,
         nonfarm_rev = tthu_11/rcpi/mcpi,
         nonfarm_exp = m4cc/rcpi/mcpi) %>% 
  select(tinh, huyen, xa, diaban, hoso, ky, hhsize, bus_start, hh_lab, inc_hh, agri_rev, 
         agri_inc, nonfarm_rev, nonfarm_exp, urban, wt45) %>%
  distinct() 

vhlss04 <- list(m123a_04, m4a_04) %>% 
  reduce(full_join, by = ivid04) %>% 
  mutate(hh = tinh*10^9+huyen*10^7+xa*10^5+diaban*100+hoso,
         female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0),
         wartime = ifelse(m1ac5 > 44 & m1ac5 < 64, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2c1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, ky, hhhead, fhead, female, age, married, wartime, educ, 
         work, formal, wagework, selfemp, selfagri, industry, inc, hours, days, hh) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  left_join(managers06, by = c("hh", "matv")) %>% 
  mutate(inc = inc/mcpi/rcpi,
         f_manager = ifelse(selfemp == 1 & manager == 1 & female == 1, 1, 0),
         f_manager = ifelse(work == 0 | selfemp == 0 | is.na(manager), NA, f_manager),
         m_manager = ifelse(selfemp == 1 & manager == 1 & female == 0, 1, 0),
         m_manager = ifelse(work == 0 | selfemp == 0 | is.na(manager), NA, m_manager)) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso, ky) %>% 
  mutate(hhid = cur_group_id()) %>% 
  left_join(province_bmr_sum, by = "tinh")

########
# 2006 # 
########

managers06 <- busid %>% 
  filter(!is.na(hhid06)) %>%
  select(hhid06, manager) %>% 
  rename(matv = manager,
         hh = hhid06) %>% 
  distinct() %>% 
  mutate(manager = 1)

vhlss06 <- list(m1a_06, m2a_06, m4a_06) %>% 
  map(~mutate(.x, diaban = as.numeric(diaban))) %>%
  reduce(full_join, by = ivid06) %>% 
  mutate(hh = tinh*10^9 + huyen*10^7 + xa*10^5 + diaban*10^2 + hoso,
         female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0),
         wartime = ifelse(m1ac5 > 44 & m1ac5 < 64, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, wartime, educ, 
         work, formal, wagework, selfemp, selfagri, industry, inc, hours, days, hh) %>% 
  left_join(def06, by = hhid06) %>% 
  left_join(managers06, by = c("hh", "matv")) %>% 
  mutate(inc = inc/mcpi/rcpi,
         f_manager = ifelse(selfemp == 1 & manager == 1 & female == 1, 1, 0),
         f_manager = ifelse(work == 0 | selfemp == 0 | is.na(manager), NA, f_manager),
         m_manager = ifelse(selfemp == 1 & manager == 1 & female == 0, 1, 0),
         m_manager = ifelse(work == 0 | selfemp == 0 | is.na(manager), NA, m_manager)) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum, by = "tinh")

########
# 2008 #
########

managers08 <- busid %>% 
  filter(!is.na(hhid08)) %>%
  select(hhid08, manager) %>% 
  rename(hh = hhid08,
         matv = manager) %>% 
  distinct() %>% 
  mutate(manager = 1)  

vhlss08 <- list(m123a_08, m4a_08) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(hh = tinh*10^10 + huyen*10^8 + xa*10^6 + diaban*10^3 + hoso,
         female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 105, 101, tinh),
         m2ac1 = as.numeric(m2ac1),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days, hh) %>% 
  left_join(wt08, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  left_join(managers08, by = c("hh", "matv")) %>% 
  mutate(f_manager = ifelse(selfemp == 1 & manager == 1 & female == 1, 1, 0),
         f_manager = ifelse(work == 0 | selfemp == 0 | is.na(manager), NA, f_manager),
         m_manager = ifelse(selfemp == 1 & manager == 1 & female == 0, 1, 0),
         m_manager = ifelse(work == 0 | selfemp == 0 | is.na(manager), NA, m_manager)) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum, by = "tinh")

########
# 2010 #
########

vhlss10 <- list(m1a_10, m2a_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 28, 1, tinh),
         m2ac1 = as.numeric(m2ac1)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(wt10, by = hhid10) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum2, by = "tinh")

########
# 2012 #
########

vhlss12 <- list(m1a_12, m2a1_12) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 28, 1, tinh),
         m2ac1 = as.numeric(m2ac1)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  left_join(wt12, by = c("tinh", "huyen", "xa", "diaban")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum2, by = "tinh")

########
# 2014 #
########

vhlss14 <- list(m1a_14, m2a_14, m4a_14) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac8 == 2, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 105, 101, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, female, age, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum2, by = "tinh")

save(vhlss02, file = "vhlss02.Rda")
save(vhlss04, file = "vhlss04.Rda")
save(vhlss06, file = "vhlss06.Rda")
save(vhlss08, file = "vhlss08.Rda")
save(vhlss10, file = "vhlss10.Rda")
save(vhlss12, file = "vhlss12.Rda")
save(vhlss14, file = "vhlss14.Rda")
