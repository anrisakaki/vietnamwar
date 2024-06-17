load("province_bmr_sum02.Rda")
load("province_bmr_sum.Rda")
load("province_bmr_sum2.Rda")

ivid02 <- c("tinh", "xa", "hoso", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02", "qui", "phieu")
ivid04 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv", "ky")
ivid06 <- c("tinh", "huyen", "diaban", "xa", "hoso", "matv")

hhid02 <- c("tinh02", "huyen02", "xa02", "diaban02", "hoso02", "qui", "phieu")
hhid04 <- c("tinh", "huyen", "xa", "hoso", "diaban", "ky")
hhid06 <- c("tinh", "huyen", "xa", "hoso")
hhid08 <- c("tinh", "huyen", "xa", "diaban", "hoso")
hhid10 <- c("tinh", "huyen", "xa", "diaban", "hoso")

def02 <- inc_02 %>% select(tinh02, huyen02, xa02, diaban02, hoso02, qui, hhsize, rcpi, mcpi) %>% distinct()
def04 <- inc_04 %>% select(tinh, huyen, xa, hoso, rcpi, mcpi) %>% distinct()
def06 <- inc_06 %>% select(tinh, huyen, xa, hoso, rcpi, mcpi) %>% distinct()

wt02 <- inc_02 %>% select(tinh02, huyen02, xa02, urban, wt75) %>% distinct()
wt04 <- inc_04 %>% select(tinh, huyen, xa, urban, wt45) %>% distinct()
wt06 <- inc_06 %>% select(tinh, huyen, xa, urban, wt45) %>% distinct()
wt08 <- wt08 %>% select(tinh, huyen, xa, wt9)
wt10 <- wt10 %>% select(-c(quyen, diaban, hoso)) %>% rename(urban = ttnt)
wt12 <- wt12 %>% select(tinh, huyen, xa, wt9)

####################################
# Cleaning population density data #
####################################

ppn02 <- ppn0019 %>% 
  group_by(tinh02) %>% 
  summarise(area = sum(Area),
            pop_02 = sum(X2002)*10000) %>% 
  mutate(popdensity_02 = pop_02/area) %>% 
  rename(tinh = tinh02)

ppn0408 <- ppn0019 %>% 
  group_by(tinh) %>% 
  summarise(area = sum(Area),
            pop_04 = sum(X2004)*10000,
            pop_06 = sum(X2006)*10000,
            pop_08 = sum(X2008)*10000) %>% 
  mutate(popdensity_04 = pop_04/area,
         popdensity_06 = pop_06/area,
         popdensity_08 = pop_08/area)

ppn1012 <- ppn0019 %>% 
  group_by(tinh_08) %>% 
  summarise(area = sum(Area),
            pop_10 = sum(X2010)*10000,
            pop_12 = sum(X2012)*10000,
            pop_14 = sum(X2014)*10000) %>% 
  mutate(popdensity_10 = pop_10/area,
         popdensity_12 = pop_12/area,
         popdensity_14 = pop_14/area) %>% 
  rename(tinh = tinh_08)

vhlss_emp_fn <- function(i){
  i %>% 
    mutate(
      work = ifelse(is.na(work), 0, work),
      work = ifelse(age < 17 | age > 64, NA, work),
      agri = ifelse(industry == 1, 1, 0),
      selfagri = ifelse(is.na(selfagri) & work == 1, 0, selfagri),
      selfemp = ifelse(is.na(selfemp) & work == 1, 0, selfemp),
      self = ifelse(selfagri == 1 | selfemp == 1, 1, 0),
      wagework = ifelse(is.na(wagework) & work == 1, 0, wagework),
      manu = ifelse(is.na(manu) & work == 1, 0, manu),
      manager = ifelse(is.na(manager) & work == 1, 0, manager),
      agri = ifelse(age < 17 | age > 64, NA, agri),
      selfagri = ifelse(age < 17 | age > 64, NA, selfagri),
      selfemp = ifelse(age < 17 | age > 64, NA, selfemp),
      self = ifelse(age < 17 | age > 64, NA, self),
      wagework = ifelse(age < 17 | age > 64, NA, wagework),
      manu = ifelse(age < 17 | age > 64, NA, manu),
      manager = ifelse(age < 17 | age > 64, NA, manager),
      inc = ifelse(is.na(work), NA, inc),
      hours = ifelse(is.na(work), NA, hours),
      days = ifelse(is.na(work), NA, days)) %>% 
    group_by(hhid) %>% 
    mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
    widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))
}

########
# 2002 # 
########

vhlss02 <- list(m1_02, m2_02, m3_02, m5a_02) %>%
  map(~mutate(.x, matv02 = as.numeric(str_sub(as.character(matv02), -2)))) %>%
  reduce(full_join, by = ivid02) %>% 
  left_join(m9_02, by = hhid02) %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, qui, phieu) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup() %>% 
  rename(age = m1c5,
         educ = m2c1,
         hours = m3c3,
         days = m3c10,
         industry = m3c7,
         marital = m1c6,
         occupation = m3c6) %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0),
         married = ifelse(marital == 2, 1, 0),
         widowed = ifelse(marital == 3, 1, 0),
         single = ifelse(marital == 1, 1, 0),
         hhhead = ifelse(m1c3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m3c1a == 1, 1, 0),
         work = ifelse(m3c2 == 1, 1, 0),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         selfagri = ifelse(m3c1b == 1, 1, 0),
         selfemp = ifelse(m3c1c == 1, 1, 0),
         inc = m5ac6 + m5ac7e,
         south = ifelse(tinh02 > 407, 1, 0),
         minority = ifelse(ch_dantoc > 1, 1, 0),
         manager = ifelse(occupation == 11 | occupation == 33, 1, 0),) %>% 
  vhlss_emp_fn () %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, hhid, qui, phieu, minority, hhhead, fhead, female, age, marital, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, agri, manu, industry, occupation, manager, inc, hours, days, widow_hh, south) %>% 
  left_join(def02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02","qui")) %>% 
  left_join(wt02, by = c("tinh02", "huyen02", "xa02")) %>% 
  rename(tinh = tinh02,
         huyen = huyen02) %>% 
  left_join(district_bmr_sum02_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         urban = ifelse(urban == 1, 1, 0),
         year = 2002) %>% 
  left_join(province_bmr_sum02, by = "tinh") %>% 
  left_join(ppn02, by = "tinh")

m5c_02 <- m5c_02 %>% 
  rename(matv02 = m5c1c3t1) %>%
  filter(m5cc1 == 1)

hhbus02 <- list(m1_02, m2_02, m3_02, m5a_02, m5c_02) %>%
  map(~mutate(.x, matv02 = as.numeric(str_sub(as.character(matv02), -2)))) %>%
  reduce(full_join, by = ivid02) %>% 
  filter(m5c1c2t1 == m3c7) %>% 
  rename(age = m1c5,
         educ = m2c1,
         marital = m1c6) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, qui, m1c2, age, educ, marital) %>% 
  left_join(def02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02","qui")) %>% 
  left_join(wt02, by = c("tinh02", "huyen02", "xa02")) %>% 
  rename(tinh = tinh02,
         huyen = huyen02) %>% 
  left_join(district_bmr_sum02_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         urban = ifelse(urban == 1, 1, 0),
         south = ifelse(tinh > 407, 1, 0),
         year = 2002) %>% 
  left_join(province_bmr_sum02, by = "tinh") 

########
# 2004 # 
########

vhlss04 <- list(m123a_04, m4a_04) %>% 
  reduce(full_join, by = ivid04) %>% 
  left_join(ho1_04, by = hhid04) %>% 
  group_by(tinh, huyen, xa, hoso, ky) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup() %>% 
  rename(age = m1ac5,
         educ = m2c1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11,
         marital = m1ac6,
         occupation = m4ac4) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         widowed = ifelse(marital == 3, 1, 0), 
         married = ifelse(marital == 2, 1, 0),
         single = ifelse(marital == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         food = ifelse(industry == 15, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0),
         minority = ifelse(dantoc > 1, 1, 0),
         south = ifelse(tinh > 407, 1, 0),
         manager = ifelse(occupation > 17 & occupation < 21, 1, 0)) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, ky, hhid, minority, hhhead, fhead, female, age, marital, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, agri, manu, food, industry, occupation, manager, inc, hours, days, widow_hh, south) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  left_join(wt04, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum04_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(inc = inc/mcpi/rcpi,
         urban = ifelse(urban == 1, 1, 0),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         year = 2004) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  left_join(ppn0408, by = "tinh")

m4c1_04 <- m4c1_04 %>% 
  rename(industry = m4c1c2)

hhbus04 <- list(m123a_04, m4a_04) %>% 
  reduce(merge, by = ivid04) %>% 
  merge(m4c1_04, by = c("tinh", "huyen", "xa", "diaban", "hoso", "ky")) %>% 
  filter(m4ac5 == industry & m4ac1c == 1) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, ky, m1ac2) %>% 
  distinct() %>% 
  left_join(wt04, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum04_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         south = ifelse(tinh > 407, 1, 0),
         year = 2004) %>% 
  left_join(province_bmr_sum, by = "tinh") 

########
# 2006 # 
########

minority06 <- ttchung_06 %>%
  mutate(minority = ifelse(dantoc > 1, 1, 0)) %>% 
  select(all_of(hhid06), minority)

vhlss06 <- list(m1a_06, m2a_06, m4a_06) %>% 
  map(~mutate(.x, diaban = as.numeric(diaban))) %>%
  reduce(full_join, by = ivid06) %>% 
  left_join(minority06, by = hhid06) %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  ungroup() %>%
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11,
         marital = m1ac6,
         occupation = m4ac4) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(marital == 2, 1, 0),
         widowed = ifelse(marital == 3, 1, 0),
         single = ifelse(marital == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         food = ifelse(industry == 15, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0),
         south = ifelse(tinh > 407, 1, 0),
         manager = ifelse(occupation > 17 & occupation < 21, 1, 0)) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhid, minority, hhhead, fhead, female, age, marital, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, agri, manu, food, industry, occupation, manager, inc, hours, days, widow_hh, south) %>% 
  left_join(def06, by = hhid06) %>% 
  left_join(wt06, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum06_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(inc = inc/mcpi/rcpi,
         urban = ifelse(urban == 1, 1, 0),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         year = 2006) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  left_join(ppn0408, by = "tinh")

m4c_06 <- m4c_06 %>% 
  mutate()
  rename(matv = m4c1c3) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv)

hhbus06 <- list(m1a_06, m4c_06) %>% 
  map(~mutate(.x, diaban = as.numeric(diaban))) %>%
  reduce(merge, by = ivid06) %>% 
  select(tinh, huyen, xa, diaban, hoso, m1ac2) %>% 
  left_join(def06, by = hhid06) %>% 
  left_join(wt06, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum06_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         urban = ifelse(urban == 1, 1, 0),
         south = ifelse(tinh > 407, 1, 0),
         year = 2006) %>% 
  left_join(province_bmr_sum, by = "tinh") 

########
# 2008 #
########

vhlss08 <- list(m123a_08, m4a_08) %>% 
  reduce(full_join, by = ivid06) %>%
  left_join(ho_08, by = hhid08) %>%
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  ungroup() %>%
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11,
         marital = m1ac6,
         occupation = m4ac4) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(marital == 2, 1, 0),
         widowed = ifelse(marital == 3, 1, 0),
         single = ifelse(marital == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         food = ifelse(industry == 15, 1, 0),
         manager = ifelse(occupation > 17 & occupation < 21, 1, 0),
         educ = as.numeric(educ),
         minority = ifelse(dantoc > 1, 1, 0),
         urban = ifelse(ttnt == 1, 1, 0),
         south = ifelse(tinh > 407, 1, 0)) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhid, minority, hhhead, fhead, female, age, marital, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, agri, manu, food, industry, occupation, manager, inc, hours, days, widow_hh, south, urban) %>% 
  left_join(wt08, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum08_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         year = 2008) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  left_join(ppn0408, by = "tinh")

m4c_08 <- m4c_08 %>% 
  rename(matv = m4c1c3)

hhbus08 <- list(m4c_08, m123a_08, m4a_08) %>% 
  reduce(merge, by = ivid06) %>% 
  rename(age = m1ac5,
         educ = m2ac1) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0)) %>% 
  filter(m4c1c2 == m4c1c2) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, female, age, educ) %>% 
  left_join(wt08, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum08_vhlss, by = c("tinh", "huyen")) %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh),
         south = ifelse(tinh > 407, 1, 0),
         year = 2008) %>% 
  left_join(province_bmr_sum, by = "tinh")

########
# 2010 #
########

vhlss10 <- list(m1a_10, m2a_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(full_join, by = ivid06) %>% 
  left_join(ho11_10, by = hhid10) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup() %>%
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11,
         marital = m1ac6,
         occupation = m4ac3) %>% 
  mutate(industry = ifelse(industry == 110, 1, industry),
         educ = as.numeric(educ),
         female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(marital == 2, 1, 0),
         widowed = ifelse(marital == 3, 1, 0),
         single = ifelse(marital == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         food = ifelse(industry == 10 | industry == 11, 1, 0),
         manager = ifelse(occupation > 17 & occupation < 21, 1, 0),
         minority = ifelse(dantoc > 1, 1, 0),
         south = ifelse(tinh > 44, 1, 0)) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhid, minority, hhhead, fhead, female, age, marital, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, agri, manu, food, industry, occupation, manager, inc, hours, days, widow_hh, south) %>% 
  left_join(wt10, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum10, by = c("tinh", "huyen")) %>% 
  distinct() %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         urban = ifelse(urban == 1, 1, 0),
         year = 2010)  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  left_join(ppn1012, by = "tinh")

########
# 2012 #
########

vhlss12 <- list(m1a_12, m2a1_12) %>% 
  reduce(full_join, by = ivid06) %>% 
  left_join(ho11_12, by = hhid10) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id()) %>% 
  ungroup() %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11,
         marital = m1ac6,
         occupation = m4ac3) %>% 
  mutate(industry = ifelse(industry == 110, 1, industry),
         female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(marital == 2, 1, 0),
         widowed = ifelse(marital == 3, 1, 0),
         single = ifelse(marital == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         food = ifelse(industry == 10 | industry == 11, 1, 0),
         educ = as.numeric(educ),
         manager = ifelse(occupation > 17 & occupation < 21, 1, 0),
         minority = ifelse(dantoc > 1, 1, 0),
         urban = ifelse(ttnt == 1, 1, 0),
         south = ifelse(tinh > 44, 1, 0)) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhid, hhhead, minority, fhead, female, age, marital, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, agri, manu, food, industry, occupation, manager, inc, hours, days, widow_hh, south, urban) %>% 
  left_join(wt12, by = c("tinh", "huyen", "xa")) %>% 
  left_join(district_bmr_sum12, by = c("tinh", "huyen")) %>% 
  distinct() %>% 
  mutate(tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh),
         year = 2012)  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  left_join(ppn1012, by = "tinh") 

vhlss <- bind_rows(vhlss02, vhlss04, vhlss06, vhlss08, vhlss10, vhlss12)

save(vhlss02, file = "vhlss02.Rda")
save(vhlss04, file = "vhlss04.Rda")
save(vhlss06, file = "vhlss06.Rda")
save(vhlss08, file = "vhlss08.Rda")
save(vhlss10, file = "vhlss10.Rda")
save(vhlss12, file = "vhlss12.Rda")