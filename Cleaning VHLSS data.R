
load("province_bmr_sum.Rda")
load("province_bmr_sum2.Rda")

ivid02 <- c("tinh","xa", "hoso", "matv", "tinh02", "huyen02", "xa02", "diaban02", "hoso02", "matv02", "qui", "phieu")
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

vhlss_emp_fn <- function(i){
  i %>% 
    mutate(
      work = ifelse(is.na(work), 0, work),
      selfagri = ifelse(is.na(selfagri), 0, selfagri),
      selfemp = ifelse(is.na(selfemp), 0, selfemp),
      wagework = ifelse(is.na(wagework), 0, wagework),
      work = ifelse(work == 0 & age < 17 | work == 0 & age > 64, NA, work),
      selfagri = ifelse(selfagri == 0 & age < 17 | selfagri == 0 & age > 64, NA, selfagri),
      selfemp = ifelse(selfemp == 0 & age < 17 | selfemp == 0 & age > 64, NA, selfemp),
      wagework = ifelse(wagework == 0 & age < 17 | wagework == 0 & age > 64, NA, wagework),
      self = ifelse(selfagri == 1 | selfemp == 1, 1, 0),
      self = ifelse(self == 0 & age < 17 | self == 0 & age > 64, NA, self))
}

########
# 2002 # 
########

vhlss02 <- list(m1_02, m2_02, m3_02, m5a_02) %>%
  map(~mutate(.x, matv02 = as.numeric(str_sub(as.character(matv02), -2)))) %>%
  reduce(full_join, by = ivid02) %>% 
  left_join(m9_02, by = hhid02) %>% 
  mutate(female = ifelse(m1c2 == 2, 1, 0),
         married = ifelse(m1c6 == 2, 1, 0),
         widowed = ifelse(m1c6 == 3, 1, 0),
         single = ifelse(m1c6 == 1, 1, 0),
         hhhead = ifelse(m1c3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m3c1a == 1, 1, 0),
         work = ifelse(m3c2 == 1, 1, 0),
         selfagri = ifelse(m3c1b == 1, 1, 0),
         selfagri = ifelse(selfagri == 0 & work == 0, NA, selfagri),
         selfemp = ifelse(m3c1c == 1, 1, 0),
         inc = m5ac6 + m5ac7e,
         wartime = ifelse(m1c5 > 42 & m1c5 < 62, 1, 0),
         minority = ifelse(ch_dantoc > 1, 1, 0)) %>% 
  rename(age = m1c5,
         educ = m2c1,
         hours = m3c3,
         days = m3c10,
         industry = m3c7) %>% 
  vhlss_emp_fn () %>% 
  group_by(tinh02, huyen02, xa02, diaban02, hoso02, qui, phieu) %>% 
  mutate(hhid = cur_group_id()) %>% 
  select(tinh02, huyen02, xa02, diaban02, hoso02, matv02, qui, phieu, minority, hhhead, fhead, female, age, married, widowed, single,
         educ, work, wagework, selfemp, selfagri, self, industry, inc, hours, days, hhid) %>% 
  left_join(def02, by = c("tinh02", "huyen02", "xa02", "diaban02", "hoso02","qui")) %>% 
  left_join(wt02, by = c("tinh02", "huyen02", "xa02")) %>% 
  rename(tinh = tinh02) %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days),
         urban = ifelse(urban == 1, 1, 0)) %>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

########
# 2004 # 
########

vhlss04 <- list(m123a_04, m4a_04) %>% 
  reduce(full_join, by = ivid04) %>% 
  left_join(ho1_04, by = hhid04) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         widowed = ifelse(m1ac6 == 3, 1, 0), 
         married = ifelse(m1ac6 == 2, 1, 0),
         single = ifelse(m1ac6 == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0),
         wartime = ifelse(m1ac5 > 44 & m1ac5 < 64, 1, 0),
         minority = ifelse(dantoc > 1, 1, 0)) %>% 
  rename(age = m1ac5,
         educ = m2c1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, ky, hhhead, fhead, minority, female, age, married, widowed, single, wartime, educ, 
         work, formal, wagework, selfemp, selfagri, self, industry, inc, hours, days) %>% 
  left_join(def04, by = c("tinh", "huyen", "xa", "hoso")) %>% 
  left_join(wt04, by = c("tinh", "huyen", "xa")) %>% 
  mutate(inc = inc/mcpi/rcpi) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso, ky) %>% 
  mutate(hhid = cur_group_id(),
         south = ifelse(tinh > 407, 1, 0)) %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days),
         urban = ifelse(urban == 1, 1, 0))%>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))


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
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         widowed = ifelse(m1ac6 == 3, 1, 0),
         single = ifelse(m1ac6 == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         formal = ifelse(m4ac10b == 1, 1, 0),
         wartime = ifelse(m1ac5 > 44 & m1ac5 < 64, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, minority, female, age, married, widowed, single, wartime, educ, 
         work, formal, wagework, selfemp, selfagri, self, industry, inc, hours, days) %>% 
  left_join(def06, by = hhid06) %>% 
  left_join(wt06, by = c("tinh", "huyen", "xa")) %>% 
  mutate(inc = inc/mcpi/rcpi) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 14 & industry < 40, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days),
         urban = ifelse(urban == 1, 1, 0)) %>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

########
# 2008 #
########

vhlss08 <- list(m123a_08, m4a_08) %>% 
  reduce(full_join, by = ivid06) %>%
  left_join(ho_08, by = hhid08) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         widowed = ifelse(m1ac6 == 3, 1, 0),
         single = ifelse(m1ac6 == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         m2ac1 = as.numeric(m2ac1),
         minority = ifelse(dantoc > 1, 1, 0),
         urban = ifelse(ttnt == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac5,
         days = m4ac7,
         hours = m4ac8,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, minority, female, age, married, widowed,single,  educ, 
         work, wagework, selfemp, selfagri, self, industry, inc, hours, days, urban) %>% 
  left_join(wt08, by = c("tinh", "huyen", "xa")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  mutate(tinh = ifelse(tinh == 105, 101, tinh),
         tinh = ifelse(tinh == 303 | tinh == 302, 301, tinh)) %>% 
  left_join(province_bmr_sum, by = "tinh") %>% 
  mutate(south = ifelse(tinh > 407, 1, 0),
         agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days)) %>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

########
# 2010 #
########

vhlss10 <- list(m1a_10, m2a_10, m4a1_10, m4a2_10, m4a3_10, m4a4_10) %>% 
  reduce(full_join, by = ivid06) %>% 
  left_join(ho11_10, by = hhid10) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         widowed = ifelse(m1ac6 == 3, 1, 0),
         single = ifelse(m1ac6 == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         m2ac1 = as.numeric(m2ac1),
         minority = ifelse(dantoc > 1, 1, 0)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, minority, female, age, married, widowed, single, educ, 
         work, wagework, selfemp, selfagri, self, industry, inc, hours, days) %>% 
  left_join(wt10, by = c("tinh", "huyen", "xa")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id(),
         south = ifelse(tinh > 44, 1, 0),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh))  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days),
         urban = ifelse(urban == 1, 1, 0)) %>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

########
# 2012 #
########

vhlss12 <- list(m1a_12, m2a1_12) %>% 
  reduce(full_join, by = ivid06) %>% 
  left_join(ho11_12, by = hhid10) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac6 == 2, 1, 0),
         widowed = ifelse(m1ac6 == 3, 1, 0),
         single = ifelse(m1ac6 == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         m2ac1 = as.numeric(m2ac1),
         minority = ifelse(dantoc > 1, 1, 0),
         urban = ifelse(ttnt == 1, 1, 0)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  vhlss_emp_fn () %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, minority, female, age, married, widowed, single, educ, 
         work, wagework, selfemp, selfagri, self, industry, inc, hours, days, urban) %>% 
  left_join(wt12, by = c("tinh", "huyen", "xa")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id(),
         south = ifelse(tinh > 44, 1, 0),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh))  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  mutate(agri = ifelse(industry < 5, 1, 0),
         agri = ifelse(is.na(agri), 0, agri),
         agri = ifelse(agri == 0 & age < 17 | agri == 0 & age > 64, NA, agri),
         manu = ifelse(industry > 9 & industry < 41, 1, 0),
         manu = ifelse(is.na(manu), 0, manu),
         manu = ifelse(manu == 0 & age < 17 | manu == 0 & age > 64, NA, manu),
         inc = ifelse(is.na(work), NA, inc),
         hours = ifelse(is.na(work), NA, hours),
         days = ifelse(is.na(work), NA, days)) %>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh))

########
# 2014 #
########

mat_pob <- m1a_14 %>% 
  filter(m1ac2 == 2,
         m1ac5 > 63 & m1ac5 < 94) %>% 
  select(tinh, huyen, xa, diaban, hoso, m1ac11, m1ac5) %>% 
  rename(age = m1ac5) %>% 
  mutate(m1ac11 = ifelse(m1ac11 == 28, 1, m1ac11),
         m1ac11 = ifelse(m1ac11 == 14 | m1ac11 == 11, 12, m1ac11)) %>% 
  left_join(province_bmr_sum2, by = c("m1ac11" = "tinh")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(age = max(age),
            tot_bmr_prov_ppn = max(tot_bmr_prov),
            killed_tot_prov_ppn = max(killed_tot_prov_ppn)) %>% 
  rename_with(~ paste0(., "_mat"), -c(tinh, huyen, xa, diaban, hoso))

pat_pob <- m1a_14 %>% 
  filter(m1ac2 == 1,
         m1ac5 > 63 & m1ac5 < 94) %>% 
  select(tinh, huyen, xa, diaban, hoso, m1ac11, m1ac5) %>% 
  rename(age = m1ac5) %>% 
  mutate(m1ac11 = ifelse(m1ac11 == 28, 1, m1ac11),
         m1ac11 = ifelse(m1ac11 == 14 | m1ac11 == 11, 12, m1ac11)) %>% 
  left_join(province_bmr_sum2, by = c("m1ac11" = "tinh")) %>% 
  distinct() %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  summarise(age = max(age),
            tot_bmr_prov_ppn = max(tot_bmr_prov),
            killed_tot_prov_ppn = max(killed_tot_prov_ppn)) %>% 
  rename_with(~ paste0(., "_pat"), -c(tinh, huyen, xa, diaban, hoso))


matpat_pob <- full_join(mat_pob, pat_pob, by = c("tinh", "huyen", "xa", "diaban", "hoso")) %>% distinct()

vhlss14 <- list(m1a_14, m2a_14, m4a_14) %>% 
  reduce(full_join, by = ivid06) %>% 
  mutate(female = ifelse(m1ac2 == 2, 1, 0),
         married = ifelse(m1ac8 == 2, 1, 0),
         widowed = ifelse(m1ac8 == 3, 1, 0),
         single = ifelse(m1ac8 == 1, 1, 0),
         hhhead = ifelse(m1ac3 == 1, 1, 0),
         fhead = ifelse(female == 1 & hhhead == 1, 1, 0),
         child = ifelse(m1ac3 == 3, 1, 0),
         wagework = ifelse(m4ac1a == 1, 1, 0),
         work = ifelse(m4ac2 == 1, 1, 0),
         selfemp = ifelse(m4ac1c == 1, 1, 0),
         selfagri = ifelse(m4ac1b == 1, 1, 0),
         work = ifelse(work == 0 & m1ac5 < 15 | work == 0 & m1ac5 > 64, NA, work),
         tinh = ifelse(tinh == 28, 1, tinh),
         tinh = ifelse(tinh == 14 | tinh == 11, 12, tinh)) %>% 
  rename(age = m1ac5,
         educ = m2ac1,
         industry = m4ac4,
         days = m4ac6,
         hours = m4ac7,
         inc = m4ac11) %>% 
  select(tinh, huyen, xa, diaban, hoso, matv, hhhead, fhead, child, female, age, married, widowed, single, educ, 
         work, wagework, selfemp, selfagri, industry, inc, hours, days) %>% 
  group_by(tinh, huyen, xa, diaban, hoso) %>% 
  mutate(hhid = cur_group_id())  %>% 
  left_join(province_bmr_sum2, by = "tinh") %>% 
  left_join(matpat_pob, by = c("tinh", "huyen", "xa", "diaban", "hoso")) %>% 
  group_by(hhid) %>% 
  mutate(widow_hh = ifelse(any(widowed == 1 & female == 1), 1, 0),
         widow_hh = ifelse(is.na(widow_hh), 0, widow_hh)) %>% 
  mutate(south = ifelse(tinh > 44, 1, 0)) 

save(vhlss02, file = "vhlss02.Rda")
save(vhlss04, file = "vhlss04.Rda")
save(vhlss06, file = "vhlss06.Rda")
save(vhlss08, file = "vhlss08.Rda")
save(vhlss10, file = "vhlss10.Rda")
save(vhlss12, file = "vhlss12.Rda")
save(vhlss14, file = "vhlss14.Rda")
